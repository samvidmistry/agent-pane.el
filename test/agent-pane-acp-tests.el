;;; agent-pane-acp-tests.el --- ACP integration tests (fake) -*- lexical-binding: t; -*-

(require 'ert)
(require 'map)
(require 'agent-pane)

(require 'acp-traffic)
(require 'acp-fakes)

;;; Code:

(defun agent-pane-test--traffic (name)
  (let* ((root (file-name-directory (directory-file-name (file-name-directory load-file-name))))
         (path (expand-file-name (concat "traffic/" name) (expand-file-name "test" root))))
    (acp-traffic-read-file path)))

(defun agent-pane-test--make-fake-client (traffic)
  ;; `acp-fakes-make-client' returns an alist-based client.  Mark it
  ;; as fake without relying on in-place map mutation.
  (cons (cons :agent-pane-fake t)
        (acp-fakes-make-client traffic)))

(ert-deftest agent-pane-acp-handshake-creates-session ()
  (with-temp-buffer
    (agent-pane-mode)
    (let* ((traffic (agent-pane-test--traffic "agent-pane-minimal.traffic"))
           (agent-pane-acp-client-maker (lambda (_buffer) (agent-pane-test--make-fake-client traffic))))
      (agent-pane--handshake (lambda () nil))
      (let ((sid (map-elt agent-pane--state :session-id)))
        (unless (equal sid "fake-session-1")
          (ert-fail (format "Expected session id, got %S. fake=%S client=%S messages=%S"
                            sid
                            (and (map-elt agent-pane--state :client)
                                 (map-elt (map-elt agent-pane--state :client) :agent-pane-fake))
                            (map-elt agent-pane--state :client)
                            (map-elt agent-pane--state :messages))))))))

(ert-deftest agent-pane-auth-failure-help-is-actionable ()
  (let ((agent-pane-acp-provider 'codex)
        (s (agent-pane--auth-failure-help '((message . "not authenticated")))))
    (should (string-match-p "not authenticated" s))
    (should (string-match-p "codex login" s))
    (should (string-match-p "OPENAI_API_KEY" s))))

(ert-deftest agent-pane-handshake-skips-authenticate-when-auth-method-is-nil ()
  (with-temp-buffer
    (agent-pane-mode)
    (let ((methods nil)
          (done nil)
          (agent-pane-auth-method-id nil)
          (agent-pane-acp-client-maker
           (lambda (_buffer)
             (agent-pane-test--make-fake-client nil))))
      (cl-letf (((symbol-function 'agent-pane--acp-send-request)
                 (lambda (&rest args)
                   (let* ((request (plist-get args :request))
                          (on-success (plist-get args :on-success))
                          (method (or (map-elt request :method)
                                      (map-elt request 'method))))
                     (push method methods)
                     (pcase method
                       ("initialize" (funcall on-success '((protocolVersion . 1))))
                       ("session/new" (funcall on-success '((sessionId . "fake-session-no-auth"))))
                       ("session/set_mode" (funcall on-success '((ok . t))))
                       (_ (funcall on-success '((ok . t)))))))))
        (agent-pane--handshake (lambda () (setq done t)))
        (should done)
        (should (equal (map-elt agent-pane--state :session-id) "fake-session-no-auth"))
        (should-not (member "authenticate" methods))))))

(ert-deftest agent-pane-handshake-applies-session-model-when-configured ()
  (with-temp-buffer
    (agent-pane-mode)
    (let ((methods nil)
          (done nil)
          (agent-pane-auth-method-id nil)
          (agent-pane-session-mode-id "bypassPermissions")
          (agent-pane-session-model-id "gpt-5")
          (agent-pane-acp-client-maker
           (lambda (_buffer)
             (agent-pane-test--make-fake-client nil))))
      (cl-letf (((symbol-function 'agent-pane--acp-send-request)
                 (lambda (&rest args)
                   (let* ((request (plist-get args :request))
                          (on-success (plist-get args :on-success))
                          (method (or (map-elt request :method)
                                      (map-elt request 'method))))
                     (push method methods)
                     (pcase method
                       ("initialize" (funcall on-success '((protocolVersion . 1))))
                       ("session/new" (funcall on-success '((sessionId . "fake-session-model"))))
                       ("session/set_mode" (funcall on-success '((ok . t))))
                       ("session/set_model" (funcall on-success '((ok . t))))
                       (_ (funcall on-success '((ok . t)))))))))
        (agent-pane--handshake (lambda () (setq done t)))
        (should done)
        (should (equal (nreverse methods)
                       '("initialize" "session/new" "session/set_mode" "session/set_model")))
        (should (equal (map-elt agent-pane--state :session-id) "fake-session-model"))
        (should (equal (map-elt (map-elt agent-pane--state :session-model-result) 'ok) t))))))

(ert-deftest agent-pane-handshake-resumes-session-when-server-supports-load-session ()
  (with-temp-buffer
    (agent-pane-mode)
    (let ((done nil)
          (agent-pane-auth-method-id nil)
          (agent-pane-acp-client-maker
           (lambda (_buffer)
             (agent-pane-test--make-fake-client nil)))
          resume-session-id)
      (map-put! agent-pane--state :resume-session-id "resume-123")
      (cl-letf (((symbol-function 'agent-pane--acp-send-request)
                 (lambda (&rest args)
                   (let* ((request (plist-get args :request))
                          (on-success (plist-get args :on-success))
                          (method (or (map-elt request :method)
                                      (map-elt request 'method))))
                     (pcase method
                       ("initialize"
                        (funcall on-success '((protocolVersion . 1)
                                              (agentCapabilities . ((loadSession . t))))))
                       ("session/new"
                        (setq resume-session-id
                              (map-nested-elt request '(:params sessionId)))
                        (funcall on-success '((sessionId . "resume-123"))))
                       (_ (funcall on-success '((ok . t)))))))))
        (agent-pane--handshake (lambda () (setq done t)))
        (should done)
        (should (equal resume-session-id "resume-123"))
        (should (equal (map-elt agent-pane--state :session-id) "resume-123"))))))

(ert-deftest agent-pane-handshake-falls-back-when-resume-fails ()
  (with-temp-buffer
    (agent-pane-mode)
    (let ((done nil)
          (agent-pane-auth-method-id nil)
          (agent-pane-acp-client-maker
           (lambda (_buffer)
             (agent-pane-test--make-fake-client nil)))
          (session-new-requests nil))
      (map-put! agent-pane--state :resume-session-id "resume-456")
      (cl-letf (((symbol-function 'agent-pane--acp-send-request)
                 (lambda (&rest args)
                   (let* ((request (plist-get args :request))
                          (on-success (plist-get args :on-success))
                          (on-failure (plist-get args :on-failure))
                          (method (or (map-elt request :method)
                                      (map-elt request 'method))))
                     (pcase method
                       ("initialize"
                        (funcall on-success '((protocolVersion . 1)
                                              (agentCapabilities . ((loadSession . t))))))
                       ("session/new"
                        (push (map-nested-elt request '(:params sessionId)) session-new-requests)
                        (if (map-nested-elt request '(:params sessionId))
                            (funcall on-failure '((message . "not found")))
                          (funcall on-success '((sessionId . "fresh-1"))))
                        )
                       (_ (funcall on-success '((ok . t)))))))))
        (agent-pane--handshake (lambda () (setq done t)))
        (should done)
        (should (equal (nreverse session-new-requests)
                       '("resume-456" nil)))
        (should (equal (map-elt agent-pane--state :session-id) "fresh-1"))))))

(ert-deftest agent-pane-acp-streaming-appends-to-assistant ()
  (with-temp-buffer
    (agent-pane-mode)
    (let* ((traffic (agent-pane-test--traffic "agent-pane-minimal.traffic"))
           (agent-pane-acp-client-maker (lambda (_buffer) (agent-pane-test--make-fake-client traffic)))
           (cancelled-permission nil))
      (cl-letf (((symbol-function 'agent-pane--acp-send-response)
                 (lambda (&rest args)
                   (setq cancelled-permission (plist-get args :response)))))
        (agent-pane--handshake (lambda ()
                                 (agent-pane--send-prompt "hi")))
        (let ((sid (map-elt agent-pane--state :session-id)))
          (unless (equal sid "fake-session-1")
            (ert-fail (format "Expected session id, got %S. fake=%S client=%S messages=%S"
                              sid
                              (and (map-elt agent-pane--state :client)
                                   (map-elt (map-elt agent-pane--state :client) :agent-pane-fake))
                              (map-elt agent-pane--state :client)
                              (map-elt agent-pane--state :messages)))))
        (should (consp (map-elt (map-elt agent-pane--state :client) :request-handlers)))
        (let* ((msgs (map-elt agent-pane--state :messages))
               (assistant-text
                (cl-loop for m in msgs
                         when (eq (map-elt m :role) 'assistant)
                         concat (or (map-elt m :text) ""))))
          (should (string-match-p "Hello, world\\." assistant-text)))
        (should (null cancelled-permission))))))

(ert-deftest agent-pane-permission-request-auto-allowed ()
  (with-temp-buffer
    (agent-pane-mode)
    (let ((sent nil))
      (cl-letf (((symbol-function 'agent-pane--acp-send-response)
                 (lambda (&rest args)
                   (setq sent (plist-get args :response)))))
        (map-put! agent-pane--state :client (list (cons :agent-pane-fake t)))
        (let ((agent-pane-permission-policy 'auto-allow))
          (agent-pane--on-request
           '((jsonrpc . "2.0")
             (id . 99)
             (method . "session/request_permission")
             (params
              . ((options
                  . [((optionId . "proceed_always") (name . "Always Allow") (kind . "allow_always"))
                     ((optionId . "proceed_once") (name . "Allow") (kind . "allow_once"))
                     ((optionId . "cancel") (name . "Reject") (kind . "reject_once"))])
                 (toolCall . ((toolCallId . "tc_1")
                              (title . "bash")
                              (status . "pending")
                              (kind . "tool"))))))))
        (should (equal (map-elt sent :request-id) 99))
        (should (equal (map-nested-elt sent '(:result outcome outcome)) "selected"))
        (should (equal (map-nested-elt sent '(:result outcome optionId)) "proceed_always"))))))

(ert-deftest agent-pane-permission-request-prompt-noninteractive-cancels ()
  (with-temp-buffer
    (agent-pane-mode)
    (let ((sent nil))
      (cl-letf (((symbol-function 'agent-pane--acp-send-response)
                 (lambda (&rest args)
                   (setq sent (plist-get args :response)))))
        (map-put! agent-pane--state :client (list (cons :agent-pane-fake t)))
        (let ((agent-pane-permission-policy 'prompt)
              (noninteractive t)
              (agent-pane--permission-project-rules-cache :uninitialized))
          (agent-pane--on-request
           '((jsonrpc . "2.0")
             (id . 101)
             (method . "session/request_permission")
             (params
              . ((options
                  . [((optionId . "proceed_once") (name . "Allow") (kind . "allow_once"))
                     ((optionId . "cancel") (name . "Reject") (kind . "reject_once"))])
                 (toolCall . ((toolCallId . "tc_1")
                              (title . "bash")
                              (status . "pending")
                              (kind . "tool"))))))))
        (should (equal (map-elt sent :request-id) 101))
        (should (equal (map-nested-elt sent '(:result outcome outcome)) "cancelled"))))))

(ert-deftest agent-pane-permission-request-prompt-uses-session-rule ()
  (with-temp-buffer
    (agent-pane-mode)
    (let ((sent nil)
          (session-rules (map-elt agent-pane--state :permission-session-rules)))
      (puthash "kind:tool" "proceed_once" session-rules)
      (cl-letf (((symbol-function 'agent-pane--acp-send-response)
                 (lambda (&rest args)
                   (setq sent (plist-get args :response)))))
        (map-put! agent-pane--state :client (list (cons :agent-pane-fake t)))
        (let ((agent-pane-permission-policy 'prompt)
              (agent-pane--permission-project-rules-cache :uninitialized))
          (agent-pane--on-request
           '((jsonrpc . "2.0")
             (id . 102)
             (method . "session/request_permission")
             (params
              . ((options
                  . [((optionId . "proceed_once") (name . "Allow") (kind . "allow_once"))
                     ((optionId . "cancel") (name . "Reject") (kind . "reject_once"))])
                 (toolCall . ((toolCallId . "tc_1")
                              (title . "bash")
                              (status . "pending")
                              (kind . "tool"))))))))
        (should (equal (map-elt sent :request-id) 102))
        (should (equal (map-nested-elt sent '(:result outcome outcome)) "selected"))
        (should (equal (map-nested-elt sent '(:result outcome optionId)) "proceed_once"))))))

(ert-deftest agent-pane-permission-request-prompt-reviews-diff-before-choice ()
  (with-temp-buffer
    (agent-pane-mode)
    (let ((sent nil)
          (shown-diff nil)
          (answers '("Review diff in Emacs"
                     "Choose Allow (allow_once, optionId=proceed_once)")))
      (cl-letf (((symbol-function 'agent-pane--acp-send-response)
                 (lambda (&rest args)
                   (setq sent (plist-get args :response))))
                ((symbol-function 'completing-read)
                 (lambda (&rest _args)
                   (prog1 (car answers)
                     (setq answers (cdr answers)))))
                ((symbol-function 'agent-pane--show-diff)
                 (lambda (diff &optional _title)
                   (setq shown-diff diff))))
        (map-put! agent-pane--state :client (list (cons :agent-pane-fake t)))
        (let ((agent-pane-permission-policy 'prompt)
              (agent-pane-permission-review-diff t)
              (noninteractive nil)
              (agent-pane--permission-project-rules-cache :uninitialized))
          (agent-pane--on-request
           '((jsonrpc . "2.0")
             (id . 106)
             (method . "session/request_permission")
             (params
              . ((options
                  . [((optionId . "proceed_once") (name . "Allow") (kind . "allow_once"))
                     ((optionId . "cancel") (name . "Reject") (kind . "reject_once"))])
                 (toolCall . ((toolCallId . "tc_1")
                              (title . "bash")
                              (status . "pending")
                              (kind . "tool")
                              (content . ((type . "diff")
                                          (path . "src/main.el")
                                          (oldText . "a\n")
                                          (newText . "a\nb\n"))))))))))
        (should (equal (map-elt sent :request-id) 106))
        (should (equal (map-nested-elt sent '(:result outcome optionId)) "proceed_once"))
        (should (equal (plist-get shown-diff :file) "src/main.el"))))))

(ert-deftest agent-pane-permission-request-prompt-can-store-session-rule ()
  (with-temp-buffer
    (agent-pane-mode)
    (let ((sent nil)
          (session-rules (map-elt agent-pane--state :permission-session-rules)))
      (cl-letf (((symbol-function 'agent-pane--acp-send-response)
                 (lambda (&rest args)
                   (setq sent (plist-get args :response))))
                ((symbol-function 'completing-read)
                 (lambda (&rest _args)
                   "Always allow this tool in current session")))
        (map-put! agent-pane--state :client (list (cons :agent-pane-fake t)))
        (let ((agent-pane-permission-policy 'prompt)
              (noninteractive nil)
              (agent-pane--permission-project-rules-cache :uninitialized))
          (agent-pane--on-request
           '((jsonrpc . "2.0")
             (id . 103)
             (method . "session/request_permission")
             (params
              . ((options
                  . [((optionId . "proceed_always") (name . "Always Allow") (kind . "allow_always"))
                     ((optionId . "cancel") (name . "Reject") (kind . "reject_once"))])
                 (toolCall . ((toolCallId . "tc_1")
                              (title . "bash")
                              (status . "pending")
                              (kind . "tool"))))))))
        (should (equal (map-elt sent :request-id) 103))
        (should (equal (map-nested-elt sent '(:result outcome optionId)) "proceed_always"))
        (should (equal (gethash "kind:tool" session-rules) "proceed_always"))))))

(ert-deftest agent-pane-permission-request-prompt-can-store-project-rule ()
  (with-temp-buffer
    (agent-pane-mode)
    (let* ((sent nil)
           (rules-file (make-temp-file "agent-pane-permissions" nil ".el"))
           (agent-pane-permission-rules-file rules-file)
           (agent-pane--permission-project-rules-cache :uninitialized)
           (request-104
            '((jsonrpc . "2.0")
              (id . 104)
              (method . "session/request_permission")
              (params
               . ((options
                   . [((optionId . "proceed_always") (name . "Always Allow") (kind . "allow_always"))
                      ((optionId . "cancel") (name . "Reject") (kind . "reject_once"))])
                  (toolCall . ((toolCallId . "tc_1")
                               (title . "bash")
                               (status . "pending")
                               (kind . "tool")))))))
           (request-105
            '((jsonrpc . "2.0")
              (id . 105)
              (method . "session/request_permission")
              (params
               . ((options
                   . [((optionId . "proceed_always") (name . "Always Allow") (kind . "allow_always"))
                      ((optionId . "cancel") (name . "Reject") (kind . "reject_once"))])
                  (toolCall . ((toolCallId . "tc_2")
                               (title . "bash")
                               (status . "pending")
                               (kind . "tool"))))))))
      (unwind-protect
          (progn
            (map-put! agent-pane--state :client (list (cons :agent-pane-fake t)))

            (cl-letf (((symbol-function 'agent-pane--acp-send-response)
                       (lambda (&rest args)
                         (setq sent (plist-get args :response))))
                      ((symbol-function 'completing-read)
                       (lambda (&rest _args)
                         "Always allow this tool in this project")))
              (let ((agent-pane-permission-policy 'prompt)
                    (noninteractive nil))
                (agent-pane--on-request request-104))
              (should (equal (map-elt sent :request-id) 104))
              (should (equal (map-nested-elt sent '(:result outcome optionId)) "proceed_always"))
              (should (file-exists-p rules-file)))

            ;; Next request should auto-apply project rule even in noninteractive mode.
            (setq sent nil)
            (cl-letf (((symbol-function 'agent-pane--acp-send-response)
                       (lambda (&rest args)
                         (setq sent (plist-get args :response)))))
              (let ((agent-pane-permission-policy 'prompt)
                    (noninteractive t))
                (agent-pane--on-request request-105))
              (should (equal (map-elt sent :request-id) 105))
              (should (equal (map-nested-elt sent '(:result outcome outcome)) "selected"))
              (should (equal (map-nested-elt sent '(:result outcome optionId)) "proceed_always"))))
        (ignore-errors (delete-file rules-file))))))

(ert-deftest agent-pane-acp-interleaves-thought-tool-and-messages ()
  (with-temp-buffer
    (agent-pane-mode)
    (let* ((traffic (agent-pane-test--traffic "agent-pane-interleaved.traffic"))
           (agent-pane-acp-client-maker (lambda (_buffer)
                                         (agent-pane-test--make-fake-client traffic))))
      ;; Pretend we're waiting so we can verify we flip to streaming.
      (map-put! agent-pane--state :in-progress 'waiting)
      (agent-pane--handshake (lambda ()
                               (agent-pane--send-prompt "hi")))
      (should (null (map-elt agent-pane--state :in-progress)))
      (let* ((msgs (map-elt agent-pane--state :messages))
             (roles (mapcar (lambda (m) (map-elt m :role)) msgs)))
        (should (equal roles '(thought assistant tool assistant assistant)))
        (should (string-match-p "Thinking" (map-elt (nth 0 msgs) :text)))
        (should (string-match-p "Hello" (map-elt (nth 1 msgs) :text)))
        (should (string-match-p "after tool" (map-elt (nth 3 msgs) :text)))
        (should (string-match-p "Done\\." (map-elt (nth 4 msgs) :text)))
        ;; Tool message got output.
        (should (string-match-p "output" (map-elt (nth 2 msgs) :text)))
        (should (string-match-p "ok" (map-elt (nth 2 msgs) :text)))
        ;; Tool label should prefer command/title over opaque ids.
        (should (string-match-p "echo hi" (or (map-elt (nth 2 msgs) :title) "")))))))

(ert-deftest agent-pane-acp-writes-transcript-file ()
  (let ((agent-pane-save-transcripts 'always)
        (dir (make-temp-file "agent-pane-transcripts" t)))
    (unwind-protect
        (let ((agent-pane-transcript-directory dir))
          (with-temp-buffer
            (agent-pane-mode)
            (let* ((traffic (agent-pane-test--traffic "agent-pane-minimal.traffic"))
                   (agent-pane-acp-client-maker
                    (lambda (_buffer) (agent-pane-test--make-fake-client traffic))))
              (agent-pane--handshake (lambda ()
                                       (agent-pane--send-prompt "hi")))
              (let ((file (map-elt agent-pane--state :transcript-file)))
                (should (stringp file))
                (should (file-exists-p file))
                (with-temp-buffer
                  (insert-file-contents file)
                  (should (string-match-p "## Agent" (buffer-string)))
                  (should (string-match-p "Hello, world\\\." (buffer-string))))))))
      (ignore-errors (delete-directory dir t)))))

(provide 'agent-pane-acp-tests)
;;; agent-pane-acp-tests.el ends here

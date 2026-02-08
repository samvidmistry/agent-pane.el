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

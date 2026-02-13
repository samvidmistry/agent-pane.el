;;; agent-pane-notify-tests.el --- Tests for lifecycle hooks and notifier -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'map)
(require 'agent-pane)
(require 'agent-pane-notify)

(require 'acp-fakes)

;;; Code:

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defun agent-pane-notify-test--make-fake-client ()
  "Create a fake ACP client suitable for hook tests."
  (cons (cons :agent-pane-fake t)
        (acp-fakes-make-client nil)))
;; ---------------------------------------------------------------------------
;; turn-end hook
;; ---------------------------------------------------------------------------

(ert-deftest agent-pane-turn-end-hook-fires-on-stop-reason ()
  "The turn-end hook fires when the prompt response has a stopReason."
  (with-temp-buffer
    (agent-pane-mode)
    (let ((fired nil)
          (agent-pane-auth-method-id nil)
          (agent-pane-acp-client-maker
           (lambda (_buffer) (agent-pane-notify-test--make-fake-client))))
      (add-hook 'agent-pane-turn-end-hook (lambda () (setq fired t)) nil t)
      (cl-letf (((symbol-function 'agent-pane--acp-send-request)
                 (lambda (&rest args)
                   (let* ((request (plist-get args :request))
                          (on-success (plist-get args :on-success))
                          (method (or (map-elt request :method)
                                      (map-elt request 'method))))
                     (pcase method
                       ("initialize"
                        (funcall on-success '((protocolVersion . 1))))
                       ("session/new"
                        (funcall on-success '((sessionId . "hook-s1"))))
                       ("session/set_mode"
                        (funcall on-success '((ok . t))))
                       ("session/prompt"
                        (funcall on-success '((stopReason . "end_turn"))))
                       (_
                        (funcall on-success '((ok . t)))))))))
        (agent-pane--handshake
         (lambda ()
           (map-put! agent-pane--state :prompt-in-flight t)
           (map-put! agent-pane--state :in-progress 'waiting)
           (agent-pane--send-prompt "hello"))))
      (should fired))))

;; ---------------------------------------------------------------------------
;; turn-start hook
;; ---------------------------------------------------------------------------

(ert-deftest agent-pane-turn-start-hook-fires-on-prompt-dispatch ()
  "The turn-start hook fires when a prompt is dispatched."
  (with-temp-buffer
    (agent-pane-mode)
    (let ((fired nil)
          (agent-pane-auth-method-id nil)
          (agent-pane-enable-acp t)
          (noninteractive nil)
          (agent-pane-acp-client-maker
           (lambda (_buffer) (agent-pane-notify-test--make-fake-client))))
      (add-hook 'agent-pane-turn-start-hook (lambda () (setq fired t)) nil t)
      (cl-letf (((symbol-function 'agent-pane--acp-send-request)
                 (lambda (&rest args)
                   (let* ((request (plist-get args :request))
                          (on-success (plist-get args :on-success))
                          (method (or (map-elt request :method)
                                      (map-elt request 'method))))
                     (pcase method
                       ("initialize"
                        (funcall on-success '((protocolVersion . 1))))
                       ("session/new"
                        (funcall on-success '((sessionId . "hook-s2"))))
                       ("session/set_mode"
                        (funcall on-success '((ok . t))))
                       ("session/prompt"
                        (funcall on-success '((stopReason . "end_turn"))))
                       (_
                        (funcall on-success '((ok . t)))))))))
        ;; Establish session then queue a prompt.
        (agent-pane--handshake #'ignore)
        (map-put! agent-pane--state :prompt-queue '("test prompt"))
        (map-put! agent-pane--state :prompt-queue-message-indices '(nil))
        (agent-pane--pump-prompt-queue))
      (should fired))))

;; ---------------------------------------------------------------------------
;; error hook
;; ---------------------------------------------------------------------------

(ert-deftest agent-pane-error-hook-fires-on-fail-current-assistant ()
  "The error hook fires with error text when `agent-pane--fail-current-assistant' runs."
  (with-temp-buffer
    (agent-pane-mode)
    (let ((captured nil))
      (add-hook 'agent-pane-error-hook
                (lambda (text) (setq captured text)) nil t)
      (agent-pane--fail-current-assistant "something broke: %s" "disk full")
      (should (stringp captured))
      (should (string-match-p "something broke" captured))
      (should (string-match-p "disk full" captured)))))

;; ---------------------------------------------------------------------------
;; cancel hook
;; ---------------------------------------------------------------------------

(ert-deftest agent-pane-cancel-hook-fires-on-cancel ()
  "The cancel hook fires after `agent-pane-cancel'."
  (with-temp-buffer
    (agent-pane-mode)
    (let ((fired nil)
          (agent-pane-enable-acp t)
          (noninteractive nil))
      (add-hook 'agent-pane-cancel-hook (lambda () (setq fired t)) nil t)
      (map-put! agent-pane--state :client 'dummy-client)
      (map-put! agent-pane--state :session-id "hook-cancel-session")
      (map-put! agent-pane--state :in-progress 'streaming)
      (cl-letf (((symbol-function 'agent-pane--acp-send-notification)
                 (lambda (&rest _) nil)))
        (agent-pane-cancel))
      (should fired))))

;; ---------------------------------------------------------------------------
;; permission-request hook
;; ---------------------------------------------------------------------------

(ert-deftest agent-pane-permission-request-hook-fires ()
  "The permission-request hook fires with params on permission request."
  (with-temp-buffer
    (agent-pane-mode)
    (let ((captured nil))
      (add-hook 'agent-pane-permission-request-hook
                (lambda (params) (setq captured params)) nil t)
      (cl-letf (((symbol-function 'agent-pane--acp-send-response)
                 (lambda (&rest _) nil)))
        (map-put! agent-pane--state :client (list (cons :agent-pane-fake t)))
        (let ((agent-pane-permission-policy 'auto-allow))
          (agent-pane--on-request
           '((jsonrpc . "2.0")
             (id . 42)
             (method . "session/request_permission")
             (params
              . ((options
                  . [((optionId . "proceed_once") (name . "Allow") (kind . "allow_once"))
                     ((optionId . "cancel") (name . "Reject") (kind . "reject_once"))])
                 (toolCall . ((toolCallId . "tc_hook")
                              (title . "bash")
                              (status . "pending")
                              (kind . "tool")))))))))
      (should captured)
      (should (map-elt captured 'toolCall)))))

;; ---------------------------------------------------------------------------
;; Built-in notifier
;; ---------------------------------------------------------------------------

(ert-deftest agent-pane-notify-on-turn-end-calls-message ()
  "The built-in notifier calls `message' on turn end."
  (let ((messages nil)
        (agent-pane-notify-style 'message))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages))))
      (agent-pane-notify-on-turn-end))
    (should (= 1 (length messages)))
    (should (string-match-p "turn complete" (car messages)))))

(ert-deftest agent-pane-notify-silent-when-style-nil ()
  "The built-in notifier does nothing when style is nil."
  (let ((messages nil)
        (agent-pane-notify-style nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages))))
      (agent-pane-notify-on-turn-end))
    (should (null messages))))

;; ---------------------------------------------------------------------------
;; No spurious hook fire during handshake
;; ---------------------------------------------------------------------------

(ert-deftest agent-pane-turn-end-hook-not-fired-during-handshake ()
  "Handshake session updates must not trigger the turn-end hook."
  (with-temp-buffer
    (agent-pane-mode)
    (let ((fired nil)
          (agent-pane-auth-method-id nil)
          (agent-pane-acp-client-maker
           (lambda (_buffer) (agent-pane-notify-test--make-fake-client))))
      (add-hook 'agent-pane-turn-end-hook (lambda () (setq fired t)) nil t)
      (cl-letf (((symbol-function 'agent-pane--acp-send-request)
                 (lambda (&rest args)
                   (let* ((request (plist-get args :request))
                          (on-success (plist-get args :on-success))
                          (method (or (map-elt request :method)
                                      (map-elt request 'method))))
                     (pcase method
                       ("initialize"
                        (funcall on-success '((protocolVersion . 1))))
                       ("session/new"
                        (funcall on-success '((sessionId . "hook-no-fire"))))
                       ("session/set_mode"
                        (funcall on-success '((ok . t))))
                       (_
                        (funcall on-success '((ok . t)))))))))
        (agent-pane--handshake #'ignore))
      (should-not fired))))

(provide 'agent-pane-notify-tests)
;;; agent-pane-notify-tests.el ends here

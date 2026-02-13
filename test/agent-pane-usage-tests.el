;;; agent-pane-usage-tests.el --- Usage metrics tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'map)
(require 'agent-pane)

;;; Code:

;; ---------------------------------------------------------------------------
;; agent-pane--extract-usage-metrics
;; ---------------------------------------------------------------------------

(ert-deftest agent-pane-extract-usage--standard-acp-usage-update ()
  "Standard ACP usage_update with top-level `used' and `size'."
  (let* ((payload '((sessionUpdate . "usage_update")
                    (used . 53000)
                    (size . 200000)))
         (result (agent-pane--extract-usage-metrics payload)))
    (should (equal (plist-get result :context-used) 53000))
    (should (equal (plist-get result :context-max) 200000))))

(ert-deftest agent-pane-extract-usage--standard-acp-usage-update-with-cost ()
  "Standard ACP usage_update with nested cost object."
  (let* ((payload '((sessionUpdate . "usage_update")
                    (used . 53000)
                    (size . 200000)
                    (cost . ((amount . 0.045)
                             (currency . "USD")))))
         (result (agent-pane--extract-usage-metrics payload)))
    (should (equal (plist-get result :context-used) 53000))
    (should (equal (plist-get result :context-max) 200000))
    (should (= (plist-get result :cost) 0.045))
    (should (equal (plist-get result :cost-currency) "USD"))))

(ert-deftest agent-pane-extract-usage--standard-acp-usage-update-non-usd ()
  "Standard ACP usage_update with non-USD currency."
  (let* ((payload '((sessionUpdate . "usage_update")
                    (used . 10000)
                    (size . 128000)
                    (cost . ((amount . 3.5)
                             (currency . "EUR")))))
         (result (agent-pane--extract-usage-metrics payload)))
    (should (equal (plist-get result :context-used) 10000))
    (should (equal (plist-get result :context-max) 128000))
    (should (= (plist-get result :cost) 3.5))
    (should (equal (plist-get result :cost-currency) "EUR"))))

(ert-deftest agent-pane-extract-usage--prompt-response-usage-field ()
  "PromptResponse with nested `usage' containing token counts."
  (let* ((payload '((sessionId . "sess_abc")
                    (stopReason . "end_turn")
                    (usage . ((total_tokens . 53000)
                              (input_tokens . 35000)
                              (output_tokens . 12000)))))
         (result (agent-pane--extract-usage-metrics payload)))
    (should (equal (plist-get result :input-tokens) 35000))
    (should (equal (plist-get result :output-tokens) 12000))
    (should (equal (plist-get result :total-tokens) 53000))))

(ert-deftest agent-pane-extract-usage--prompt-response-camel-case ()
  "PromptResponse with camelCase token fields."
  (let* ((payload '((usage . ((inputTokens . 1000)
                              (outputTokens . 500)
                              (totalTokens . 1500)))))
         (result (agent-pane--extract-usage-metrics payload)))
    (should (equal (plist-get result :input-tokens) 1000))
    (should (equal (plist-get result :output-tokens) 500))
    (should (equal (plist-get result :total-tokens) 1500))))

(ert-deftest agent-pane-extract-usage--context-window-nested ()
  "Payload with nested contextWindow object."
  (let* ((payload '((contextWindow . ((usedTokens . 8000)
                                      (maxTokens . 32000)))))
         (result (agent-pane--extract-usage-metrics payload)))
    (should (equal (plist-get result :context-used) 8000))
    (should (equal (plist-get result :context-max) 32000))))

(ert-deftest agent-pane-extract-usage--context-window-snake-case ()
  "Payload with snake_case context_window object."
  (let* ((payload '((context_window . ((used_tokens . 4000)
                                       (max_tokens . 16000)))))
         (result (agent-pane--extract-usage-metrics payload)))
    (should (equal (plist-get result :context-used) 4000))
    (should (equal (plist-get result :context-max) 16000))))

(ert-deftest agent-pane-extract-usage--context-utilization-percent ()
  "Explicit utilization percentage is normalized to 0..1."
  (let* ((payload '((contextWindow . ((usedTokens . 5000)
                                      (maxTokens . 10000)
                                      (utilization . 50)))))
         (result (agent-pane--extract-usage-metrics payload)))
    (should (= (plist-get result :context-utilization) 0.5))))

(ert-deftest agent-pane-extract-usage--context-utilization-ratio ()
  "Utilization already in 0..1 range passes through."
  (let* ((payload '((contextWindow . ((usedTokens . 5000)
                                      (maxTokens . 10000)
                                      (utilization . 0.42))))))
    (should (= (plist-get (agent-pane--extract-usage-metrics payload)
                          :context-utilization)
               0.42))))

(ert-deftest agent-pane-extract-usage--cost-usd-shorthand ()
  "Top-level costUsd number."
  (let* ((payload '((usage . ((totalTokens . 100)))
                    (costUsd . 0.0023)))
         (result (agent-pane--extract-usage-metrics payload)))
    (should (= (plist-get result :cost) 0.0023))))

(ert-deftest agent-pane-extract-usage--empty-payload-returns-nil ()
  "Empty or unrelated payload returns nil."
  (should (null (agent-pane--extract-usage-metrics nil)))
  (should (null (agent-pane--extract-usage-metrics '((foo . "bar"))))))

(ert-deftest agent-pane-extract-usage--zero-values-accepted ()
  "Zero token counts are valid non-negative integers."
  (let* ((payload '((usage . ((input_tokens . 0)
                              (output_tokens . 0)
                              (total_tokens . 0)))))
         (result (agent-pane--extract-usage-metrics payload)))
    (should (equal (plist-get result :input-tokens) 0))
    (should (equal (plist-get result :output-tokens) 0))
    (should (equal (plist-get result :total-tokens) 0))))

;; ---------------------------------------------------------------------------
;; agent-pane--update-usage-metrics
;; ---------------------------------------------------------------------------

(ert-deftest agent-pane-update-usage--merges-into-empty-state ()
  "First usage_update populates metrics from nil."
  (with-temp-buffer
    (agent-pane-mode)
    (let ((payload '((sessionUpdate . "usage_update")
                     (used . 10000)
                     (size . 200000))))
      (should (agent-pane--update-usage-metrics payload))
      (let ((m (map-elt agent-pane--state :usage-metrics)))
        (should (equal (plist-get m :context-used) 10000))
        (should (equal (plist-get m :context-max) 200000))
        ;; Auto-computed utilization.
        (should (= (plist-get m :context-utilization) 0.05))))))

(ert-deftest agent-pane-update-usage--merges-incrementally ()
  "Subsequent updates overwrite changed fields, keep others."
  (with-temp-buffer
    (agent-pane-mode)
    ;; First update: context window.
    (agent-pane--update-usage-metrics
     '((sessionUpdate . "usage_update")
       (used . 10000)
       (size . 200000)))
    ;; Second update: only token counts from a prompt response.
    (agent-pane--update-usage-metrics
     '((usage . ((input_tokens . 5000)
                 (output_tokens . 2000)
                 (total_tokens . 7000)))))
    (let ((m (map-elt agent-pane--state :usage-metrics)))
      ;; Context window data persists.
      (should (equal (plist-get m :context-used) 10000))
      (should (equal (plist-get m :context-max) 200000))
      ;; Token counts added.
      (should (equal (plist-get m :input-tokens) 5000))
      (should (equal (plist-get m :output-tokens) 2000)))))

(ert-deftest agent-pane-update-usage--returns-nil-for-no-change ()
  "Returns nil when called with same payload twice."
  (with-temp-buffer
    (agent-pane-mode)
    (let ((payload '((sessionUpdate . "usage_update")
                     (used . 1000)
                     (size . 10000))))
      (should (agent-pane--update-usage-metrics payload))
      (should-not (agent-pane--update-usage-metrics payload)))))

(ert-deftest agent-pane-update-usage--auto-computes-utilization ()
  "Utilization is derived from used/max when not explicit."
  (with-temp-buffer
    (agent-pane-mode)
    (agent-pane--update-usage-metrics
     '((sessionUpdate . "usage_update")
       (used . 50000)
       (size . 200000)))
    (let ((m (map-elt agent-pane--state :usage-metrics)))
      (should (= (plist-get m :context-utilization) 0.25)))))

;; ---------------------------------------------------------------------------
;; agent-pane--ui-usage-summary
;; ---------------------------------------------------------------------------

(ert-deftest agent-pane-ui-usage-summary--nil-without-metrics ()
  "Returns nil when no usage metrics and estimation is disabled."
  (with-temp-buffer
    (agent-pane-mode)
    (let ((agent-pane-estimate-context-usage nil))
      (should (null (agent-pane--ui-usage-summary))))))

(ert-deftest agent-pane-ui-usage-summary--context-percent-and-counts ()
  "Shows ctx=NN% (used/max) when context window data is available."
  (with-temp-buffer
    (agent-pane-mode)
    (agent-pane--update-usage-metrics
     '((sessionUpdate . "usage_update")
       (used . 53000)
       (size . 200000)))
    (let ((summary (agent-pane--ui-usage-summary)))
      (should (stringp summary))
      (should (string-match-p "ctx=26%" summary))
      (should (string-match-p "(53000/200000)" summary)))))

(ert-deftest agent-pane-ui-usage-summary--cost-usd ()
  "Shows cost=$X.XXXX for USD cost."
  (with-temp-buffer
    (agent-pane-mode)
    (agent-pane--update-usage-metrics
     '((sessionUpdate . "usage_update")
       (used . 10000)
       (size . 200000)
       (cost . ((amount . 0.045)
                (currency . "USD")))))
    (let ((summary (agent-pane--ui-usage-summary)))
      (should (string-match-p "cost=\\$0.045" summary)))))

(ert-deftest agent-pane-ui-usage-summary--cost-non-usd ()
  "Shows cost=X.XXXX CCY for non-USD."
  (with-temp-buffer
    (agent-pane-mode)
    (agent-pane--update-usage-metrics
     '((sessionUpdate . "usage_update")
       (used . 10000)
       (size . 128000)
       (cost . ((amount . 3.5)
                (currency . "EUR")))))
    (let ((summary (agent-pane--ui-usage-summary)))
      (should (string-match-p "cost=3.5 EUR" summary)))))

(ert-deftest agent-pane-ui-usage-summary--context-and-cost-together ()
  "Both context and cost appear when available."
  (with-temp-buffer
    (agent-pane-mode)
    (agent-pane--update-usage-metrics
     '((sessionUpdate . "usage_update")
       (used . 100000)
       (size . 200000)
       (cost . ((amount . 1.23)
                (currency . "USD")))))
    (let ((summary (agent-pane--ui-usage-summary)))
      (should (string-match-p "ctx=50%" summary))
      (should (string-match-p "cost=\\$1.23" summary)))))

;; ---------------------------------------------------------------------------
;; agent-pane--ui-header-line
;; ---------------------------------------------------------------------------

(ert-deftest agent-pane-ui-header-line--includes-usage-when-present ()
  "Header line includes usage summary after provider/model."
  (with-temp-buffer
    (agent-pane-mode)
    (agent-pane--update-usage-metrics
     '((sessionUpdate . "usage_update")
       (used . 53000)
       (size . 200000)))
    (let ((header (agent-pane--ui-header-line)))
      (should (string-match-p "provider=" header))
      (should (string-match-p "model=" header))
      (should (string-match-p "ctx=26%" header)))))

(ert-deftest agent-pane-ui-header-line--omits-usage-when-absent ()
  "Header line has no trailing usage portion without metrics or estimation."
  (with-temp-buffer
    (agent-pane-mode)
    (let ((agent-pane-estimate-context-usage nil)
          (header (agent-pane--ui-header-line)))
      (should (string-match-p "provider=" header))
      (should (string-match-p "model=" header))
      (should-not (string-match-p "ctx=" header)))))

;; ---------------------------------------------------------------------------
;; agent-pane--ui-format-cost
;; ---------------------------------------------------------------------------

(ert-deftest agent-pane-format-cost--usd ()
  "USD costs are formatted with $ prefix."
  (should (equal (agent-pane--ui-format-cost 0.045 "USD") "$0.045"))
  (should (equal (agent-pane--ui-format-cost 0.045 nil) "$0.045")))

(ert-deftest agent-pane-format-cost--non-usd ()
  "Non-USD costs show the currency code."
  (should (equal (agent-pane--ui-format-cost 3.5 "EUR") "3.5 EUR")))

(ert-deftest agent-pane-format-cost--trailing-zeros-stripped ()
  "Trailing zeros after decimal are removed."
  (should (equal (agent-pane--ui-format-cost 1.0 nil) "$1"))
  (should (equal (agent-pane--ui-format-cost 1.5 nil) "$1.5")))

;; ---------------------------------------------------------------------------
;; agent-pane--normalize-utilization-ratio
;; ---------------------------------------------------------------------------

(ert-deftest agent-pane-normalize-utilization--ratio ()
  "Values in 0..1 pass through."
  (should (= (agent-pane--normalize-utilization-ratio 0.42) 0.42))
  (should (= (agent-pane--normalize-utilization-ratio 0.0) 0.0))
  (should (= (agent-pane--normalize-utilization-ratio 1.0) 1.0)))

(ert-deftest agent-pane-normalize-utilization--percent ()
  "Values > 1 and <= 100 are treated as percentages."
  (should (= (agent-pane--normalize-utilization-ratio 50) 0.5))
  (should (= (agent-pane--normalize-utilization-ratio 100) 1.0)))

(ert-deftest agent-pane-normalize-utilization--out-of-range ()
  "Negative or > 100 values return nil."
  (should (null (agent-pane--normalize-utilization-ratio -1)))
  (should (null (agent-pane--normalize-utilization-ratio 101))))

(ert-deftest agent-pane-normalize-utilization--nil-input ()
  "Nil input returns nil."
  (should (null (agent-pane--normalize-utilization-ratio nil))))

;; ---------------------------------------------------------------------------
;; Client-side token estimation
;; ---------------------------------------------------------------------------

(ert-deftest agent-pane-estimate-tokens--basic ()
  "Rough 4 chars/token estimation."
  (should (= (agent-pane--estimate-tokens "hello world!") 3))
  (should (= (agent-pane--estimate-tokens "") 1))
  (should (= (agent-pane--estimate-tokens nil) 0)))

(ert-deftest agent-pane-model-context-size--known-model ()
  "Known models return their registered size."
  (should (= (agent-pane--model-context-size "claude-sonnet-4.5") 200000))
  (should (= (agent-pane--model-context-size "gpt-5-mini") 128000))
  (should (= (agent-pane--model-context-size "claude-opus-4.6-1m") 1000000)))

(ert-deftest agent-pane-model-context-size--1m-heuristic ()
  "Models with 1m in the name get 1M context."
  (should (= (agent-pane--model-context-size "some-model-1m") 1000000)))

(ert-deftest agent-pane-model-context-size--unknown-model ()
  "Unknown models get default size."
  (should (= (agent-pane--model-context-size "unknown-model-xyz")
             agent-pane--default-context-size))
  (should (= (agent-pane--model-context-size nil)
             agent-pane--default-context-size)))

(ert-deftest agent-pane-estimate-context-usage--empty-chat ()
  "Empty chat estimates zero usage."
  (with-temp-buffer
    (agent-pane-mode)
    (let ((est (agent-pane--estimate-context-usage)))
      (should (= (plist-get est :context-used) 0))
      (should (> (plist-get est :context-max) 0)))))

(ert-deftest agent-pane-estimate-context-usage--with-messages ()
  "Messages contribute to estimated token count."
  (with-temp-buffer
    (agent-pane-mode)
    (agent-pane--append-message* :role 'user :text (make-string 400 ?x))
    (agent-pane--append-message* :role 'assistant :text (make-string 800 ?y))
    (let ((est (agent-pane--estimate-context-usage)))
      ;; 1200 chars / 4 = 300 tokens.
      (should (= (plist-get est :context-used) 300))
      (should (= (plist-get est :context-max) agent-pane--default-context-size)))))

(ert-deftest agent-pane-estimate-context-usage--includes-tool-output ()
  "Tool call outputs are counted in estimation."
  (with-temp-buffer
    (agent-pane-mode)
    (agent-pane--append-message* :role 'user :text (make-string 40 ?x))
    ;; Simulate a tool call with output stored in the tool-calls table.
    (let ((tool-table (map-elt agent-pane--state :tool-calls)))
      (puthash "tool-1"
               (list :toolCallId "tool-1"
                     :output (make-string 800 ?z))
               tool-table))
    (let ((est (agent-pane--estimate-context-usage)))
      ;; 40 (user) + 800 (tool output) = 840 chars / 4 = 210 tokens.
      (should (= (plist-get est :context-used) 210)))))

(ert-deftest agent-pane-estimate-context-usage--uses-current-model ()
  "Estimation uses current model for context max."
  (with-temp-buffer
    (agent-pane-mode)
    (map-put! agent-pane--state :current-model-id "gpt-5-mini")
    (let ((est (agent-pane--estimate-context-usage)))
      (should (= (plist-get est :context-max) 128000)))))

(ert-deftest agent-pane-ui-usage-summary--estimated-has-tilde ()
  "Estimated usage is prefixed with ~ to distinguish from server data."
  (with-temp-buffer
    (agent-pane-mode)
    (let ((agent-pane-estimate-context-usage t))
      (agent-pane--append-message* :role 'user :text (make-string 400 ?x))
      (let ((summary (agent-pane--ui-usage-summary)))
        (should (stringp summary))
        (should (string-match-p "ctx~=" summary))
        (should (string-match-p "(~" summary))))))

(ert-deftest agent-pane-ui-usage-summary--server-overrides-estimate ()
  "Server-reported metrics take priority over estimation."
  (with-temp-buffer
    (agent-pane-mode)
    (let ((agent-pane-estimate-context-usage t))
      (agent-pane--append-message* :role 'user :text (make-string 400 ?x))
      (agent-pane--update-usage-metrics
       '((sessionUpdate . "usage_update")
         (used . 53000)
         (size . 200000)))
      (let ((summary (agent-pane--ui-usage-summary)))
        ;; Server data: no tilde prefix.
        (should (string-match-p "ctx=26%" summary))
        (should-not (string-match-p "~" summary))))))

(ert-deftest agent-pane-ui-usage-summary--estimation-disabled ()
  "No summary when estimation is disabled and no server metrics."
  (with-temp-buffer
    (agent-pane-mode)
    (let ((agent-pane-estimate-context-usage nil))
      (agent-pane--append-message* :role 'user :text (make-string 400 ?x))
      (should (null (agent-pane--ui-usage-summary))))))

(provide 'agent-pane-usage-tests)

;;; agent-pane-usage-tests.el ends here

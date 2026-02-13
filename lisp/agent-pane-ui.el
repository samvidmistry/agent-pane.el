;;; agent-pane-ui.el --- Incremental UI rendering for agent-pane -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "29.1"))
;;; Commentary:
;;
;; This module owns the agent-pane buffer layout and the incremental rendering
;; logic (anti-flicker).  It deliberately avoids ACP protocol wiring; it only
;; reads `agent-pane--state` and mutates the current buffer.
;;
;;; Code:
(require 'agent-pane-config)
(require 'agent-pane-markdown)
(require 'agent-pane-state)
(require 'agent-pane-util)
(require 'acp) ;; for traffic/log buffers in the header
(require 'cl-lib)
(require 'map)
(require 'subr-x)
(defvar agent-pane-acp-provider)
(defvar agent-pane-session-model-id)
(defvar agent-pane-header-details-collapsed)
(declare-function agent-pane-sessions-rerender-if-visible "agent-pane-sessions")
(defvar-local agent-pane--input-marker nil
  "Marker pointing to the start of the editable input area.")
(defvar-local agent-pane--header-end-marker nil
  "Marker pointing to the end of the rendered header region.")
(defvar-local agent-pane--transcript-end-marker nil
  "Marker pointing to the end of the rendered transcript (start of footer).")
(defvar-local agent-pane--status-beg-marker nil
  "Marker pointing to the beginning of the status line in the footer.")
(defvar-local agent-pane--status-end-marker nil
  "Marker pointing to the end of the status line in the footer.")
(defvar-local agent-pane--rendered-message-count 0
  "Number of messages currently rendered into the transcript region.")
(defvar-local agent-pane--msg-locs nil
  "Hash table mapping message objects (eq) to marker plists.
Used for incremental updates (append/replace message body) without full
buffer re-rendering.")
(defvar-local agent-pane--fold-overlays nil
  "Hash table mapping message objects (eq) to fold overlays.")
(defvar-local agent-pane--header-details-collapsed nil
  "Whether verbose ACP header details are currently collapsed.")
(defun agent-pane--goto-input ()
  "Move point to the editable input area."
  (goto-char (or agent-pane--input-marker (point-max))))
(defun agent-pane--input-text ()
  "Return the current input text, trimmed, or nil if unavailable."
  (when agent-pane--input-marker
    (string-trim (buffer-substring-no-properties agent-pane--input-marker (point-max)))))
(defun agent-pane--clear-input ()
  "Delete the editable input region."
  (when agent-pane--input-marker
    (let ((inhibit-read-only t))
      (delete-region agent-pane--input-marker (point-max)))))
(defun agent-pane--set-input-text (text)
  "Replace the editable input region with TEXT."
  (when agent-pane--input-marker
    (let ((inhibit-read-only t))
      (delete-region agent-pane--input-marker (point-max))
      (goto-char agent-pane--input-marker)
      (insert (or text "")))))
(defun agent-pane--insert-read-only (s &rest props)
  "Insert S and mark it read-only, applying PROPS as text properties."
  (let ((start (point)))
    (insert s)
    ;; Prevent `read-only' from sticking onto subsequently inserted input.
    (add-text-properties
     start (point)
     (append (list 'read-only t
                   'front-sticky '(read-only)
                   'rear-nonsticky '(read-only))
             props))))
(defmacro agent-pane--with-preserved-point (&rest body)
  "Evaluate BODY, preserving point relative to inserted text.
This uses a marker so point stays at the same logical location even if text is
inserted before it (which would otherwise break `save-excursion' for users
typing in the input area)."
  (declare (indent 0))
  `(let ((m (copy-marker (point) t)))
     (unwind-protect
         (progn ,@body)
       (goto-char m)
       (set-marker m nil))))
(defun agent-pane--ui-layout-ready-p ()
  "Return non-nil if the buffer has an initialized agent-pane layout."
  (and (markerp agent-pane--input-marker)
       (marker-buffer agent-pane--input-marker)
       (markerp agent-pane--header-end-marker)
       (marker-buffer agent-pane--header-end-marker)
       (markerp agent-pane--transcript-end-marker)
       (marker-buffer agent-pane--transcript-end-marker)
       (markerp agent-pane--status-beg-marker)
       (marker-buffer agent-pane--status-beg-marker)
       (markerp agent-pane--status-end-marker)
       (marker-buffer agent-pane--status-end-marker)
       (hash-table-p agent-pane--msg-locs)
       (integerp agent-pane--rendered-message-count)))
(defun agent-pane--ui-status-line ()
  "Compute the status line text (without trailing newline)."
  (let* ((connecting (map-elt agent-pane--state :connecting))
         (in-progress (map-elt agent-pane--state :in-progress))
         (session-id (map-elt agent-pane--state :session-id))
         (queue (map-elt agent-pane--state :prompt-queue))
         (queued (if (listp queue) (length queue) 0))
         (queue-suffix (if (> queued 0) (format " (queued: %d)" queued) "")))
    (concat
     (cond
      (connecting
       (format "Status: connecting%s"
               (if session-id (format " (session=%s)" session-id) "")))
      ((eq in-progress 'waiting)
       "Status: waiting for agent…")
      ((eq in-progress 'cancelling)
       "Status: cancelling…")
      ((eq in-progress 'streaming)
       "Status: agent is responding…")
      (in-progress
       "Status: waiting for agent…")
      (t
       "Status: ready"))
     queue-suffix)))
(defun agent-pane--header-provider-label ()
  "Return current provider label for header summary/status text."
  (format "%s" (or agent-pane-acp-provider 'codex)))

(defconst agent-pane--default-context-size 200000
  "Fallback model context window size used for local estimation.")

(defconst agent-pane--known-model-context-sizes
  '(("claude-sonnet-4.5" . 200000)
    ("claude-opus-4.6-1m" . 1000000)
    ("gpt-5-mini" . 128000))
  "Known model context window sizes keyed by normalized model id.")

(defun agent-pane--usage-get (obj keys)
  "Return first mapped value from OBJ for KEYS, or nil."
  (catch 'found
    (dolist (key keys)
      (condition-case nil
          (when (map-contains-key obj key)
            (throw 'found (map-elt obj key)))
        (error nil)))
    nil))

(defun agent-pane--normalize-nonnegative-integer (value)
  "Return VALUE as a non-negative integer, or nil."
  (when (and (numberp value)
             (>= value 0)
             (= value (truncate value)))
    (truncate value)))

(defun agent-pane--normalize-utilization-ratio (value)
  "Normalize utilization VALUE to a ratio in the range 0..1."
  (when (numberp value)
    (cond
     ((and (>= value 0.0) (<= value 1.0))
      value)
     ((and (> value 1.0) (<= value 100.0))
      (/ value 100.0))
     (t nil))))

(defun agent-pane--extract-usage-metrics (payload)
  "Extract usage metrics plist from ACP PAYLOAD, or nil when absent."
  (let* ((usage (agent-pane--usage-get payload '(usage)))
         (context-window (or (agent-pane--usage-get payload '(contextWindow context_window))
                             (agent-pane--usage-get usage '(contextWindow context_window))))
         (context-used
          (agent-pane--normalize-nonnegative-integer
           (or (agent-pane--usage-get context-window '(usedTokens used_tokens used))
               (agent-pane--usage-get payload '(used usedTokens used_tokens)))))
         (context-max
          (agent-pane--normalize-nonnegative-integer
           (or (agent-pane--usage-get context-window '(maxTokens max_tokens size max))
               (agent-pane--usage-get payload '(size maxTokens max_tokens max)))))
         (utilization
          (agent-pane--normalize-utilization-ratio
           (or (agent-pane--usage-get context-window '(utilization))
               (agent-pane--usage-get payload '(utilization)))))
         (input-tokens
          (agent-pane--normalize-nonnegative-integer
           (or (agent-pane--usage-get usage '(input_tokens inputTokens))
               (agent-pane--usage-get payload '(input_tokens inputTokens)))))
         (output-tokens
          (agent-pane--normalize-nonnegative-integer
           (or (agent-pane--usage-get usage '(output_tokens outputTokens))
               (agent-pane--usage-get payload '(output_tokens outputTokens)))))
         (total-tokens
          (agent-pane--normalize-nonnegative-integer
           (or (agent-pane--usage-get usage '(total_tokens totalTokens))
               (agent-pane--usage-get payload '(total_tokens totalTokens)))))
         (cost-map (or (agent-pane--usage-get payload '(cost))
                       (agent-pane--usage-get usage '(cost))))
         (cost-usd (agent-pane--usage-get payload '(costUsd cost_usd)))
         (cost (cond
                ((numberp cost-usd) cost-usd)
                ((numberp (agent-pane--usage-get cost-map '(amount)))
                 (agent-pane--usage-get cost-map '(amount)))
                (t nil)))
         (cost-currency (or (agent-pane--usage-get cost-map '(currency))
                            (and (numberp cost-usd) "USD")))
         metrics)
    (when (numberp context-used)
      (setq metrics (plist-put metrics :context-used context-used)))
    (when (numberp context-max)
      (setq metrics (plist-put metrics :context-max context-max)))
    (when (numberp utilization)
      (setq metrics (plist-put metrics :context-utilization utilization)))
    (when (numberp input-tokens)
      (setq metrics (plist-put metrics :input-tokens input-tokens)))
    (when (numberp output-tokens)
      (setq metrics (plist-put metrics :output-tokens output-tokens)))
    (when (numberp total-tokens)
      (setq metrics (plist-put metrics :total-tokens total-tokens)))
    (when (numberp cost)
      (setq metrics (plist-put metrics :cost cost))
      (when (stringp cost-currency)
        (setq metrics (plist-put metrics :cost-currency cost-currency))))
    metrics))

(defun agent-pane--update-usage-metrics (payload)
  "Merge usage metrics extracted from PAYLOAD into chat state.
Return non-nil when metrics changed."
  (agent-pane--ensure-state)
  (let ((delta (agent-pane--extract-usage-metrics payload)))
    (when delta
      (let* ((current (copy-sequence (or (map-elt agent-pane--state :usage-metrics) nil)))
             (next (copy-sequence current))
             (delta-iter delta)
             (context-changed (or (plist-member delta :context-used)
                                  (plist-member delta :context-max))))
        (while delta-iter
          (setq next (plist-put next (car delta-iter) (cadr delta-iter)))
          (setq delta-iter (cddr delta-iter)))
        (when (and context-changed
                   (not (plist-member delta :context-utilization)))
          (let ((used (plist-get next :context-used))
                (max (plist-get next :context-max)))
            (when (and (numberp used) (numberp max) (> max 0))
              (setq next (plist-put next :context-utilization
                                    (/ (float used) (float max)))))))
        (unless (equal current next)
          (map-put! agent-pane--state :usage-metrics next)
          t)))))

(defun agent-pane--ui-format-cost (amount currency)
  "Return display string for COST AMOUNT and CURRENCY."
  (when (numberp amount)
    (let* ((formatted (format "%.6f" amount))
           (trimmed (replace-regexp-in-string "\\.?0+\\'" "" formatted))
           (ccy (if (stringp currency) (upcase currency) "USD")))
      (if (equal ccy "USD")
          (format "$%s" trimmed)
        (format "%s %s" trimmed ccy)))))

(defun agent-pane--estimate-tokens (text)
  "Estimate token count for TEXT using a rough chars-per-token heuristic."
  (cond
   ((null text) 0)
   ((not (stringp text))
    (agent-pane--estimate-tokens (format "%s" text)))
   ((string-empty-p text) 1)
   (t
    (ceiling (/ (float (length text)) 4.0)))))

(defun agent-pane--model-context-size (model-id)
  "Return estimated context size for MODEL-ID."
  (let ((id (and (stringp model-id) (downcase (string-trim model-id)))))
    (cond
     ((not (stringp id))
      agent-pane--default-context-size)
     ((cdr (assoc id agent-pane--known-model-context-sizes)))
     ((string-match-p (rx (or string-start "-" "_")
                          "1m"
                          (or string-end "-" "_"))
                      id)
      1000000)
     (t
      agent-pane--default-context-size))))

(defun agent-pane--estimate-context-usage ()
  "Estimate context usage for the current chat from local message state."
  (agent-pane--ensure-state)
  (let ((used 0))
    (dolist (msg (or (map-elt agent-pane--state :messages) nil))
      (let ((text (map-elt msg :text)))
        (when (and (stringp text) (not (string-empty-p text)))
          (setq used (+ used (agent-pane--estimate-tokens text))))))
    (let ((tool-table (map-elt agent-pane--state :tool-calls)))
      (when (hash-table-p tool-table)
        (maphash (lambda (_id entry)
                   (let ((output (plist-get entry :output)))
                     (when (and (stringp output) (not (string-empty-p output)))
                       (setq used (+ used (agent-pane--estimate-tokens output))))))
                 tool-table)))
    (let* ((context-max (agent-pane--model-context-size
                         (map-elt agent-pane--state :current-model-id)))
           (utilization (and (> context-max 0)
                             (/ (float used) (float context-max)))))
      (list :context-used used
            :context-max context-max
            :context-utilization utilization))))

(defun agent-pane--ui-usage-summary ()
  "Return compact usage summary text, or nil when unavailable."
  (let* ((server (map-elt agent-pane--state :usage-metrics))
         (server-used (plist-get server :context-used))
         (server-max (plist-get server :context-max))
         (server-has-context (and (numberp server-used)
                                  (numberp server-max)
                                  (> server-max 0)))
         (estimated (and agent-pane-estimate-context-usage
                         (not server-has-context)
                         (agent-pane--estimate-context-usage)))
         (context-used (if server-has-context
                           server-used
                         (plist-get estimated :context-used)))
         (context-max (if server-has-context
                          server-max
                        (plist-get estimated :context-max)))
         (context-utilization
          (or (and server-has-context
                   (or (plist-get server :context-utilization)
                       (/ (float server-used) (float server-max))))
              (plist-get estimated :context-utilization)))
         (estimated-context (and (not server-has-context) estimated))
         (cost (plist-get server :cost))
         (cost-currency (plist-get server :cost-currency))
         parts)
    (when (and (numberp context-used)
               (numberp context-max)
               (> context-max 0)
               (numberp context-utilization))
      (let ((pct (floor (* 100 context-utilization))))
        (push (if estimated-context
                  (format "ctx~=%d%% (~%d/%d)" pct context-used context-max)
                (format "ctx=%d%% (%d/%d)" pct context-used context-max))
              parts)))
    (when-let ((cost-text (agent-pane--ui-format-cost cost cost-currency)))
      (push (format "cost=%s" cost-text) parts))
    (when parts
      (string-join (nreverse parts) "  "))))

(defun agent-pane--ui-header-line ()
  "Compute the header-line text with status and provider/model metadata."
  (let* ((base (format "%s  provider=%s  model=%s"
                       (agent-pane--ui-status-line)
                       (agent-pane--header-provider-label)
                       (agent-pane--header-model-label)))
         (usage (agent-pane--ui-usage-summary)))
    (if usage
        (format "%s  %s" base usage)
      base)))

(defun agent-pane--ui-status-face ()
  "Return the face symbol used for the current status line."
  (let ((in-progress (map-elt agent-pane--state :in-progress))
        (connecting (map-elt agent-pane--state :connecting)))
    (if (or connecting
            (memq in-progress '(waiting streaming cancelling))
            in-progress)
        'agent-pane-status-busy
      'agent-pane-status-ready)))

(defun agent-pane--ui-update-status-line ()
  "Update the footer status line in-place (and optionally header-line)."
  (let* ((s (agent-pane--ui-status-line))
         (face (agent-pane--ui-status-face))
         (header (if agent-pane-show-header-line-status
                     (propertize (agent-pane--ui-header-line) 'face face)
                   s)))
    (when agent-pane-show-header-line-status
      (setq-local header-line-format header))
    (when (agent-pane--ui-layout-ready-p)
      (agent-pane--with-preserved-point
        (let ((inhibit-read-only t)
              (start (marker-position agent-pane--status-beg-marker))
              (end (marker-position agent-pane--status-end-marker)))
          (when (and start end (<= start end))
            (delete-region start end)
            (goto-char start)
            (agent-pane--insert-read-only
             (concat s "\n")
             'agent-pane-status t
             'face face)
            (set-marker agent-pane--status-end-marker (point))))))
    (when (fboundp 'agent-pane-sessions-rerender-if-visible)
      (ignore-errors
        (agent-pane-sessions-rerender-if-visible)))))
(defun agent-pane--ui--store-message-loc (msg beg label-end text-beg text-end end)
  "Store marker locations for MSG.
BEG is the message start.
LABEL-END is end of the label line.
TEXT-BEG and TEXT-END bound the body text.
END is the message end (after trailing blank line)."
  (unless (hash-table-p agent-pane--msg-locs)
    (setq-local agent-pane--msg-locs (make-hash-table :test 'eq)))
  (let ((loc (list :beg beg
                   :label-end label-end
                   :text-beg text-beg
                   :text-end text-end
                   :end end)))
    (puthash msg loc agent-pane--msg-locs)
    loc))
(defun agent-pane--ui-message-loc (msg)
  "Return marker loc plist for MSG, if available."
  (and (hash-table-p agent-pane--msg-locs)
       (gethash msg agent-pane--msg-locs)))
(defun agent-pane--message-label-and-face (msg)
  "Return (LABEL . FACE) for MSG."
  (let* ((role0 (map-elt msg :role))
         (role (if (stringp role0) (intern role0) role0))
         (queued (and (eq role 'user) (map-elt msg :queued)))
         (title (map-elt msg :title))
         (label (pcase role
                  ('user (if queued "You (queued)" "You"))
                  ('assistant "Agent")
                  ('system "System")
                  ('thought "Thought")
                  ('tool "Tool")
                  ('acp "ACP")
                  (_ (format "%s" role))))
         (face (pcase role
                 ('user 'agent-pane-role-user)
                 ('assistant 'agent-pane-role-assistant)
                 ('system 'agent-pane-role-system)
                 ('thought 'agent-pane-role-thought)
                 ('tool 'agent-pane-role-tool)
                 ('acp 'agent-pane-role-acp)
                 (_ 'default))))
    (cons (if (and (stringp title)
                   (not (string-empty-p title)))
              (format "%s (%s)" label title)
             label)
           face)))

(defun agent-pane--ui-queued-message-p (msg)
  "Return non-nil when MSG is a queued user prompt."
  (and (eq (map-elt msg :role) 'user)
       (map-elt msg :queued)))

(defun agent-pane--ui-first-queued-position ()
  "Return buffer position of first rendered queued message, or nil."
  (let ((messages (map-elt agent-pane--state :messages))
        found)
    (while (and messages (not found))
      (let ((msg (car messages)))
        (when (agent-pane--ui-queued-message-p msg)
          (when-let* ((loc (agent-pane--ui-message-loc msg))
                      (beg (plist-get loc :beg))
                      (pos (and (markerp beg) (marker-position beg))))
            (setq found pos))))
      (setq messages (cdr messages)))
    found))
(defun agent-pane--ui--apply-face-to-lines (beg end face)
  "Apply FACE to whole lines spanning BEG..END, including line endings."
  (save-excursion
    (goto-char beg)
    (beginning-of-line)
    (let ((from (point)))
      (goto-char end)
      (unless (bolp)
        (end-of-line))
      (let ((to (if (< (point) (point-max))
                    (1+ (point))
                  (point))))
        (add-text-properties from to (list 'face face))))))

(defun agent-pane--ui-style-tool-message (beg end)
  "Apply tool-specific styling within message region BEG..END."
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (when (re-search-forward (rx bol "tool:" (* nonl) eol) nil t)
        (agent-pane--ui--apply-face-to-lines (match-beginning 0)
                                             (match-end 0)
                                             'agent-pane-tool-invocation-block))
      (goto-char (point-min))
      (when (re-search-forward (rx bol "output (" (or "latest " "full") (* nonl) eol) nil t)
        (let ((block-beg (match-beginning 0)))
          (agent-pane--ui--apply-face-to-lines block-beg (point-max)
                                               'agent-pane-tool-output-block))))))

(defun agent-pane--ui-render-message (msg)
  "Insert MSG into current buffer and record marker locations."
  (let* ((start (point))
         (text (or (map-elt msg :text) ""))
         (raw (map-elt msg :raw))
         (lf (agent-pane--message-label-and-face msg))
         (label (car lf))
         (face (cdr lf)))
    (agent-pane--insert-read-only
     (propertize (format "%s:\n" label) 'face face)
     'agent-pane-message msg
     'agent-pane-raw raw)
    (let ((label-end-pos (point))
          (text-beg-pos (point)))
      (agent-pane--insert-read-only
       text
       'agent-pane-message msg
       'agent-pane-raw raw)
      (let ((text-end-pos (point)))
        (agent-pane--insert-read-only "\n\n" 'agent-pane-message msg 'agent-pane-raw raw)
        (let* ((beg (copy-marker start nil))
               (label-end (copy-marker label-end-pos nil))
               (text-beg (copy-marker text-beg-pos nil))
               (text-end (copy-marker text-end-pos t))
               (end (copy-marker (point) nil)))
          (agent-pane--ui--store-message-loc msg beg label-end text-beg text-end end)
          (agent-pane--md-fontify-range (map-elt msg :role)
                                        text-beg-pos text-end-pos)
          (when (eq (map-elt msg :role) 'tool)
            (agent-pane--ui-style-tool-message text-beg-pos text-end-pos)))))))
(defun agent-pane--ui-sync-new-messages ()
  "Append any not-yet-rendered messages from state into the transcript."
  (when (agent-pane--ui-layout-ready-p)
    (agent-pane--with-preserved-point
      (let ((inhibit-read-only t)
            (msgs (map-elt agent-pane--state :messages)))
        (when (< agent-pane--rendered-message-count (length msgs))
          (cl-loop for i from agent-pane--rendered-message-count below (length msgs)
                   for msg = (nth i msgs)
                   do (let* ((insert-at
                              (if (agent-pane--ui-queued-message-p msg)
                                  (marker-position agent-pane--transcript-end-marker)
                                (or (agent-pane--ui-first-queued-position)
                                    (marker-position agent-pane--transcript-end-marker)))))
                        (goto-char insert-at)
                        (agent-pane--ui-render-message msg)
                        (setq-local agent-pane--rendered-message-count
                                    (1+ agent-pane--rendered-message-count)))))))))
(defun agent-pane--ui-append-to-message-text (idx chunk)
  "Append CHUNK to message body at IDX in-place."
  (when (agent-pane--ui-layout-ready-p)
    (let* ((msgs (map-elt agent-pane--state :messages))
           (msg (nth idx msgs)))
      (unless msg
        (error "No message at idx=%s" idx))
      (agent-pane--ui-sync-new-messages)
      (let* ((loc (agent-pane--ui-message-loc msg))
             (text-end (plist-get loc :text-end)))
        (unless (markerp text-end)
          (error "Missing rendered location for idx=%s" idx))
        (agent-pane--with-preserved-point
          (let ((inhibit-read-only t)
                (old-end (marker-position text-end)))
            (goto-char text-end)
            (agent-pane--insert-read-only
             (or chunk "")
             'agent-pane-message msg
             'agent-pane-raw (map-elt msg :raw))
            (set-marker text-end (point))
            ;; Refontify the full message body so markdown delimiters that open
            ;; in older chunks and close in newer chunks are handled reliably.
            (let ((beg (or (and (markerp (plist-get loc :text-beg))
                                (marker-position (plist-get loc :text-beg)))
                           old-end)))
              (agent-pane--md-fontify-range (map-elt msg :role) beg (point)))
            (when (eq (map-elt msg :role) 'tool)
              (when-let ((tool-beg (plist-get loc :text-beg)))
                (agent-pane--ui-style-tool-message
                 (marker-position tool-beg)
                 (point))))))))))
(defun agent-pane--ui-set-message-text (idx text)
  "Replace message body at IDX with TEXT, in-place."
  (when (agent-pane--ui-layout-ready-p)
    (let* ((msgs (map-elt agent-pane--state :messages))
           (msg (nth idx msgs)))
      (unless msg
        (error "No message at idx=%s" idx))
      (agent-pane--ui-sync-new-messages)
      (let* ((loc (agent-pane--ui-message-loc msg))
             (text-beg (plist-get loc :text-beg))
             (text-end (plist-get loc :text-end)))
        (unless (and (markerp text-beg) (markerp text-end))
          (error "Missing rendered location for idx=%s" idx))
        (agent-pane--with-preserved-point
          (let ((inhibit-read-only t)
                (tb (marker-position text-beg)))
            (delete-region text-beg text-end)
            (goto-char text-beg)
            (agent-pane--insert-read-only
             (or text "")
             'agent-pane-message msg
             'agent-pane-raw (map-elt msg :raw))
            (set-marker text-end (point))
            (agent-pane--md-fontify-range (map-elt msg :role) tb (point))
            (when (eq (map-elt msg :role) 'tool)
              (agent-pane--ui-style-tool-message tb (point)))))))))
(defun agent-pane--ui-rerender-message (idx)
  "Re-render message IDX in-place (label + body).
Use this when fields affecting the label (like :title) change."
  (when (agent-pane--ui-layout-ready-p)
    (let* ((msgs (map-elt agent-pane--state :messages))
           (msg (nth idx msgs)))
      (unless msg
        (error "No message at idx=%s" idx))
      (agent-pane--ui-sync-new-messages)
      (let* ((loc (agent-pane--ui-message-loc msg))
             (beg (plist-get loc :beg))
             (end (plist-get loc :end)))
        (unless (and (markerp beg) (markerp end))
          (error "Missing rendered location for idx=%s" idx))
        (agent-pane--with-preserved-point
          (let ((inhibit-read-only t)
                (b (marker-position beg))
                (e (marker-position end)))
            (remhash msg agent-pane--msg-locs)
            (delete-region b e)
            (goto-char b)
            (agent-pane--ui-render-message msg)))))))
(defun agent-pane--ui-insert-footer (input)
  "Insert the footer and editable input area.
INPUT is the current input contents to restore (or nil)."
  (let* ((queue (map-elt agent-pane--state :prompt-queue))
         (queued (if (listp queue) (length queue) 0))
         (queue-preview-items nil)
         (preview
          (when (> queued 0)
            (setq queue-preview-items queue)
            (mapconcat
             (lambda (item)
               (string-trim (replace-regexp-in-string "[\n\r]+" " " (or item ""))))
             (cl-loop for item in queue-preview-items
                      for i from 1
                      while (<= i 3)
                      collect item)
             "\n"))))
    (agent-pane--insert-read-only "-----\n" 'face 'agent-pane-separator)
    (setq-local agent-pane--status-beg-marker (point-marker))
    (agent-pane--insert-read-only (concat (agent-pane--ui-status-line) "\n")
                                  'agent-pane-status t
                                  'face (agent-pane--ui-status-face))
    (setq-local agent-pane--status-end-marker (point-marker))
    (when preview
      (agent-pane--insert-read-only
       (concat "Queued prompts:\n" preview
               (if (> queued 3)
                   (format "\n... and %d more" (- queued 3))
                 "")
               "\n\n")
       'face 'agent-pane-footer-hint))
    (agent-pane--insert-read-only
     "Type your message below and press C-c C-c\n\n"
     'face 'agent-pane-footer-hint)
    (setq-local agent-pane--input-marker (point-max-marker))
    (when (and input (not (string-empty-p input)))
      (insert input))))
(defun agent-pane--ui-full-render ()
  "Render the entire agent-pane buffer from state.
This is used on first entry to `agent-pane-mode' and as a fallback if the
buffer layout gets corrupted."
  (agent-pane--ensure-state)
  (let ((input (agent-pane--input-text)))
    (agent-pane--with-preserved-point
      (let ((inhibit-read-only t)
            (messages
             (let* ((all (map-elt agent-pane--state :messages))
                    (normal nil)
                    (queued nil))
               (dolist (m all)
                 (if (agent-pane--ui-queued-message-p m)
                     (setq queued (append queued (list m)))
                   (setq normal (append normal (list m)))))
               (append normal queued))))
        (erase-buffer)
        (when (hash-table-p agent-pane--fold-overlays)
          (maphash (lambda (_msg ov)
                     (when (overlayp ov)
                       (delete-overlay ov)))
                   agent-pane--fold-overlays))
        (setq-local agent-pane--fold-overlays (make-hash-table :test 'eq))
        (setq-local agent-pane--msg-locs (make-hash-table :test 'eq))
        (setq-local agent-pane--rendered-message-count 0)
        ;; Header.
        (agent-pane--render-header)
        (setq-local agent-pane--header-end-marker (point-marker))
        ;; Transcript.
        (dolist (m messages)
          (agent-pane--ui-render-message m)
          (setq-local agent-pane--rendered-message-count
                      (1+ agent-pane--rendered-message-count)))
        ;; Footer boundary marker (start of footer).
        ;; Important: do NOT set insertion-type to t yet, otherwise inserting the
        ;; footer at this position would move the marker past the footer and
        ;; cause streamed messages to be inserted after the input area.
        (setq-local agent-pane--transcript-end-marker (point-marker))
        ;; Footer + input.
        (agent-pane--ui-insert-footer input)
        ;; Now that the footer exists, make the marker behave like an end marker
        ;; so new transcript insertions at this boundary keep the footer after
        ;; the transcript.
        (set-marker-insertion-type agent-pane--transcript-end-marker t)
        ;; Ensure status reflects current state.
        (agent-pane--ui-update-status-line)))))
(defun agent-pane--ui-ensure-layout ()
  "Ensure the agent-pane buffer has been rendered at least once."
  (unless (agent-pane--ui-layout-ready-p)
    (agent-pane--ui-full-render)))
(defun agent-pane--ui-update-header ()
  "Update the ACP header region in-place."
  (when (agent-pane--ui-layout-ready-p)
    (agent-pane--with-preserved-point
      (let ((inhibit-read-only t)
            (end (marker-position agent-pane--header-end-marker)))
        (when (and end (<= (point-min) end))
          (delete-region (point-min) end)
          (goto-char (point-min))
          (agent-pane--render-header)
          (set-marker agent-pane--header-end-marker (point)))))))
(defun agent-pane--ui-sync-transcript ()
  "Synchronize transcript/footer to state (without touching the header)."
  (agent-pane--ensure-state)
  (agent-pane--ui-ensure-layout)
  (agent-pane--ui-update-status-line)
  (agent-pane--ui-sync-new-messages))
(defun agent-pane--traffic-buffers-string ()
  "Return a short string describing ACP logs/traffic buffers."
  (let ((client (map-elt agent-pane--state :client)))
    (when client
      (format "traffic: %s  logs: %s"
              (buffer-name (acp-traffic-buffer :client client))
              (buffer-name (acp-logs-buffer :client client))))))
(defun agent-pane--acp-stderr-buffer-name (client)
  "Return the acp.el stderr buffer name for CLIENT, or nil."
  (when client
    (format "acp-client-stderr(%s)-%s"
            (map-elt client :command)
            (map-elt client :instance-count))))
(defun agent-pane--acp-stderr-buffer (client)
  "Return the acp.el stderr buffer for CLIENT, or nil."
  (when-let ((name (agent-pane--acp-stderr-buffer-name client)))
    (get-buffer name)))
(defun agent-pane--acp-pending-request-count (client)
  "Return the number of pending requests for CLIENT."
  (let ((pending (and client (map-elt client :pending-requests))))
    (if (listp pending)
        (length pending)
      0)))
(defun agent-pane--header-model-label ()
  "Return current model label for compact header summary."
  (agent-pane--ensure-state)
  (let* ((current-model (and (stringp (map-elt agent-pane--state :current-model-id))
                             (string-trim (map-elt agent-pane--state :current-model-id))))
         (configured-model (and (stringp agent-pane-session-model-id)
                                (string-trim agent-pane-session-model-id))))
    (cond
     ((and current-model (not (string-empty-p current-model)))
      current-model)
     ((and configured-model (not (string-empty-p configured-model)))
      configured-model)
     (t "provider-default"))))

(defun agent-pane--render-header ()
  "Insert ACP summary and optional debug details.
The compact row always stays visible and emphasizes provider/model.
Verbose connection/session/debug fields are shown only when expanded."
  (when agent-pane-show-acp-header
    (agent-pane--ensure-state)
    (let* ((client (map-elt agent-pane--state :client))
           (session-id (map-elt agent-pane--state :session-id))
           (collapsed (if (local-variable-p 'agent-pane--header-details-collapsed (current-buffer))
                          agent-pane--header-details-collapsed
                        agent-pane-header-details-collapsed)))
       (agent-pane--insert-read-only
        (propertize (format "ACP: provider=%s  model=%s  details=%s (C-c v)\n"
                            (agent-pane--header-provider-label)
                            (agent-pane--header-model-label)
                            (if collapsed "hidden" "shown"))
                   'face 'agent-pane-header-title)
       'agent-pane-header t
       'agent-pane-raw agent-pane--state)
      (if collapsed
          (agent-pane--insert-read-only
           "\n"
           'agent-pane-header t
           'face 'agent-pane-separator)
        (let* ((subscribed (map-elt agent-pane--state :subscribed))
               (init (map-elt agent-pane--state :init-result))
               (auth (map-elt agent-pane--state :auth-result))
               (session-new (map-elt agent-pane--state :session-new-result))
               (mode-result (map-elt agent-pane--state :session-mode-result))
               (last-prompt (map-elt agent-pane--state :last-prompt-result))
               (last-stop (and (listp last-prompt) (map-elt last-prompt 'stopReason)))
               (last-update (map-elt agent-pane--state :last-session-update-kind))
               (proto (and (listp init) (map-elt init 'protocolVersion)))
               (caps (and (listp init) (map-elt init 'agentCapabilities)))
               (load-session (and (listp caps) (map-elt caps 'loadSession)))
               (prompt-caps (and (listp caps) (map-elt caps 'promptCapabilities)))
               (pc-image (and (listp prompt-caps) (map-elt prompt-caps 'image)))
               (pc-audio (and (listp prompt-caps) (map-elt prompt-caps 'audio)))
               (pc-ctx (and (listp prompt-caps) (map-elt prompt-caps 'embeddedContext)))
               (auth-methods (agent-pane--listify (and (listp init) (map-elt init 'authMethods))))
               (auth-methods-str
                (if (null auth-methods)
                    "-"
                  (string-join
                   (mapcar (lambda (m)
                             (or (map-elt m 'id)
                                 (map-elt m 'name)
                                 (format "%s" m)))
                           auth-methods)
                   ", ")))
               (proc (and client (map-elt client :process)))
               (proc-live (and (processp proc) (process-live-p proc)))
               (instance (and client (map-elt client :instance-count)))
               (proc-info
                (cond
                 ((and client (map-elt client :agent-pane-fake)) "fake")
                 (proc-live (format "pid=%s" (process-id proc)))
                 (client "not-started")
                 (t "-")))
               (cmd (and client (map-elt client :command)))
               (cmd-params (and client (map-elt client :command-params)))
               (pending (agent-pane--acp-pending-request-count client))
               (last-req-id (and client (map-elt client :request-id)))
               (stderr-name (agent-pane--acp-stderr-buffer-name client))
               (tool-call-count (and (hash-table-p (map-elt agent-pane--state :tool-calls))
                                     (hash-table-count (map-elt agent-pane--state :tool-calls)))))
          (agent-pane--insert-read-only
           (format "  session=%s  subscribed=%s  notifSession=%s\n"
                   (or session-id "-")
                   (if subscribed "yes" "no")
                   (or (map-elt agent-pane--state :last-notification-session-id) "-"))
           'agent-pane-header t
           'agent-pane-raw (list :session-new session-new)
           'face 'agent-pane-header-info)
          (agent-pane--insert-read-only
           (format "  auth=%s  mode=%s  model=%s  permissions=%s\n"
                   (or agent-pane-auth-method-id "-")
                   (or agent-pane-session-mode-id "-")
                   (or agent-pane-session-model-id "-")
                   agent-pane-permission-policy)
           'agent-pane-header t
           'agent-pane-raw (list :auth auth :mode mode-result :model (map-elt agent-pane--state :session-model-result))
           'face 'agent-pane-header-info)
          (when cmd
            (agent-pane--insert-read-only
             (format "Client: %s %s  (instance=%s %s, pending=%s, lastRequestId=%s)\n"
                     cmd
                     (string-join (or cmd-params nil) " ")
                     (or instance "-")
                     proc-info
                     pending
                     (or last-req-id "-"))
             'agent-pane-header t
             'agent-pane-raw client
             'face 'agent-pane-header-info))
          (when proto
            (agent-pane--insert-read-only
             (format "Server: protocolVersion=%s  loadSession=%s  promptCaps(image=%s audio=%s embeddedContext=%s)\n"
                     proto
                     (if load-session "t" "nil")
                     (if pc-image "t" "nil")
                     (if pc-audio "t" "nil")
                     (if pc-ctx "t" "nil"))
             'agent-pane-header t
             'agent-pane-raw init
             'face 'agent-pane-header-info)
            (agent-pane--insert-read-only
             (format "  authMethods: %s\n" auth-methods-str)
             'agent-pane-header t
             'agent-pane-raw init
             'face 'agent-pane-header-info))
          (when (or last-stop last-update tool-call-count)
            (agent-pane--insert-read-only
             (format "Prompt: stopReason=%s  lastUpdate=%s  toolCalls=%s\n"
                     (or last-stop "-")
                     (or last-update "-")
                     (or tool-call-count 0))
             'agent-pane-header t
             'agent-pane-raw last-prompt
             'face 'agent-pane-header-info))
          (when-let ((bufs (agent-pane--traffic-buffers-string)))
            (agent-pane--insert-read-only
             (format "%s  stderr: %s\n" bufs (or stderr-name "-"))
             'agent-pane-header t
             'agent-pane-raw (list :traffic (and client (acp-traffic-buffer :client client))
                                   :logs (and client (acp-logs-buffer :client client))
                                   :stderr stderr-name)
             'face 'agent-pane-header-info))
          (agent-pane--insert-read-only
           "Keys: C-c . menu | C-c C-c send | C-c C-k cancel | C-c C-q exit | C-c v details | C-c m model | C-c d diff | C-c o output-mode | C-c i edit-input | C-c C-y last-user | C-c C-w last-tool | C-c C-b code-block | M-p/M-n history | C-c C-p/C-c C-n jump-msg | C-c C-f fold | C-c C-t traffic | C-c C-l logs | C-c C-e stderr | C-c C-s state | C-c C-r raw\n"
           'agent-pane-header t
           'face 'agent-pane-header-keys)
          (agent-pane--insert-read-only "\n" 'agent-pane-header t 'face 'agent-pane-separator))))))
(defun agent-pane--rerender ()
  "Synchronize the UI with current state.
This function avoids full-buffer re-rendering to prevent flicker.  It:
- ensures the layout exists
- updates the header/footer in-place
- appends any newly added transcript messages
Streaming chunk updates (agent/thought) should use in-place operations,
for example: `agent-pane--ui-append-to-message-text' and
`agent-pane--ui-set-message-text'."
  (agent-pane--ensure-state)
  (agent-pane--ui-ensure-layout)
  (agent-pane--ui-update-header)
  (agent-pane--ui-update-status-line)
  (agent-pane--ui-sync-new-messages)
  ;; When this chat is actively visible, treat current transcript as seen so
  ;; sidebar "done" indicators clear after the user looks at the thread.
  (when (and (window-live-p (selected-window))
             (eq (window-buffer (selected-window)) (current-buffer)))
    (when (agent-pane--mark-thread-seen)
      (when (fboundp 'agent-pane-sessions-rerender-if-visible)
        (ignore-errors
          (agent-pane-sessions-rerender-if-visible))))))
(provide 'agent-pane-ui)
;;; agent-pane-ui.el ends here

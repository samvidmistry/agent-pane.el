;;; agent-pane-config.el --- Customization for agent-pane -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "29.1"))
;;; Commentary:
;;
;; This file contains user-facing customization (defcustom/defgroup) and faces.
;; It is kept separate so the rest of the codebase can `require' it without
;; pulling in ACP wiring or UI code.
;;
;;; Code:
(defgroup agent-pane nil
  "Codex-style UI for ACP-backed agents."
  :group 'applications)
(defface agent-pane-role-user
  '((((class color) (background dark)) :foreground "#7dd3fc" :weight semibold)
    (((class color) (background light)) :foreground "#0369a1" :weight semibold)
    (t :inherit bold))
  "Face used for user role labels."
  :group 'agent-pane)
(defface agent-pane-role-assistant
  '((((class color) (background dark)) :foreground "#a78bfa" :weight semibold)
    (((class color) (background light)) :foreground "#6d28d9" :weight semibold)
    (t :inherit font-lock-keyword-face))
  "Face used for assistant role labels."
  :group 'agent-pane)
(defface agent-pane-role-system
  '((((class color) (background dark)) :foreground "#fca5a5" :weight semibold)
    (((class color) (background light)) :foreground "#b91c1c" :weight semibold)
    (t :inherit font-lock-warning-face))
  "Face used for system messages."
  :group 'agent-pane)
(defface agent-pane-role-thought
  '((((class color) (background dark)) :foreground "#86efac" :slant italic)
    (((class color) (background light)) :foreground "#15803d" :slant italic)
    (t :inherit font-lock-doc-face))
  "Face used for agent thought process messages."
  :group 'agent-pane)
(defface agent-pane-role-tool
  '((((class color) (background dark)) :foreground "#f9a8d4" :weight semibold)
    (((class color) (background light)) :foreground "#be185d" :weight semibold)
    (t :inherit font-lock-constant-face))
  "Face used for tool call messages."
  :group 'agent-pane)
(defface agent-pane-role-acp
  '((((class color) (background dark)) :foreground "#94a3b8")
    (((class color) (background light)) :foreground "#475569")
    (t :inherit shadow))
  "Face used for low-level ACP protocol events."
  :group 'agent-pane)
(defface agent-pane-tool-invocation-block
  '((((class color) (background dark)) :foreground "#f9a8d4" :background "#3b1d2e")
    (((class color) (background light)) :foreground "#9d174d" :background "#ffe4f1")
    (t :inherit default))
  "Face for concise tool invocation lines."
  :group 'agent-pane)
(defface agent-pane-tool-output-block
  '((((class color) (background dark)) :foreground "#e2e8f0" :background "#1f2937")
    (((class color) (background light)) :foreground "#0f172a" :background "#e2e8f0")
    (t :inherit default))
  "Face for collapsed tool output preview blocks."
  :group 'agent-pane)
(defface agent-pane-header-title
  '((((class color) (background dark)) :foreground "#e2e8f0" :weight bold)
    (((class color) (background light)) :foreground "#0f172a" :weight bold)
    (t :inherit bold))
  "Face for the first line of the ACP header."
  :group 'agent-pane)
(defface agent-pane-header-info
  '((((class color) (background dark)) :foreground "#94a3b8")
    (((class color) (background light)) :foreground "#334155")
    (t :inherit shadow))
  "Face for ACP header metadata rows."
  :group 'agent-pane)
(defface agent-pane-header-keys
  '((((class color) (background dark)) :foreground "#cbd5e1" :slant italic)
    (((class color) (background light)) :foreground "#334155" :slant italic)
    (t :inherit shadow))
  "Face for ACP header keybinding hints."
  :group 'agent-pane)
(defface agent-pane-separator
  '((((class color) (background dark)) :foreground "#334155")
    (((class color) (background light)) :foreground "#94a3b8")
    (t :inherit shadow))
  "Face for visual separators in the transcript view."
  :group 'agent-pane)
(defface agent-pane-status-ready
  '((((class color) (background dark)) :foreground "#86efac" :weight semibold)
    (((class color) (background light)) :foreground "#15803d" :weight semibold)
    (t :inherit success))
  "Face for ready status text."
  :group 'agent-pane)
(defface agent-pane-status-busy
  '((((class color) (background dark)) :foreground "#fcd34d" :weight semibold)
    (((class color) (background light)) :foreground "#b45309" :weight semibold)
    (t :inherit warning))
  "Face for waiting/streaming/cancelling status text."
  :group 'agent-pane)
(defface agent-pane-footer-hint
  '((((class color) (background dark)) :foreground "#94a3b8")
    (((class color) (background light)) :foreground "#475569")
    (t :inherit shadow))
  "Face for footer hint text above the input area."
  :group 'agent-pane)
(defface agent-pane-sessions-title
  '((((class color) (background dark)) :foreground "#e2e8f0" :weight bold)
    (((class color) (background light)) :foreground "#0f172a" :weight bold)
    (t :inherit bold))
  "Face for sessions sidebar title line."
  :group 'agent-pane)
(defface agent-pane-sessions-meta
  '((((class color) (background dark)) :foreground "#94a3b8")
    (((class color) (background light)) :foreground "#475569")
    (t :inherit shadow))
  "Face for sessions sidebar metadata and hint lines."
  :group 'agent-pane)
(defface agent-pane-sessions-project
  '((((class color) (background dark)) :foreground "#c4b5fd" :weight semibold)
    (((class color) (background light)) :foreground "#6d28d9" :weight semibold)
    (t :inherit font-lock-keyword-face))
  "Face for project group headings in the sessions sidebar."
  :group 'agent-pane)
(defface agent-pane-sessions-session
  '((((class color) (background dark)) :foreground "#e2e8f0")
    (((class color) (background light)) :foreground "#0f172a")
    (t :inherit default))
  "Face for session entries in the sessions sidebar."
  :group 'agent-pane)
(defface agent-pane-markdown-strong-assistant
  '((t :inherit font-lock-keyword-face :weight normal))
  "Face for markdown strong (e.g. **bold**) in assistant messages.
We intentionally do not render it as bold; instead we colorize it."
  :group 'agent-pane)
(defface agent-pane-markdown-strong-thought
  '((t :inherit bold))
  "Face for markdown strong (e.g. **bold**) in thought messages."
  :group 'agent-pane)
(defface agent-pane-markdown-code
  '((t :inherit fixed-pitch))
  "Face for markdown inline/fenced code."
  :group 'agent-pane)
(defface agent-pane-markdown-delimiter
  '((t :inherit shadow))
  "Face for markdown delimiters like ** and ` and code fences."
  :group 'agent-pane)
(defface agent-pane-markdown-heading
  '((t :inherit font-lock-function-name-face :weight semibold))
  "Face for markdown heading text."
  :group 'agent-pane)
(defface agent-pane-markdown-list-marker
  '((t :inherit shadow))
  "Face for markdown list markers like `-` and `*`."
  :group 'agent-pane)
(defface agent-pane-markdown-link-label
  '((t :inherit link))
  "Face for markdown link labels."
  :group 'agent-pane)
(defface agent-pane-markdown-table
  '((t :inherit fixed-pitch))
  "Face for rendered markdown tables."
  :group 'agent-pane)
(defcustom agent-pane-buffer-name "*agent-pane*"
  "Name of the main agent-pane buffer."
  :type 'string
  :group 'agent-pane)
(defcustom agent-pane-acp-provider 'codex
  "ACP provider profile used to launch the backend agent server.

`codex' uses `agent-pane-codex-*' settings.
`copilot' uses `agent-pane-copilot-*' settings.
`claude-code' uses `agent-pane-claude-code-*' settings.
`custom' uses `agent-pane-codex-command' directly for full manual control."
  :type '(choice (const :tag "Codex" codex)
                 (const :tag "GitHub Copilot CLI" copilot)
                 (const :tag "Claude Code" claude-code)
                 (const :tag "Custom command" custom))
  :group 'agent-pane)
(defcustom agent-pane-codex-command
  '("codex-acp")
  "Command and parameters for the Codex ACP server.
The first element is the command name, and the rest are command parameters."
  :type '(repeat string)
  :group 'agent-pane)
(defcustom agent-pane-copilot-command
  '("copilot" "--acp")
  "Command and parameters for the GitHub Copilot CLI ACP server."
  :type '(repeat string)
  :group 'agent-pane)
(defcustom agent-pane-claude-code-command
  '("claude-code-acp")
  "Command and parameters for the Claude Code ACP server."
  :type '(repeat string)
  :group 'agent-pane)
(defcustom agent-pane-codex-config-overrides
  '("model_reasoning_effort=\"high\""
    "sandbox_mode=\"danger-full-access\"")
  "Extra `codex-acp -c key=value` overrides to pass when launching.
These are appended to `agent-pane-codex-command' as repeated `-c` flags.
Notes:
- Codex typically reads defaults from `~/.codex/config.toml'.  These overrides
  are a way to make agent-pane's behavior explicit and reproducible.
- `sandbox_mode=\"danger-full-access\"' disables Codex's OS sandbox.  This is
  intentionally risky, but it can also be necessary on Windows where Codex's
  native sandboxing may be unavailable."
  :type '(repeat string)
  :group 'agent-pane)
(defcustom agent-pane-codex-environment
  nil
  "Environment variables (\"NAME=value\") for the Codex ACP server process."
  :type '(repeat string)
  :group 'agent-pane)
(defcustom agent-pane-copilot-environment
  nil
  "Environment variables (\"NAME=value\") for Copilot CLI ACP process."
  :type '(repeat string)
  :group 'agent-pane)
(defcustom agent-pane-claude-code-environment
  nil
  "Environment variables (\"NAME=value\") for Claude Code ACP process."
  :type '(repeat string)
  :group 'agent-pane)
(defcustom agent-pane-session-mode-id "bypassPermissions"
  "ACP session mode id to set after `session/new'.
Common values (if supported by the server) include:
- \"default\"
- \"plan\"
- \"acceptEdits\"
- \"bypassPermissions\"
When nil, agent-pane will not send `session/set_mode'."
  :type '(choice (const :tag "Do not set" nil) string)
  :group 'agent-pane)
(defcustom agent-pane-session-model-id nil
  "ACP model id to set after `session/new'.
When non-nil, agent-pane sends `session/set_model'.
When nil, the provider default model is used."
  :type '(choice (const :tag "Provider default" nil) string)
  :group 'agent-pane)
(defcustom agent-pane-permission-policy 'auto-allow
  "How to respond to `session/request_permission' requests.
`auto-allow' chooses an allow option automatically.
`auto-cancel' always cancels.
`prompt' asks interactively and supports storing allow rules."
  :type '(choice (const :tag "Auto allow" auto-allow)
                 (const :tag "Auto cancel" auto-cancel)
                 (const :tag "Prompt" prompt))
  :group 'agent-pane)
(defcustom agent-pane-permission-rules-file
  (expand-file-name "agent-pane/permission-rules.el" user-emacs-directory)
  "Path to persisted project-scoped permission rules.
Rules are used when `agent-pane-permission-policy' is `prompt'."
  :type 'file
  :group 'agent-pane)
(defcustom agent-pane-auth-method-id "chatgpt"
  "ACP authentication method id to request.

Set this to nil to skip `authenticate' for providers that do not require an
explicit ACP authentication request (for example Copilot CLI or Claude Code
in login-configured environments)."
  :type '(choice (const :tag "Skip authenticate request" nil) string)
  :group 'agent-pane)
(defcustom agent-pane-enable-acp t
  "When non-nil, connect to the selected ACP provider.
Batch tests and CI should not spawn external processes; in batch mode,
`agent-pane-submit' will skip ACP regardless of this value."
  :type 'boolean
  :group 'agent-pane)
(defcustom agent-pane-acp-client-maker nil
  "Optional function to construct an ACP client for the current buffer.
When non-nil, this function is called with one argument (BUFFER) and
must return an ACP client object compatible with `acp.el'.
This is primarily intended for deterministic integration tests using
fake clients (see `acp-fakes')."
  :type '(choice (const :tag "Default (spawn real ACP server)" nil)
                 function)
  :group 'agent-pane)
(defcustom agent-pane-enable-acp-logging t
  "When non-nil, enable `acp.el' logging/traffic buffers.
This sets `acp-logging-enabled' when agent-pane establishes a connection."
  :type 'boolean
  :group 'agent-pane)
(defcustom agent-pane-show-thoughts t
  "When non-nil, display `agent_thought_chunk' updates in the UI."
  :type 'boolean
  :group 'agent-pane)
(defcustom agent-pane-show-tool-calls t
  "When non-nil, display tool call updates (tool_call/tool_call_update)."
  :type 'boolean
  :group 'agent-pane)
(defcustom agent-pane-tool-output-preview-lines 5
  "How many trailing tool-output lines to render in tool messages."
  :type 'integer
  :group 'agent-pane)
(defcustom agent-pane-diff-viewer 'diff-mode
  "Viewer used when opening file-change diffs from agent-pane.
`diff-mode' opens a unified diff buffer.
`ediff' opens an Ediff session between before/after buffers."
  :type '(choice (const :tag "Unified diff buffer" diff-mode)
                 (const :tag "Ediff session" ediff))
  :group 'agent-pane)
(defcustom agent-pane-permission-review-diff t
  "When non-nil, permission prompts include a `Review diff in Emacs' option.
This is shown when the request contains structured file-change information."
  :type 'boolean
  :group 'agent-pane)
(defcustom agent-pane-show-acp-header t
  "When non-nil, show an ACP status/debug header at the top of the buffer."
  :type 'boolean
  :group 'agent-pane)
(defcustom agent-pane-header-details-collapsed t
  "When non-nil, ACP debug header details start collapsed.
The compact summary row still shows provider/model and a key hint for toggling
full details."
  :type 'boolean
  :group 'agent-pane)
(defcustom agent-pane-show-raw-events nil
  "When non-nil, append low-level ACP events to the transcript.
Even when nil, you can view the ACP traffic buffer via
`agent-pane-open-acp-traffic'."
  :type 'boolean
  :group 'agent-pane)
(defcustom agent-pane-markdown-hide-markup t
  "When non-nil, hide markdown markup characters in the UI.
For example, \"**bold**\" is displayed as just \"bold\" (styled), and the
\"**\" delimiters are made invisible.
This only affects rendering in the agent-pane buffer; transcript files still
contain the original markdown."
  :type 'boolean
  :group 'agent-pane)
(defcustom agent-pane-markdown-fontify-backtrack 200
  "How far to backtrack when refontifying markdown during streaming.
This helps handle cases where a markdown delimiter spans chunk boundaries,
for example: \"**bo\" + \"ld**\"."
  :type 'integer
  :group 'agent-pane)
(defcustom agent-pane-show-header-line-status t
  "When non-nil, show the agent-pane status in `header-line-format'.
This makes progress visible even when the window is scrolled away from the
footer."
  :type 'boolean
  :group 'agent-pane)
(defcustom agent-pane-save-transcripts 'interactive
  "Whether agent-pane should save chat transcripts to disk.
When set to `interactive', transcripts are only saved when Emacs is
interactive (that is, `noninteractive' is nil).  This avoids CI/test runs
writing to the user's filesystem.
When set to `always', transcripts are saved even in batch mode.
When nil, transcripts are not saved."
  :type '(choice (const :tag "Off" nil)
                 (const :tag "Interactive only" interactive)
                 (const :tag "Always" always))
  :group 'agent-pane)
(defcustom agent-pane-transcript-directory
  (expand-file-name "agent-pane/transcripts" user-emacs-directory)
  "Directory where agent-pane transcripts are written."
  :type 'directory
  :group 'agent-pane)
(defcustom agent-pane-transcript-include-thoughts nil
  "When non-nil, include thought blocks in transcripts."
  :type 'boolean
  :group 'agent-pane)
(defcustom agent-pane-use-sidebar t
  "When non-nil, `agent-pane' opens the sidebar app layout.
When nil, `agent-pane' opens only the chat buffer."
  :type 'boolean
  :group 'agent-pane)
(provide 'agent-pane-config)
;;; agent-pane-config.el ends here

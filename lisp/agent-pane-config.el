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

  '((t :inherit bold))

  "Face used for user role labels."

  :group 'agent-pane)

(defface agent-pane-role-assistant

  '((t :inherit font-lock-keyword-face))

  "Face used for assistant role labels."

  :group 'agent-pane)

(defface agent-pane-role-system

  '((t :inherit font-lock-warning-face))

  "Face used for system messages."

  :group 'agent-pane)

(defface agent-pane-role-thought

  '((t :inherit font-lock-doc-face))

  "Face used for agent thought process messages."

  :group 'agent-pane)

(defface agent-pane-role-tool

  '((t :inherit font-lock-constant-face))

  "Face used for tool call messages."

  :group 'agent-pane)

(defface agent-pane-role-acp

  '((t :inherit shadow))

  "Face used for low-level ACP protocol events."

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

(defcustom agent-pane-buffer-name "*agent-pane*"

  "Name of the main agent-pane buffer."

  :type 'string

  :group 'agent-pane)

(defcustom agent-pane-codex-command

  '("codex-acp")

  "Command and parameters for the Codex ACP server.

The first element is the command name, and the rest are command parameters."

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

(defcustom agent-pane-permission-policy 'auto-allow

  "How to respond to `session/request_permission' requests.

This is a temporary policy until a proper permissions UI is implemented."

  :type '(choice (const :tag "Auto allow" auto-allow)

                 (const :tag "Auto cancel" auto-cancel))

  :group 'agent-pane)

(defcustom agent-pane-auth-method-id "chatgpt"

  "ACP authentication method id to request.

For Codex login flows, agent-shell uses method id \"chatgpt\"."

  :type 'string

  :group 'agent-pane)

(defcustom agent-pane-enable-acp t

  "When non-nil, connect to Codex via ACP.

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

(defcustom agent-pane-show-acp-header t

  "When non-nil, show an ACP status/debug header at the top of the buffer."

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


#!/usr/bin/env bash
set -euo pipefail

if [[ -n "${EMACS:-}" ]]; then
  echo "$EMACS"
  exit 0
fi

if command -v emacs >/dev/null 2>&1; then
  command -v emacs
  exit 0
fi

# Common Windows install location (Git Bash path form).
candidate="$(ls -1d /c/Program\ Files/Emacs/emacs-*/bin/emacs.exe 2>/dev/null | sort -V | tail -n 1 || true)"
if [[ -n "$candidate" && -x "$candidate" ]]; then
  echo "$candidate"
  exit 0
fi

cat >&2 <<'EOF'
Could not find Emacs.

Set EMACS to the full path to emacs (or put emacs on PATH).
Example:
  EMACS="/c/Program Files/Emacs/emacs-30.2/bin/emacs.exe" ./scripts/test.sh
EOF
exit 1

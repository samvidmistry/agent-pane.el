#!/usr/bin/env bash
set -euo pipefail

# Normalize line endings and trailing whitespace for text files.
#
# - converts CRLF/CR to LF
# - trims trailing spaces/tabs
# - ensures a final newline
# - skips binaries (NUL byte detection)
# - preserves UTF-8 BOM when present
#
# Usage:
#   ./scripts/normalize.sh [--check] [--git] [--all] [PATH...]
#
# Flags:
#   --check  : don't write, exit 1 if any file would change
#   --git    : normalize files reported by git (tracked/modified)
#   --all    : normalize common text files under the repo (skips .git/ and vendor/)
#

usage() {
  cat <<'EOF'
Usage: ./scripts/normalize.sh [--check] [--git] [--all] [PATH...]

Normalize line endings + whitespace for text files:
- CRLF/CR -> LF
- trim trailing spaces/tabs
- ensure final newline

Modes:
  PATH... : normalize the given files/dirs
  --git   : normalize changed tracked files (git diff)
  --all   : normalize common text files under the repo

Exit codes:
  0 success
  1 (with --check) changes would be made
  2 usage / invalid args
EOF
}

CHECK=0
MODE="paths"

ARGS=()
for a in "$@"; do
  case "$a" in
    --check) CHECK=1 ;;
    --git) MODE="git" ;;
    --all) MODE="all" ;;
    -h|--help) usage; exit 0 ;;
    *) ARGS+=("$a") ;;
  esac
done

PYTHON=${PYTHON:-python}
if ! command -v "$PYTHON" >/dev/null 2>&1; then
  if command -v python3 >/dev/null 2>&1; then
    PYTHON=python3
  else
    echo "normalize.sh: python not found (set PYTHON=/path/to/python)" >&2
    exit 2
  fi
fi

# Determine repo root if possible.
REPO_ROOT="."
if command -v git >/dev/null 2>&1; then
  if git rev-parse --show-toplevel >/dev/null 2>&1; then
    REPO_ROOT=$(git rev-parse --show-toplevel)
  fi
fi

# Build file list for modes.
FILES=()
if [[ "$MODE" == "git" ]]; then
  if ! command -v git >/dev/null 2>&1; then
    echo "normalize.sh: --git requires git" >&2
    exit 2
  fi
  mapfile -t FILES < <(git -C "$REPO_ROOT" diff --name-only --diff-filter=ACMRTUXB)
elif [[ "$MODE" == "all" ]]; then
  # Common text extensions in this repo.
  mapfile -t FILES < <(
    cd "$REPO_ROOT" && \
    find . -type f \
      -not -path './.git/*' \
      -not -path './vendor/*' \
      -not -name '*.elc' \
      \( \
        -name '*.el' -o -name '*.md' -o -name '*.sh' -o -name '*.ps1' -o \
        -name '*.yml' -o -name '*.yaml' -o -name '*.json' -o -name '*.toml' -o \
        -name '*.txt' -o -name '*.traffic' -o -name '*.editorconfig' -o -name '*.gitignore' \
      \) \
      -print | sed 's#^\./##'
  )
else
  if [[ ${#ARGS[@]} -eq 0 ]]; then
    usage >&2
    exit 2
  fi
  FILES=("${ARGS[@]}")
fi

# Normalize.
"$PYTHON" - "$REPO_ROOT" "$CHECK" "${FILES[@]}" <<'PY'
import os
import re
import sys
from pathlib import Path

repo_root = Path(sys.argv[1]).resolve()
check = (sys.argv[2] == "1")
paths = [Path(p) for p in sys.argv[3:]]

# If a directory is provided, walk it.
def iter_paths(ps):
    for p in ps:
        p = (repo_root / p) if not p.is_absolute() else p
        if not p.exists():
            print(f"normalize: missing: {p}", file=sys.stderr)
            continue
        if p.is_dir():
            for root, dirs, files in os.walk(p):
                rootp = Path(root)
                # Skip .git and vendor by default.
                parts = set(rootp.parts)
                if ".git" in parts or "vendor" in parts:
                    dirs[:] = []
                    continue
                for f in files:
                    yield rootp / f
        else:
            yield p

# Only touch common text extensions by default.
ALLOWED_EXT = {
    ".el", ".md", ".sh", ".ps1", ".yml", ".yaml", ".json", ".toml", ".txt", ".traffic",
}
ALLOWED_NAMES = {".editorconfig", ".gitignore"}

changed = []
processed = 0

for p in iter_paths(paths):
    name = p.name
    if name.endswith('.elc'):
        continue
    if name not in ALLOWED_NAMES and p.suffix not in ALLOWED_EXT:
        continue

    try:
        data = p.read_bytes()
    except OSError as e:
        print(f"normalize: unreadable: {p}: {e}", file=sys.stderr)
        continue

    # Skip binary-ish files (NUL byte).
    if b"\x00" in data:
        continue

    has_bom = data.startswith(b"\xef\xbb\xbf")
    if has_bom:
        data0 = data[3:]
    else:
        data0 = data

    # Preserve bytes via surrogateescape.
    s = data0.decode("utf-8", errors="surrogateescape")

    # Normalize newlines.
    s = s.replace("\r\n", "\n").replace("\r", "\n")

    # Trim trailing spaces/tabs on each line.
    lines = s.split("\n")
    lines = [re.sub(r"[\t ]+$", "", ln) for ln in lines]
    out = "\n".join(lines)

    # Ensure final newline.
    if not out.endswith("\n"):
        out += "\n"

    out_bytes = out.encode("utf-8", errors="surrogateescape")
    if has_bom:
        out_bytes = b"\xef\xbb\xbf" + out_bytes

    if out_bytes != data:
        changed.append(str(p))
        if not check:
            try:
                p.write_bytes(out_bytes)
            except OSError as e:
                print(f"normalize: write failed: {p}: {e}", file=sys.stderr)
                continue

    processed += 1

if check:
    for p in changed:
        print(p)
    if changed:
        sys.exit(1)

PY

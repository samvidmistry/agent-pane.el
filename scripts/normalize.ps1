# Normalize line endings and trailing whitespace for text files.
#
# Usage:
#   .\scripts\normalize.ps1 [--check] [--git] [--all] [PATH...]
#
# Flags:
#   --check : don't write, exit 1 if any file would change
#   --git   : normalize files reported by git (tracked/modified)
#   --all   : normalize common text files under the repo (skips .git\ and vendor\)
#

$ErrorActionPreference = "Stop"

$Root = (Resolve-Path (Join-Path $PSScriptRoot "..")).Path

# Parse arguments manually to support both -flag and --flag conventions.
$doCheck = $false
$doGit   = $false
$doAll   = $false
$Paths   = @()

foreach ($a in $args) {
  switch -regex ($a) {
    '^--?check$' { $doCheck = $true }
    '^--?git$'   { $doGit   = $true }
    '^--?all$'   { $doAll   = $true }
    '^--?h(elp)?$' {
      Write-Host @"
Usage: .\scripts\normalize.ps1 [--check] [--git] [--all] [PATH...]

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
"@
      exit 0
    }
    default { $Paths += $a }
  }
}

# Find Python.
$Python = if ($env:PYTHON) { $env:PYTHON }
           elseif (Get-Command python -ErrorAction SilentlyContinue) { "python" }
           elseif (Get-Command python3 -ErrorAction SilentlyContinue) { "python3" }
           else { throw "normalize.ps1: python not found (set `$env:PYTHON)" }

# Build file list.
$FileList = @()

if ($doGit) {
  $FileList = git -C $Root diff --name-only --diff-filter=ACMRTUXB
  if (-not $FileList) { $FileList = @() }
} elseif ($doAll) {
  $allowedExts = @(".el",".md",".sh",".ps1",".yml",".yaml",".json",".toml",".txt",".traffic")
  $allowedNames = @(".editorconfig",".gitignore")
  $FileList = Get-ChildItem -Path $Root -Recurse -File |
    Where-Object {
      $rel = $_.FullName.Substring($Root.Length + 1)
      -not ($rel.StartsWith(".git\") -or $rel.StartsWith("vendor\")) -and
      $_.Extension -ne ".elc" -and
      ($allowedExts -contains $_.Extension -or $allowedNames -contains $_.Name)
    } |
    ForEach-Object { $_.FullName.Substring($Root.Length + 1) }
  if (-not $FileList) { $FileList = @() }
} else {
  if ($Paths.Count -eq 0) {
    Write-Host "Usage: .\scripts\normalize.ps1 [--check] [--git] [--all] [PATH...]"
    exit 2
  }
  $FileList = $Paths
}

if ($FileList.Count -eq 0) {
  exit 0
}

$checkVal = if ($doCheck) { "1" } else { "0" }

# Inline Python script (same logic as normalize.sh).
$pyScript = @'
import os
import re
import sys
from pathlib import Path

repo_root = Path(sys.argv[1]).resolve()
check = (sys.argv[2] == "1")
paths = [Path(p) for p in sys.argv[3:]]

def iter_paths(ps):
    for p in ps:
        p = (repo_root / p) if not p.is_absolute() else p
        if not p.exists():
            print(f"normalize: missing: {p}", file=sys.stderr)
            continue
        if p.is_dir():
            for root, dirs, files in os.walk(p):
                rootp = Path(root)
                parts = set(rootp.parts)
                if ".git" in parts or "vendor" in parts:
                    dirs[:] = []
                    continue
                for f in files:
                    yield rootp / f
        else:
            yield p

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

    if b"\x00" in data:
        continue

    has_bom = data.startswith(b"\xef\xbb\xbf")
    if has_bom:
        data0 = data[3:]
    else:
        data0 = data

    s = data0.decode("utf-8", errors="surrogateescape")
    s = s.replace("\r\n", "\n").replace("\r", "\n")

    lines = s.split("\n")
    lines = [re.sub(r"[\t ]+$", "", ln) for ln in lines]
    out = "\n".join(lines)

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
'@

# Write Python script to a temp file to avoid quoting issues.
$tmpPy = [System.IO.Path]::GetTempFileName() -replace '\.tmp$', '.py'
[System.IO.File]::WriteAllText($tmpPy, $pyScript)

try {
  $pyArgs = @($tmpPy, $Root, $checkVal) + $FileList
  & $Python @pyArgs
  if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
} finally {
  Remove-Item $tmpPy -Force -ErrorAction SilentlyContinue
}

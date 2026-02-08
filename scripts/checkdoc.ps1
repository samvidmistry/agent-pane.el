Param(
  [string]$RepoRoot = (Resolve-Path (Join-Path $PSScriptRoot "..")).Path
)

$ErrorActionPreference = "Stop"

if (-not (Get-Command bash -ErrorAction SilentlyContinue)) {
  throw "bash not found on PATH. Install Git for Windows (Git Bash) or add bash to PATH."
}

Push-Location $RepoRoot
try {
  bash -lc "./scripts/checkdoc.sh"
} finally {
  Pop-Location
}


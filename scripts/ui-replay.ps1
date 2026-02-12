Param(
  [string]$RepoRoot = (Resolve-Path (Join-Path $PSScriptRoot "..")).Path,
  [string]$TrafficFile = "",
  [string]$OutputFile = "",
  [string]$PromptText = ""
)

$ErrorActionPreference = "Stop"

if (-not (Get-Command bash -ErrorAction SilentlyContinue)) {
  throw "bash not found on PATH. Install Git for Windows (Git Bash) or add bash to PATH."
}

if ([string]::IsNullOrWhiteSpace($TrafficFile)) {
  $TrafficFile = "./test/traffic/agent-pane-interleaved.traffic"
}
if ([string]::IsNullOrWhiteSpace($OutputFile)) {
  $OutputFile = "./test/artifacts/agent-pane-ui-replay.txt"
}
if ([string]::IsNullOrWhiteSpace($PromptText)) {
  $PromptText = "hi"
}

Push-Location $RepoRoot
try {
  bash -lc "./scripts/ui-replay.sh '$TrafficFile' '$OutputFile' '$PromptText'"
} finally {
  Pop-Location
}

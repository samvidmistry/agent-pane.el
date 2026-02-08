$ErrorActionPreference = "Stop"

$Root = (Resolve-Path (Join-Path $PSScriptRoot ".."))
$Emacs = & (Join-Path $Root "scripts/_emacs.sh")

& $Emacs -Q --batch -l (Join-Path $Root "scripts/check-parens.el")
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }

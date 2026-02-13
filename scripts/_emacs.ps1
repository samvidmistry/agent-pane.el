# Helper: locate Emacs executable.
# Returns the path to emacs.exe.  Checks $env:EMACS, PATH, then common
# Windows install locations.

$ErrorActionPreference = "Stop"

function Find-Emacs {
  if ($env:EMACS) {
    return $env:EMACS
  }

  $cmd = Get-Command emacs -ErrorAction SilentlyContinue
  if ($cmd) {
    return $cmd.Source
  }

  # Common Windows install location.
  $candidates = Get-ChildItem "C:\Program Files\Emacs\emacs-*\bin\emacs.exe" -ErrorAction SilentlyContinue |
    Sort-Object FullName
  if ($candidates) {
    return ($candidates | Select-Object -Last 1).FullName
  }

  throw @"
Could not find Emacs.

Set `$env:EMACS to the full path to emacs.exe (or put emacs on PATH).
Example:
  `$env:EMACS = "C:\Program Files\Emacs\emacs-30.2\bin\emacs.exe"
  .\scripts\test.ps1
"@
}

Find-Emacs

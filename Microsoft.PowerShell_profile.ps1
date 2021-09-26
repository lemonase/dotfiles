#--------
# Imports
#--------
# Posh
Import-Module posh-git

# Chocolatey
$ChocolateyProfile = "$env:ChocolateyInstall\helpers\chocolateyProfile.psm1"
if (Test-Path($ChocolateyProfile)) {
  Import-Module "$ChocolateyProfile"
}
# PSReadLine
Set-PSReadLineOption -EditMode Emacs
Set-PSReadLineOption -BellStyle Visual

# PSFzf
Set-PsFzfOption -PSReadlineChordProvider 'Ctrl+t' -PSReadlineChordReverseHistory 'Ctrl+r'

#----------------------
# Environment Varaibles
#----------------------
$EDITOR = "code"

#--------
# Aliases
#--------
New-Alias so "Select-Object"
New-Alias wo "Where-Object"

New-Alias venvac ".\venv\Scripts\Activate.ps1"

#----------
# Functions
#----------
function ep { Start-Process "$EDITOR" "$PROFILE" }

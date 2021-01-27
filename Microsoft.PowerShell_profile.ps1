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
$S = "$env:USERPROFILE\src"
$GOWD = "$env:USERPROFILE\go\src\github.com\lemonase"
$GODOTWD = "$env:USERPROFILE\Google Drive\Game Dev"
$EDITOR = "gvim"

#--------
# Aliases
#--------
New-Alias so "Select-Object"
New-Alias wo "Where-Object"

New-Alias venvac ".\venv\Scripts\Activate.ps1"

#----------
# Functions
#----------
function wd { Set-Location $WD }
function gowd { Set-Location $GOWD }
function godotwd { Set-Location $GODOTWD }

function ep { Start-Process "$EDITOR" "$PROFILE" }

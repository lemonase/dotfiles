# .bashrc

# This file is kept under git version control

# To make any local (non-tracked) edits or configurations, 
# please edit `~/.local/.bashrc`, which is sourced from this file.

# If not running interactively, don't do anything
case $- in
  *i*) ;;
  *) return;;
esac

# environment variables for commands
export PAGER="less"
export EDITOR="/usr/bin/vim"
export VISUAL="/usr/bin/vim"

# bash history options
export HISTSIZE= ;
export HISTFILESIZE= ;
export HISTCONTROL="ignoreboth:erasedups"
export HISTTIMEFORMAT="%F %T  "

# shell options
shopt -s checkhash
shopt -s cdspell
shopt -s dirspell

# ls options
ls --version &> /dev/null
if [ "$? " -eq 0 ]; then
  LS_OPTS="--color=auto --group-directories-first -F"
else
  LS_OPTS="-GF"
  export CLICOLOR=1
fi

#+* aliases *+#
# core utils (ls, grep, tree)
alias l="ls ${LS_OPTS}"
alias ls="ls ${LS_OPTS}"
alias ll="ls -lsh ${LS_OPTS}"
alias la="ls -Alsh ${LS_OPTS}"
alias al="ls -A ${LS_OPTS}"
alias sl="ls -lsSh ${LS_OPTS}"
alias sal="ls -AlsSh ${LS_OPTS}"
alias grep="grep ${GREP_OPTS}"
alias fgrep="fgrep ${GREP_OPTS}"
alias egrep="egrep ${GREP_OPTS}"
alias treel="tree -C | less -R"
alias lsmnt="mount | column -t"
alias pathls='printf "%b\n" "${PATH//:/\\n}"'

# (git - vim - tmux)
alias g="git"
alias groot="cd $(git rev-parse --show-toplevel 2> /dev/null || echo -n ".")"
alias tmls="tmux ls"
alias tmlsc="tmux lsc"
alias tmks="tmux kill-session -t" # kill one session
alias tmka="tmux kill-server" # aka killall

# python
# python3 is python unless python is already python3
if ! command -v python &> /dev/null && command -v python3 &> /dev/null;  then
  alias python="python3"
  alias py="python3"
  alias ipy="ipython3"
fi
alias venvac="source venv/bin/activate"
alias mkvenv="python -m venv venv"

# compression/archives
alias untar="tar -xvf"
alias mktar="tar -caf"
alias tarls="tar -tvf"
alias ungzip="gunzip"

# color vars using tput or ANSI/VT100 Control sequences
# check if tput is available
if [ -x "$(command -v tput)" ]; then
  num_colors=$(tput colors)
  if [ -n "$num_colors" ]; then
    if [ "$num_colors" -ge 8 ]; then
      black="\[$(tput setaf 0)\]"; unesc_black="$(tput setaf 0)"
      red="\[$(tput setaf 1)\]"; unesc_red="$(tput setaf 1)"
      green="\[$(tput setaf 2)\]"; unesc_green="$(tput setaf 2)"
      brown="\[$(tput setaf 3)\]"; unesc_brown="$(tput setaf 3)"
      blue="\[$(tput setaf 4)\]"; unesc_blue="$(tput setaf 4)"
      purple="\[$(tput setaf 5)\]"; unesc_purple="$(tput setaf 5)"
      cyan="\[$(tput setaf 6)\]"; unesc_cyan="$(tput setaf 6)"
      grey="\[$(tput setaf 7)\]"; unesc_grey="$(tput setaf 7)"
      dark_grey="\[$(tput setaf 8)\]"; unesc_dark_grey="$(tput setaf 8)"
    fi
    if [ "$num_colors" -ge 16 ]; then
      bright_red="\[$(tput setaf 9)\]"; unesc_bright_red="$(tput setaf 9)"
      bright_green="\[$(tput setaf 10)\]"; unesc_bright_green="$(tput setaf 10)"
      bright_yellow="\[$(tput setaf 11)\]"; unesc_bright_yellow="$(tput setaf 11)"
      bright_blue="\[$(tput setaf 12)\]"; unesc_bright_blue="$(tput setaf 12)"
      bright_magenta="\[$(tput setaf 13)\]"; unesc_bright_magenta="$(tput setaf 13)"
      bright_cyan="\[$(tput setaf 14)\]"; unesc_bright_cyan="$(tput setaf 14)"
      white="\[$(tput setaf 15)\]"; unesc_bright_white="$(tput setaf 15)"
    fi
    reset="\[$(tput sgr0)\]"; unesc_reset="$(tput sgr0)"
    bold="\[$(tput bold)\]"; unesc_bold="$(tput bold)"
  fi
else # or fallback to ANSI esacpe codes
  black="\[\033[0;30m\]"; unesc_black="\033[0;30m"
  red="\[\033[1;31m\]"; unesc_red="\033[1;31m"
  green="\[\033[1;32m\]"; unesc_green="\033[1;32m"
  brown="\[\033[1;33m\]"; unesc_brown="\033[1;33m"
  blue="\[\033[1;34m\]"; unesc_blue="\033[1;34m"
  purple="\[\033[1;35m\]"; unesc_purple="\033[1;35m"
  cyan="\[\033[1;36m\]"; unesc_cyan="\033[1;36m"
  grey="\[\033[1;37m\]"; unesc_grey="\033[1;37m"
  dark_grey="\[\033[1;90m\]"; unesc_dark_grey="\033[1;90m"
  bright_red="\[\033[1;91m\]"; unesc_bright_red="\033[1;91m"
  bright_green="\[\033[1;92m\]"; unesc_bright_yellow="\033[1;92m"
  bright_yellow="\[\033[1;93m\]"; unesc_bright_yellow="\033[1;93m"
  bright_blue="\[\033[1;94m\]"; unesc_bright_blue="\033[1;94m"
  bright_magenta="\[\033[1;95m\]"; unesc_bright_magenta="\033[1;95m"
  bright_cyan="\[\033[1;96m\]"; unesc_bright_cyan="\033[1;96m"
  white="\[\033[1;97m\]"; unesc_white="\033[1;97m"
  reset="\[\033[0m\]"; unesc_reset="\033[0m"
  bold="\[\033[1m\]"; unesc_bold="\033[1m"
fi

#+* bash functions *+#

# concat common commands
mkcd() { mkdir -p -- "$1" && cd "$1"; }
cdd() { [ -n "$1" ] && for i in $(seq 1 "$1"); do cd ..; done; }
touchx() { touch "$@" && chmod +x "$@"; }

# git
lazygit() {
  git commit -a -m "$*" && git push;
}
lg() {
  lazygit "$*";
}

# tmux
tm() {
  if [ "$#" -gt 0 ]; then
    tmux new-session -As "$1"
  else
    tmux new-session
  fi
}
tma() {
  if [ "$#" -gt 0 ]; then
    tmux attach-session -d -t "$1"
    if [ "$?" -ne 0 ]; then
      tmux new-session -As "$1"
    fi
  else
    tmux attach
  fi
}

# path helpers
appendpath () {
  [[ ":$PATH:" != *":$1:"* ]] && PATH="${PATH}:$1"
}
prependpath() {
  [[ ":$PATH:" != *":$1:"* ]] && PATH="$1:${PATH}"
}

# misc functions
colordump(){
  for i in $(seq 0 255); do printf "$(tput setaf $i)$i "; done
}

# "smart" extract function
extract() {
  if [ -z "$1" ]; then
    # display usage if no parameters given
    echo "Usage: extract <path/file_name>.<zip|rar|bz2|gz|tar|tbz2|tgz|Z|7z|xz|ex|tar.bz2|tar.gz|tar.xz>"
    echo "       extract <path/file_name_1.ext> [path/file_name_2.ext] [path/file_name_3.ext]"
    return 1
  else
    for n in "$@"; do
      if [ -f "$n" ] ; then
        case "${n%,}" in
          *.tar.bz2|*.tar.gz|*.tar.xz|*.tbz2|*.tgz|*.txz|*.tar)
            tar xvf "$n"       ;;
          *.lzma)      unlzma ./"$n"      ;;
          *.bz2)       bunzip2 ./"$n"     ;;
          *.rar)       unrar x -ad ./"$n" ;;
          *.gz)        gunzip ./"$n"      ;;
          *.zip)       unzip ./"$n"       ;;
          *.z)         uncompress ./"$n"  ;;
          *.7z|*.arj|*.cab|*.chm|*.deb|*.dmg|*.iso|*.lzh|*.msi|*.rpm|*.udf|*.wim|*.xar)
            7z x ./"$n"        ;;
          *.xz)        unxz ./"$n"        ;;
          *.exe)       cabextract ./"$n"  ;;
          *)
            echo "extract: '$n' - unknown archive method"
            return 1
            ;;
        esac
      else
        echo "'$n' - file does not exist"
        return 1
      fi
    done
  fi
}

bkup() {
  if [ -f "$1" ]; then
    cp "${1}" "${1}.bkup.$(date +'%F.%R')";
  fi
}

datauri() {
  local mimeType=""
  if [ -f "$1" ]; then
    mimeType=$(file -b --mime-type "$1")

    if [[ $mimeType == text/* ]]; then
      mimeType="$mimeType;charset=utf-8"
    fi

    printf "data:%s;base64,%s" \
      "$mimeType" \
      "$(openssl base64 -in "$1" | tr -d "\n")"
        else
          printf "%s is not a file.\n" "$1"
  fi
}

# grep with color into less
grepless(){
  grep -ir --color=always "$*" --exclude-dir=".git" --exclude-dir="node_modules" . | less -RX
}

# curl shortcuts
cheatsh() {
  curl cheat.sh/"$1"
}
watip() {
  curl ifconfig.co
  # dig +short myip.opendns.com @resolver1.opendns.com
}

#+* prompt settings (PS1) *+#

# git prompt function
parse_git() {
  BRANCH="$(git rev-parse --abbrev-ref HEAD 2> /dev/null)"
  STATUS="$(git status 2> /dev/null)"

  if [[ "$?" -ne 0 ]]; then
    return
  else
    printf "\001${unesc_reset}${unesc_bold}\002:(%s)" "${BRANCH}"
    printf "\001${unesc_white}\002%s" "["
    if echo "${STATUS}" | grep -c "nothing to commit" &> /dev/null; then printf "\001${unesc_bright_blue}\002%s" "="; fi
    if echo "${STATUS}" | grep -c "renamed:"  &> /dev/null; then printf "\001${unesc_red}\002%s" "%"; fi
    if echo "${STATUS}" | grep -c "deleted:"  &> /dev/null; then printf "\001${unesc_red}\002%s" "-"; fi
    if echo "${STATUS}" | grep -c "new file:" &> /dev/null; then printf "\001${unesc_green}\002%s" "+"; fi
    if echo "${STATUS}" | grep -c "branch is ahead:" &> /dev/null; then printf "\001${unesc_bright_yellow}\002%s" ">"; fi
    if echo "${STATUS}" | grep -c "branch is behind" &> /dev/null; then printf "\001${unesc_bright_yellow}\002%s" "<"; fi
    if echo "${STATUS}" | grep -c "Untracked files:" &> /dev/null; then printf "\001${unesc_bright_yellow}\002%s" "?"; fi
    if echo "${STATUS}" | grep -c "modified:"        &> /dev/null; then printf "\001${unesc_bright_yellow}\002%s" "*"; fi
    printf "\001${unesc_reset}${unesc_bold}${unesc_white}\002%s" "]"
  fi
}

# *plain prompts*
# PS1="\W \\$ "
# PS1="[\u@\h:\W]\\$ "
# PS1="\u@\h:\W\\$ "

# *color prompts*
# PS1="${bold}${bright_blue}\w ${white}\\$ ${reset}"
# PS1="${bold}${bright_cyan}\u${bright_magenta}@${bright_yellow}\h${white}:${bright_blue}\w${white}\\$ ${reset}"

# *git color prompts*
# PS1="${bold}${bright_blue}\w\$(parse_git)${white} \\$ ${reset}"
# PS1="${bold}${white}\t ${bright_blue}\w\$(parse_git) ${white}\\$ ${reset}"
PS1="${bold}${bright_cyan}\u${bright_magenta}@${bright_yellow}\h${white}:${bright_blue}\w\$(parse_git)${white}\\$ ${reset}"

##* bash completions and integrations *##

# bash autocompletion
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    source /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    source /etc/bash_completion
  fi
fi

# Add tab completion for SSH hostnames based on ~/.ssh/config, ignoring wildcards
[ -e "$HOME/.ssh/config" ] && complete -o "default" -o "nospace" -W "$(grep "^Host" ~/.ssh/config | grep -v "[?*]" | cut -d " " -f2- | tr ' ' '\n')" scp sftp ssh;

# fzf shell integration
export FZF_DEFAULT_OPTS="--bind=ctrl-f:page-down,ctrl-b:page-up"
[ -f "~/.config/.fzf.bash" ] && source "~/.config/.fzf.bash"

#+* paths *+#
# language version managers
# ruby (rbenv)
src_rbenv(){
  if command -v ruby &> /dev/null && command -v gem > /dev/null; then
    appendpath "$(ruby -r rubygems -e 'puts Gem.user_dir')/bin"
    # rbenv shim
    if [ -d "$HOME/.rbenv/bin" ]; then
      appendpath "$HOME/.rbenv/bin"
      [[ ":$PATH:" != *":$HOME/.rbenv/shims:"* ]] && eval "$(rbenv init -)"
    fi
  fi
}

# node (nvm)
src_nvm(){
  if [ -d "$HOME/.nvm" ]; then
    export NVM_DIR="$HOME/.nvm"
    [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
    [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"
  fi
}

# python3 (pyenv)
src_pyenv() {
  if [ -d "$HOME/.pyenv" ]; then
    export PYENV_ROOT="$HOME/.pyenv"
    appendpath "$PYENV_ROOT/bin"
    command -v pyenv &> /dev/null && eval "$(pyenv init -)"
  fi
}

# go
[ -d "/usr/local/go/bin" ] && appendpath "/usr/local/go/bin"
[ -d "$HOME/go/bin" ] && appendpath "$HOME/go/bin"

# rust
[ -d "$HOME/.cargo" ] && appendpath "$HOME/.cargo/bin"

#+* OS specific config *+#
case "$OSTYPE" in
  darwin*)  
    # homebrew
    [ -d "/opt/homebrew/bin" ] && appendpath "/opt/homebrew/bin"
    # python3 (macOS)
    LATEST_PY=$(find "$HOME/Library/Python" -type d -maxdepth 1 | sort -V | tail -n 1)
    [ -d "$LATEST_PY/bin" ] && appendpath "$LATEST_PY/bin"
    # gui things
    alias o="open"
  ;;
  linux*)
    # linux gui things
    alias xo="xdg-open"
    alias firefox-temp='firefox --profile $(mktemp -d) &> /dev/null &'
  ;;
esac

# local bins
[ -d "$HOME/.local/bin" ] && appendpath "$HOME/.local/bin"
[ -d "$HOME/.local/scripts" ] && appendpath "$HOME/.local/scripts"

# local rc
[ -r "$HOME/.local/.bashrc" ] && source "$HOME/.local/.bashrc"

# vim:ft=sh

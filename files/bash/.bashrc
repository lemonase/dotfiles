# If not running interactively, don't do anything
case $- in
  *i*) ;;
  *) return;;
esac

# history
HISTSIZE= ;
HISTFILESIZE=
HISTCONTROL="ignoreboth:erasedups"
HISTTIMEFORMAT="%F %T  "

# shell options
shopt -s checkhash checkjobs checkwinsize
shopt -s dirspell extglob globstar
shopt -s cmdhist histappend

# command variables
export EDITOR="/usr/bin/vim"
export VISUAL="/usr/bin/vim"
export PAGER="less"

# command options
LS_OPTS="-F --color=auto"
GREP_OPTS="--color=auto"

## aliases ##

alias l="ls ${LS_OPTS}"
alias ls="ls ${LS_OPTS}"
alias ll="ls -lsh ${LS_OPTS}"
alias la="ls -Alsh ${LS_OPTS}"
alias al="ls -A ${LS_OPTS}"
alias sl="ls -lsSh ${LS_OPTS}"
alias sal="ls -AlsSh ${LS_OPTS}"
alias mkdir="mkdir -p"

alias grep="grep ${GREP_OPTS}"
alias fgrep="fgrep ${GREP_OPTS}"
alias egrep="egrep ${GREP_OPTS}"

alias tree="tree -C"
alias treel="tree -C | less -R"
alias lsmnt="mount | column -t"
alias df="df -h"

alias g="git"
alias groot="cd $(git rev-parse --show-toplevel 2> /dev/null || echo -n ".")"

alias tmls="tmux ls"
alias tmlsc="tmux lsc"
alias tmks="tmux kill-session -t" # kill one session
alias tmka="tmux kill-server" # aka killall

alias s="systemctl"
alias sud="sudo su"

if [ "$(grep -Ei 'debian|buntu|mint' /etc/*release)" ]; then
  alias a="sudo apt"
  alias aup="sudo apt update"
  alias aupg="sudo apt upgrade -y"
  alias alu="apt list --upgradable"
fi

alias vi="$EDITOR"
alias imv="$EDITOR"
alias ivm="$EDITOR"
alias vmi="$EDITOR"

alias venvac="source venv/bin/activate"

## colors ##

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

## functions ##

# common commands improved
mkcd() { mkdir -p -- "$1" && cd "$1"; }
cdd() { [ -n "$1" ] && for i in $(seq 1 "$1"); do cd ..; done; }
touchx() { touch "$@" && chmod +x "$@"; }

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

# checks if directory exists and path duplication
appendpath () {
  if [ -d "$1" ]; then
    case ":$PATH:" in
      *:"$1":*)
        ;;
      *)
        PATH="${PATH:+$PATH:}$1"
    esac
  fi
}

# easy backup
bkup() {
  if [ -f "$1" ]; then
    cp "${1}" "${1}.bkup.$(date +'%F.%R')";
  fi
}

# git
lazygit() {
  git commit -a -m "$*" && git push;
}

lg() {
  lazygit "$*";
}

## prompt stuff ##

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
# PS1="${bold}${blue}\W ${yellow}\\$ ${reset}"
# PS1="${bold}${purple}\u${yellow}@${cyan}\h${white}:${blue}\W ${yellow}\\$ ${reset}"

# *git color prompts*
# PS1="${bold}${blue}\W\$(parse_git)${green} \\$ ${reset}"
# PS1="${bold}${white}\t ${blue}\W\$(parse_git) ${cyan}\\$ ${reset}"
# PS1="${bold}${purple}\u${yellow}@${cyan}\h${white}:${blue}\W\$(parse_git)${green} \\$ ${reset}"
# PS1="${bold}\n${cyan}\u ${white}at ${yellow}\h ${white}in ${blue}\w ${white}on \$(parse_git)\n${yellow}\\$ ${reset}"

# bash autocompletion
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    source /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    source /etc/bash_completion
  fi
fi

## extra paths ##

# ruby
if command -v ruby > /dev/null && command -v gem > /dev/null; then
  appendpath "$(ruby -r rubygems -e 'puts Gem.user_dir')/bin"

  # rbenv shim
  if [ -d "$HOME/.rbenv/bin" ]; then
    appendpath "$HOME/.rbenv/bin"
    [[ ":$PATH:" != *":$HOME/.rbenv/shims:"* ]] && eval "$(rbenv init -)"
  fi
fi

# go
if command -v go > /dev/null; then
  [ -d "$HOME/go" ] && mv "$HOME/go" "$HOME/.go"
  export GOPATH="$HOME/.go"
  export GOWD="$GOPATH/src/github.com/lemonase"
  export GOMOD="$GOPATH/pkg/mod/github.com/lemonase/"
  appendpath "$(go env GOPATH)/bin"
fi

# rust
if command -v cargo > /dev/null; then
  appendpath "$HOME/.cargo/bin"
fi

# local bins
appendpath "$HOME/.local/bin"
appendpath "$HOME/.local/scripts"

## extra tools ##

export FZF_DEFAULT_OPTS="--bind=ctrl-f:page-down,ctrl-b:page-up"

# local rc ##

[ -r "$HOME/.config/bashrc" ] && source "$HOME/.config/bashrc"
[ -r "$HOME/.local/bashrc" ] && source "$HOME/.local/bashrc"

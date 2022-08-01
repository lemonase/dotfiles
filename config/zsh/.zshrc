## Variables ##
#
# Set some variables
# ls and grep options
ls --version &>/dev/null
if [ $? -eq 0 ]; then
  LS_OPTS="--color --group-directories-first -F"
else
  LS_OPTS="-GF"
  export CLICOLOR=1
fi
GREP_OPTS="--color=auto"
EDITOR=vim
VISUAL=vim
PAGER=less
# PROMPT='%n@%m %~ %# '

## Shell Options ##

# History
setopt append_history
setopt extended_history
setopt inc_append_history
setopt share_history

# Less Annoying
setopt interactive_comments
unsetopt correct_all
export BLOCK_SIZE="'1"

# Tab Completion
autoload -Uz compinit && compinit
setopt auto_menu

# Prompt
setopt prompt_subst

# Deduplicate path
typeset -U path

# Set 'emacs' keybinds
bindkey -e

## Aliases ##

# core utils
alias l="ls ${LS_OPTS}"
alias ls="ls ${LS_OPTS}"
alias ll="ls -lsh ${LS_OPTS}"
alias la="ls -alsh ${LS_OPTS}"
alias al="ls -A ${LS_OPTS}"
alias sl="ls -lsSh ${LS_OPTS}"
alias sal="ls -AlsSh ${LS_OPTS}"

alias grep="grep ${GREP_OPTS}"
alias fgrep="fgrep ${GREP_OPTS}"
alias egrep="egrep ${GREP_OPTS}"

alias treel="tree -C | less -R"
alias lsmnt="mount | column -t"

alias pathls='printf "%b\n" "${PATH//:/\\n}"'

# git - vim - tmux
alias g="git"
alias groot="cd $(git rev-parse --show-toplevel 2> /dev/null || echo -n ".")"

alias v="$EDITOR"
alias vi="$EDITOR"

alias tmls="tmux ls"
alias tmlsc="tmux lsc"
alias tmks="tmux kill-session -t" # kill one session
alias tmka="tmux kill-server" # aka killall

# python
alias py="python3"
alias ipy="ipython3"
alias venvac="source venv/bin/activate"

alias firefox-temp='firefox --profile $(mktemp -d) &> /dev/null &'

# compression/archives
alias untar="tar -xvf"
alias mktar="tar -caf"
alias tarls="tar -tvf"
alias ungzip="gunzip"

# ruby
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi

PATH+=":/Users/james/Library/Python/3.8/bin"
PATH="/opt/homebrew/opt/sqlite/bin:$PATH"

# functions
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

# vim
swp_vimrc(){
  mv ~/.vim/vimrc ~/.vim/vimrc.swp
  mv ~/.vim/vimrc.min ~/.vim/vimrc
  mv ~/.vim/vimrc.swp ~/.vim/vimrc.min
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

# PROMPT/PS1
git_prompt() {
  BRANCH=$(git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/*\(.*\)/\1/')
  if [ ! -z $BRANCH ]; then
    echo -n "%F{yellow}$BRANCH"

    if [ ! -z "$(git status --short)" ]; then
      echo " %F{red}✗"
    fi
  fi
}
PROMPT='%F{blue}%~$(git_prompt) %F{244}%# %F{reset}'


# * * * * * * * * *
## Env Variables  *
# * * * * * * * * *
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
# (Default prompt - git prompt set later)
# PROMPT='%n@%m %~ %# '

# * * * * * * * * *
## Shell Options  *
# * * * * * * * * *
# History
setopt append_history
setopt extended_history
setopt inc_append_history
setopt share_history
setopt histsavenodups
# Less Annoying
setopt interactive_comments
unsetopt correct_all
export BLOCK_SIZE="'1"
# Tab Completion
setopt auto_menu
autoload -Uz compinit && compinit
# Prompt
setopt prompt_subst
# Deduplicate path
typeset -U path
# Set 'emacs' keybinds
bindkey -e
bindkey '\e[3~' delete-char
bindkey '^p' history-search-backward
bindkey '^n' history-search-forward
bindkey ' '  magic-space

# * * * * * *
## Aliases  *
# * * * * * *
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


# compression/archives
alias untar="tar -xvf"
alias mktar="tar -caf"
alias tarls="tar -tvf"
alias ungzip="gunzip"

# misc
alias firefox-temp='firefox --profile $(mktemp -d) &> /dev/null &'

# * * * * * *
# Functions *
# * * * * * *
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

# * * * * * * * * * * * *
# PROMPT/PS1 Functions  *
# * * * * * * * * * * * *
git_prompt() {
  BRANCH=$(git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/*\(.*\)/\1/')
  STATUS="$(git status 2> /dev/null)"

  if [[ "$?" -ne 0 ]]; then
    return
  fi

  if [ ! -z $BRANCH ]; then
    printf "%s" "%F{reset}%F{yellow}$BRANCH"
    printf "%s" "%F{reset}["
    if echo "${STATUS}" | grep -c "nothing to commit" &> /dev/null; then printf "%s" "%F{blue}="; fi
    if echo "${STATUS}" | grep -c "renamed:"  &> /dev/null; then printf "%s" "%F{red}%"; fi
    if echo "${STATUS}" | grep -c "deleted:"  &> /dev/null; then printf "%s" "%F{red}-"; fi
    if echo "${STATUS}" | grep -c "new file:" &> /dev/null; then printf "%s" "%F{green}+"; fi
    if echo "${STATUS}" | grep -c "branch is ahead:" &> /dev/null; then printf "%s" "%F{yellow}>"; fi
    if echo "${STATUS}" | grep -c "branch is behind" &> /dev/null; then printf "%s" "%F{yellow}<"; fi
    if echo "${STATUS}" | grep -c "Untracked files:" &> /dev/null; then printf "%s" "%F{yellow}?"; fi
    if echo "${STATUS}" | grep -c "Changes not staged for commit:" &> /dev/null; then printf "%s" "%F{red}*"; fi
    if echo "${STATUS}" | grep -c "Changes to be committed:" &> /dev/null; then printf "%s" "%F{green}="; fi
    printf "%s" "%F{reset}]"
  fi
}
PROMPT='%F{blue}%~$(git_prompt) %F{cyan}%# %F{reset}'

# * * * * * * * * * * * * * * * * * * *
# Language Specific Version Managers  *
# * * * * * * * * * * * * * * * * * * *
# rbenv (ruby)
src_rbenv(){
  if command -v ruby > /dev/null && command -v gem > /dev/null; then
	path+=("$(ruby -r rubygems -e 'puts Gem.user_dir')/bin")
    # rbenv shim
    if [ -d "$HOME/.rbenv/bin" ]; then
	  path+=("$HOME/.rbenv/bin")
      [[ ":$PATH:" != *":$HOME/.rbenv/shims:"* ]] && eval "$(rbenv init -)"
    fi
  fi
}

# nvm (node)
src_nvm(){
  if [ -d "$HOME/.nvm" ]; then
    export NVM_DIR="$HOME/.nvm"
    [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
    [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"
  fi
}

# pyenv (python3)
src_pyenv() {
  if [ -d "$HOME/.pyenv" ]; then
    export PYENV_ROOT="$HOME/.pyenv"
	path+=("$PYENV_ROOT/bin")
    command -v pyenv > /dev/null && eval "$(pyenv init -)"
  fi
}

# * * * * * * * * * * * * *
# Language Specific Paths *
# * * * * * * * * * * * * *
#python
# path+=("/Users/james/Library/Python/3.8/bin")

# ruby
if command -v rbenv > /dev/null; then eval "$(rbenv init -)"; fi

# go
if command -v go > /dev/null; then
  [ -d "$HOME/go" ] && mv "$HOME/go" "$HOME/.go"
  export GOPATH="$HOME/.go"
  export GOWD="$GOPATH/src/github.com/lemonase"
  export GOMOD="$GOPATH/pkg/mod/github.com/lemonase/"
  path+=("$(go env GOPATH)/bin")
fi

# rust
if command -v cargo > /dev/null; then
  path+=("$HOME/.cargo/bin")
fi

# * * * * * * * * * * * *
# MISC $PATH Additions  *
# * * * * * * * * * * * *
# homebrew stuff
[ -d "/opt/homebrew/bin" ] && path+=("/opt/homebrew/bin" $path)
[ -d "/opt/homebrew/opt/sqlite/bin" ] && path+=("/opt/homebrew/opt/sqlite/bin" $path)

# local bins
[ -d "$HOME/.local/bin" ] && path+=("$HOME/.local/bin")
[ -d "$HOME/.local/scripts" ] && path+=("$HOME/.local/scripts")

# local rc
[ -r "$HOME/.config/zshrc" ] && source "$HOME/.config/zshrc"
[ -r "$HOME/.local/zshrc" ] && source "$HOME/.local/zshrc"

[ -d "$HOME/.rbenv" ] && path+=("$HOME/.rbenv/bin") && eval "$(rbenv init - zsh)"

# ZSH syntax highlighting plugin
ZSH_SYNTAX_HIGHLIGHT_PATH="${HOMEBREW_PREFIX}/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
[ -f "$ZSH_SYNTAX_HIGHLIGHT_PATH" ] && source $ZSH_SYNTAX_HIGHLIGHT_PATH

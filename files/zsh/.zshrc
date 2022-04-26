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
PROMPT='%n@%m %~ %# '

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

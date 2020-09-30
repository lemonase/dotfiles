# .bash_profile is *not sourced* when launching bash from most GUI terminal emulators
#
# .bash_profile *is sourced* for login shells like from a TTY, ssh connection, or tmux.

# For the sake of simplicity and consistency, I source my .bashrc
[ -f "$HOME/.bashrc" ] && source "$HOME/.bashrc"

# vim:ft=sh

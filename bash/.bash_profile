# .bash_profile is *not* sourced when launching bash from some GUI terminal emulators
# However, it *is* sourced when logging in via TTY or ssh.

# Due to this somewhat inconsistent behaviour, I ended up putting everything in
# .bashrc, which is more or less guarunteed to be sourced no matter what.

# [ -f "$HOME/.bashrc" ] && source "$HOME/.bashrc"

# vim:ft=sh

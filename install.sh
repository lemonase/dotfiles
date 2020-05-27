#!/bin/bash

# make these homey paths
mkdir -pv "$HOME/.vim" "$HOME/.local/"{bin,lib} "$HOME/.config"

# find our dotfiles repo
REPOPATH=$(find "$HOME" -type d -iname 'dotfiles')

# copy our files
cp "$REPOPATH/bash/bashrc"        "$HOME/.bashrc"
cp "$REPOPATH/bash/bash_profile"  "$HOME/.bash_profile"
cp "$REPOPATH/tmux/tmux.conf"     "$HOME/.tmux.conf"
cp "$REPOPATH/git/gitconfig"      "$HOME/.gitconfig"
cp "$REPOPATH/vim/vimrc"          "$HOME/.vim/vimrc"

# link scripts dir
ln -sv "$REPOPATH/scripts" "$HOME/.local/"


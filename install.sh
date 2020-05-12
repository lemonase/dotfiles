#!/bin/bash

[ ! -d "$HOME/.vim/" ] && mkdir "$HOME/.vim"

cp bash/bashrc        ~/.bashrc
cp bash/bash_profile  ~/.bash_profile
cp tmux/tmux.conf     ~/.tmux.conf
cp git/gitconfig      ~/.gitconfig
cp vim/vimrc          ~/.vim/vimrc

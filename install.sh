#!/bin/bash
#
# A simple install script to manage dotfiles with stow

CONFIG_DIR="$(dirname "$0")/config"
cd $CONFIG_DIR

if [[ "$1" == "help" || "$1" == "--help" || "$1" == "-h" ]]; then
  echo "dotfile symlink install script:"
  echo "options:"
  echo ""
  echo "install, i:"
  echo "  install dotfile symlinks to $HOME directory"
  echo ""
  echo "uninstall, u, delete, d, remove, r:"
  echo "  uninstall dotfile symlinks to $HOME directory"
  echo ""
  echo "repair, r:"
  echo "  repair old dotfile symlinks"
  echo ""
  echo "list, l:"
  echo "  list symlinks files in home directory"
  exit
fi

if [[ "$1" == "install" || "$1" == "i" ]]; then
  stow -t "$HOME" -S *
fi

if [[ "$1" == "uninstall" || "$1" == "u" || "$1" == "delete" || "$1" == "d" || "$1" == "remove" || "$1" == "r" ]]; then
  stow -t "$HOME" -D *
fi

if [[ "$1" == "repair" || "$1" == "R" ]]; then
  stow -t "$HOME" -R *
fi

if [[ "$1" == "list" || "$1" == "l" ]]; then
  echo "listing symlinks under $HOME:"
  echo
  find $HOME -maxdepth 1 -type l -ls | awk '{print $11, $12, $NF}'
fi


# default behaviour (run stow to symlink dotfiles to $HOME)
[ "$#" -eq 0 ] && stow -t "$HOME" -S *


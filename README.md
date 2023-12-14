# My dotfiles

A collection of my config files (also known as dotfiles).

I am using [GNU Stow](https://www.gnu.org/software/stow/) to symlink
my dotfiles to their correct locations in my `$HOME` directory.

## Cloning this repo

```bash
git clone https://github.com/lemonase/dotfiles.git
```

## Installing `stow` on Linux and macOS

```bash
# Ubuntu/Debian
apt install stow

# Fedora/RHEL
dnf install stow

# Arch
pacman -S stow

# macOS
brew install stow
```

## Using `install.sh` script

```bash
$ ./install.sh --help
dotfile symlink install script:
options:

install, i:
  install dotfile symlinks to $HOME directory

uninstall, u, delete, d, remove, r:
  uninstall dotfile symlinks to $HOME directory

repair, r:
  repair old dotfile symlinks

list, l:
  list symlinks files in home directory
```

## Using `stow`

```bash
# Installing all symlinks
stow -t "$HOME" -S *

# Uninstalling all symlinks
stow -t "$HOME" -D *

# Repairing all symlinks
stow -t "$HOME" -R *

# Installing specific dotfiles
stow -t "$HOME" -S git
```

If there are no errors, everything in that directory should be symlinked.
Check by doing a quick `ls -al ~/`

## Troubleshooting

### Error: File conflicts

```
WARNING! stowing git would cause conflicts:
  * existing target is neither a link nor a directory: .gitconfig
```

### Solution

Delete or move file in question


# My dotfiles

I am using [GNU Stow](https://www.gnu.org/software/stow/) to symlink
my dotfiles to their correct locations in my `$HOME` directory.

## Cloning this repo

```bash
git clone https://github.com/lemonase/dotfiles.git && cd dotfiles/config
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

---

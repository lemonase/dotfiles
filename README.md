# My dotfiles

I am using [GNU Stow](https://www.gnu.org/software/stow/) to symlink
my dotfiles to their correct locations in my `$HOME` directory.

## Installing `stow`

### Ubuntu/Debian

```bash
apt install stow
```

### Fedora/RHEL

```bash
dnf install stow
```

### Arch

```bash
pacman -S stow
```

### MacOS

```bash
brew install stow
```

## Cloning this repo

```bash
git clone https://github.com/lemonase/dotfiles.git && cd dotfiles/config
```

## Using `stow`

### Installing symlinks

```bash
stow -t "$HOME" -S *
```

### Uninstalling symlinks

```bash
stow -t "$HOME" -D *
```

### Repairing symlinks

```bash
stow -t "$HOME" -R *
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

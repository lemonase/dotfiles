# My dotfiles

I am using [GNU Stow](https://www.gnu.org/software/stow/) to symlink
my dotfiles to their correct locations in my `$HOME` directory.

## Instructions

### Installing stow

```bash
apt install stow # debian based distros
dnf install stow # fedora based distros
pacman -S stow # arch based distros
```

### Cloning this repo

```sh
git clone https://github.com/lemonase/dotfiles.git
cd dotfiles/files
```

### Installing

```sh
stow --target="$HOME" *
```

### Uninstalling

```sh
stow --delete *
```

### Repair Links

```sh
stow --restow *
```

If there are no errors, everything in that directory should be symlinked.
Check by doing a quick `ls -al ~/`

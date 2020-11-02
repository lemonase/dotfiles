# My dotfiles

I am using [GNU Stow](https://www.gnu.org/software/stow/) to symlink
my dotfiles to their correct locations in my `$HOME` directory.

## Instructions

```sh
$ git clone https://github.com/lemonase/dotfiles.git
$ cd dotfiles/files
```

### Installing

```sh
$ stow --target="$HOME" *
```

### Uninstalling

```sh
$ stow --delete *
```

### Repair Links

```sh
$ stow --restow *
```

If there are no errors, everything in that directory should be symlinked.

# My dotfiles

I am using [GNU Stow](https://www.gnu.org/software/stow/) to symlink
my dotfiles to their correct locations in my `$HOME` directory.

## Instructions

First `cd` into the `stow` directory and run the following command

```sh
stow * -t "$HOME"
```

If there are no errors, everything in that directory should be symlinked.

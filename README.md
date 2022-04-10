# dotfiles

This repository contains public sections of my configuration files.

# Application

First, create new files in the `~/.dotfiles` directory under the respective package. Then populate into the actual path, searched for by the respective tool.

``` shell
cd ~/.dotfiles
# create new files that should exist in the target, e.g. emacs
stow -v emacs
```

**Note**: This does not work for `.gitignore` files. These are simply ignored and not linked.

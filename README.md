# About #

A collection of configuration file for emacs, vim, zsh, git, and others, as well
as various plugins/packages as git submodules.

I can't live without these, and hopefully others might find it helpful. I don't
believe in things like [oh-my-zsh](https://github.com/robbyrussell/oh-my-zsh/)
or [prelude](http://batsov.com/prelude/), I have a compulsive need to know
exactly what's going on and how to make stuff work the way I like it.

I mostly assembled these from the web and tweaked them to my preference, but have used
several sources as inspiration:

* Emacs:
    * [emacs-fu](http://www.djcbsoftware.nl/dot-emacs.html)
    * [magnars/.emacs.d](https://github.com/magnars/.emacs.d)
    * [vmalloc/emacs](https://github.com/vmalloc/emacs)
* Zsh:
    * [oh-my-zsh](https://github.com/robbyrussell/oh-my-zsh/)

# Installation #

You can either clone using:

    git clone --recursive git://github.com/spatz/config-files.git

Or clone as usual and then run:

    git submodule update --init --recursive

## Emacs ##

Create the symlink `~/.emacs.d` using:

    ln -s <REPO_PATH>/emacs.d ~/.emacs.d

Note: Emacs 24.1 is required, older version will not work.

## VIM ##

Two symlinks are required, `~/.vimrc` and `~/.vim`:

    ln -s <REPO_PATH>/vim/vimrc.local ~/.vimrc
    ln -s <REPO_PATH>/vim/dotvim ~/.vim

## Zsh ##

Run the script `zsh/install.zsh <REPO_PATH>` (note that `<REPO_PATH>` is
optional and defaults to `~/config-files`).

## Git ##

Symlink the `gitconfig` in the repository to either the system-wide or user
specific config files:

    ln -s <REPO_PATH>/gitconfig /etc/gitconfig

Or

    ln -s <REPO_PATH>/gitconfig ~/.gitconfig

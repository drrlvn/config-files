# Table of Contents #

* [About](#about)
* [Usage](#usage)
  * [Emacs](#emacs)
  * [VIM](#vim)
  * [Zsh](#zsh)
  * [Git](#git)
  * [Tmux](#tmux)

# About #

A collection of configuration file for emacs, vim, zsh, git, and others, as well as various
plugins/packages as git submodules.

Hopefully others might find these helpful. I don't believe in "starter-packas" such as
[oh-my-zsh](https://github.com/robbyrussell/oh-my-zsh/), [prelude](http://batsov.com/prelude/) or
[Spacemacs](http://spacemacs.org/). I feel like the "one size fits all" approach leads to bloat and
means that users are not familiar with either the tool or the options the package configured for
them.

I mostly assembled bits and pieces over time as the need grew, but some sources served as
inspiration and deserve explicit credit:

* Emacs:
  * [emacs-fu](http://www.djcbsoftware.nl/dot-emacs.html)
  * [magnars/.emacs.d](https://github.com/magnars/.emacs.d)
  * [vmalloc/emacs](https://github.com/vmalloc/emacs)
* Zsh:
  * [oh-my-zsh](https://github.com/robbyrussell/oh-my-zsh/)

# Usage #

Install scripts are provided for some configurations that are a bit harder to setup. They are
deliberately kept separated so that users can pick and choose and are not forced to use everything.

Install script always get an optional path to where this repository is cloned and default to
`~/config-files`.

You can either clone using:

    git clone --recursive git://github.com/spatz/config-files.git

Or clone as usual and then run:

    git submodule update --init --recursive

## Emacs ##

Create the symlink `~/.emacs.d` using:

    ln -s <REPO_PATH>/emacs.d ~/.emacs.d

Notes:
* Emacs 25.1 is required, older version will not work.
* For spell checking to work an ispell compatible program and dictionary need to be installed,
  e.g. `aspell-en`.
* By default `cmark` is used to markdown export/preview so it needs to be installed.

## VIM ##

Two symlinks are required, `~/.vimrc` and `~/.vim`:

    ln -s <REPO_PATH>/vim/vimrc.local ~/.vimrc
    ln -s <REPO_PATH>/vim/dotvim ~/.vim

## Zsh ##

Run the script `zsh/install.zsh <REPO_PATH>`.

## Git ##

Run the script `install-git.zsh <REPO_PATH>`.

## Tmux ##

Run the script `tmux/install.zsh <REPO_PATH>`.

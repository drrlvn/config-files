#!/usr/bin/zsh

REPO_PATH="${1:-$0:a:h}"

echo -n "Using ${REPO_PATH} -- Press enter to continue..."; read

mkdir -p ~/.config/git
> ~/.config/git/config <<END
[include]
    path = ${REPO_PATH}/gitconfig
END

mkdir -p ~/bin
ln -sf ${REPO_PATH}/diff-highlight ~/bin/

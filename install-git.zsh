#!/usr/bin/zsh

REPO_PATH="${1:-${HOME}/config-files}"

echo -n "Using ${REPO_PATH} -- Press enter to continue..."; read

mkdir -p ~/.config/git
ln -sf ${REPO_PATH}/gitconfig ~/.config/git/config

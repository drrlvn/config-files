#!/usr/bin/zsh

REPO_PATH="${1:-${HOME}/config-files}"

echo -n "Using ${REPO_PATH} -- Press enter to continue..."; read

> ~/.tmux.conf <<END
source ${REPO_PATH}/tmux/tmux.conf
END

ln -sf ${REPO_PATH}/tmux ~/.tmux

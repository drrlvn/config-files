#!/usr/bin/env zsh

REPO_PATH="${1:-$0:a:h:h}"

echo -n "Using ${REPO_PATH} -- Press enter to continue..."; read

> ~/.tmux.conf <<END
source ${REPO_PATH}/tmux/tmux.conf
END

ln -sf ${REPO_PATH}/tmux ~/.tmux

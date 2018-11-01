#!/usr/bin/env sh

if [ -z $1 ]; then
    repo_path=$(dirname $(dirname $(realpath $0)))
else
    repo_path="$1"
fi

read -p "Using $repo_path -- Press enter to continue..." REPLY

echo "source $repo_path/tmux/tmux.conf" > ~/.tmux.conf

ln -sf $repo_path/tmux ~/.tmux

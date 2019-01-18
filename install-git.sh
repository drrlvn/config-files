#!/usr/bin/env sh

if [ -z $1 ]; then
    repo_path=$(dirname "$(realpath $0)")
else
    repo_path="$1"
fi

read -p "Using $repo_path -- Press enter to continue..." REPLY

mkdir -p ~/.config/git
printf "[include]\n    path = $repo_path/gitconfig" > ~/.config/git/config

mkdir -p ~/.local/bin/
ln -sf "$repo_path/diff-highlight" ~/.local/bin/
ln -sf "$repo_path/gitignore_global" ~/.gitignore_global

#!/usr/bin/env bash

if [ -z $1 ]; then
    repo_path=$(dirname $(realpath $0))
else
    repo_path="$1"
fi

read -p "Using $repo_path -- Press enter to continue..."

mkdir -p ~/.config/git
echo -e "[include]\n    path = $repo_path/gitconfig" > ~/.config/git/config

mkdir -p ~/bin
ln -sf $repo_path/diff-highlight ~/bin/
ln -sf $repo_path/gitignore_global ~/.gitignore_global

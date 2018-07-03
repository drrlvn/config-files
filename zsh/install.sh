#!/usr/bin/env sh

if [ -z $1 ]; then
    repo_path=$(dirname $(dirname $(realpath $0)))
else
    repo_path="$1"
fi

read -p "Using $repo_path -- Press enter to continue..."

echo "source $repo_path/zsh/zshrc" > ~/.zshrc

mkdir -p ~/.zsh
ln -sf $repo_path/zsh/prompt_spatz_setup ~/.zsh/

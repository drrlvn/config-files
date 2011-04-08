#!/bin/zsh

if [[ -e ~/.zshrc ]]; then
	if [[ $1 = -f ]]; then
		shift
	else
		echo "Aborting: ~/.zshrc exists. Use -f to override."
		exit 1
	fi
fi

REPO_PATH="${1:-~/config-files}"

echo -n "Using ${REPO_PATH} -- Press enter to continue..."; read

> ~/.zshrc <<END
fpath=(~/.zsh \$fpath)
source ${REPO_PATH}/zsh/zshrc
END

mkdir -p ~/.zsh
ln -s ${REPO_PATH}/zsh/prompt_spatz_setup ~/.zsh/

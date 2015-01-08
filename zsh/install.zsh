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
fpath=(\$fpath ~/.zsh)
source ${REPO_PATH}/zsh/zshrc
END

ln -sf "${~REPO_PATH}/zsh/zlogin" ~/.zlogin

mkdir -p ~/.zsh
pushd ~/.zsh
ln -sf "${~REPO_PATH}/zsh/prompt_spatz_setup" .
popd

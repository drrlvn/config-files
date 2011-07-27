#!/bin/zsh

for file in **/*.el; do
    if [[ ! -e ${file}c ]] || [[ ${file} -nt ${file}c ]]; then
        echo -n ${file}...
        emacsclient -a "" -e "(byte-compile-file \"$file\")" | grep -v '^t$'
        echo " Done"
    fi
done

#!/bin/zsh

for file in **/*.el; do
    [ -e ${file}c ] || emacsclient -a "" -e "(byte-compile-file \"$file\")"
done

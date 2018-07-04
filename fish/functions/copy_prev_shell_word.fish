function copy_prev_shell_word
    commandline -i (commandline -co | tail -n1 | string escape)
end

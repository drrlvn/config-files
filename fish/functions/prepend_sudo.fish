function prepend_sudo -d "Prepend sudo to command line"
    commandline | read -la current_cmdline
    if not contains sudo $current_cmdline
        commandline "sudo $current_cmdline"
    end
end

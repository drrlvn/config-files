function fish_title
    switch $_
        case fish
            prompt_pwd
        case sudo
            echo (string split -m 2 ' ' $argv[1])[2]
        case '*'
            echo $_
    end
end

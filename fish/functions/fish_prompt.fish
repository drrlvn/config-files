function fish_prompt
    if test $status -eq 0
        set prompt_status (set_color green)' ✔'
    else
        set prompt_status (set_color red)' ✘'
    end

	test $SSH_TTY
    and printf (set_color red)$USER(set_color brwhite)'@'(set_color yellow)(prompt_hostname)' '
    test "$USER" = 'root'
    and echo (set_color red)"#"

    # Main

    echo -n $prompt_status (set_color red)'❯'(set_color yellow)'❯'(set_color green)'❯ '
end

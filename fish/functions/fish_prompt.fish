function fish_prompt
    if test $status -eq 0
        set prompt_status (set_color brgreen)' ✔'
    else
        set prompt_status (set_color brred)' ✘'
    end

    if not set -q $SSH_TTY
        echo -n (set_color brmagenta)$USER@(prompt_hostname)
    end
    if test "$USER" = 'root'
        echo -n (set_color brred)"#"
    end

    echo -n $prompt_status (set_color brred)'❯'(set_color bryellow)'❯'(set_color brgreen)'❯ '(set_color normal)
end

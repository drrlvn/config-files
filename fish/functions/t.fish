function t
    if set -q $argv[1]
        set session_name $argv[1]
    else
        set session_name main
    end
    tmux new-session -A -s $session_name
end

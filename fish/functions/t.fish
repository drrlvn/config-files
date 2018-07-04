function t
    if set -q $argv[1]
        set session_name main
    else
        set session_name $argv[1]
    end
    tmux new-session -A -s $session_name
end

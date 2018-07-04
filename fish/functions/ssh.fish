function ssh
    if string match -q 'tmux*' "$TERM"
        env TERM=screen ssh $argv
    else
        command ssh $argv
    end
end

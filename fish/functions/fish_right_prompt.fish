function fish_right_prompt
    echo -ns (__fish_git_prompt '%s ') (prompt_virtualenv) (set_color brblue) (prompt_pwd)
end

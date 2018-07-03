function fish_right_prompt
    echo -ns (__fish_git_prompt '%s ') (prompt_virtualenv) (set_color blue) (prompt_pwd)
end

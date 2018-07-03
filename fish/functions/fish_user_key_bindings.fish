function fish_user_key_bindings
    bind \es prepend_sudo
    bind \e\[1\;7D cycle_dir_left
    bind \e\[1\;7C cycle_dir_right

    type -q fzf_key_bindings; and fzf_key_bindings
end

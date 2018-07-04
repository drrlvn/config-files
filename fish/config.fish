function fish_greeting
    if type -q systemctl
        set failed (systemctl list-units --state=failed --no-legend)
        if not set -q $failed
            echo Failed Units: (set_color brred)(count $failed)(set_color normal)
            for unit in $failed
                echo '  '(string split -m 1 ' ' -- $unit)[1]
            end
        end
    end
end

setup_theme
setup_git_prompt
setup_path

set -x EDITOR '/usr/bin/emacsclient -c'
set -x ALTERNATE_EDITOR '/usr/bin/vim'
set -x LESS '-R -n -X -m -i -S'
set -x VIRTUAL_ENV_DISABLE_PROMPT 1
set -x RIPGREP_CONFIG_PATH $HOME/.config/ripgreprc

if type -q fzf
    set -x FZF_TMUX 1
    set -x FZF_DEFAULT_COMMAND 'fd -t f'
    set -x FZF_CTRL_T_COMMAND "$FZF_DEFAULT_COMMAND . \$dir"
    set -x FZF_CTRL_T_OPTS '--preview \'bat --color always {}\''
    set -x FZF_ALT_C_COMMAND "fd -t d . \$dir"
    set -x FZF_ALT_C_OPTS '--preview \'exa --tree --group-directories-first -s extension --color always -F -L 2 {} | head -n 100\''
end

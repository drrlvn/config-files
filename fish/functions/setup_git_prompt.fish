function setup_git_prompt
    set -g __fish_git_prompt_show_informative_status
    set -g __fish_git_prompt_color_branch brwhite
    set -g __fish_git_prompt_color_upstream_ahead green
    set -g __fish_git_prompt_color_upstream_behind red
    set -g __fish_git_prompt_color_upstream yellow
    set -g __fish_git_prompt_color_dirtystate red
    set -g __fish_git_prompt_color_stagedstate green
    set -g __fish_git_prompt_color_untrackedfiles yellow
    set -g __fish_git_prompt_color_cleanstate green
end

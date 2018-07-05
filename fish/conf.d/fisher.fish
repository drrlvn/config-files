set fisher_file ~/.config/fish/fishfile
set fish_path ~/.config/fish-plugins

set fish_function_path $fish_path/functions $fish_function_path
set fish_complete_path $fish_path/completions $fish_complete_path

for file in $fish_path/conf.d/*.fish
    builtin source $file 2> /dev/null
end

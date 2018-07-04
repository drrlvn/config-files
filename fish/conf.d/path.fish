set -U fish_user_paths
for path in ~/.gem/ruby/*/bin ~/.cargo/bin ~/bin
    test -d "$path"; and set -U fish_user_paths "$path" $fish_user_paths
end

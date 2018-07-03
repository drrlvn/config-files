function setup_path
    for path in $HOME/.cargo/bin $HOME/bin
        test -d "$path"; and set -x PATH "$path" $PATH
    end
    for path in $HOME/.gem/ruby/*/bin
        test -d "$path"; and set -x PATH $PATH "$path"
    end
end

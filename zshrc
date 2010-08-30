setopt extendedglob correct auto_pushd pushd_silent pushd_to_home
bindkey -e

bindkey ';3D' emacs-backward-word
bindkey ';5D' emacs-backward-word
bindkey ';3C' emacs-forward-word
bindkey ';5C' emacs-forward-word

bindkey '[5~' history-beginning-search-backward
bindkey '[6~' history-beginning-search-forward

bindkey '[H' beginning-of-line
bindkey '[F' end-of-line
bindkey '[3~' delete-char

#zstyle :compinstall filename '/home/spatz/.zshrc'

autoload -Uz compinit promptinit
compinit
promptinit; prompt gentoo

[[ -f /etc/DIR_COLORS ]] && eval $(dircolors -b /etc/DIR_COLORS)

alias cp='cp -i'
alias mv='mv -i'
alias ls='ls --color=auto --group-directories-first -lhX'
alias grep='grep --color=auto'
alias egrep='egrep --color=auto'

alias g='git'
gg() {
	local dir="$1" ; shift
	( cd "$dir" && git "$@" )
}

overlays() {
	local o
	for o in "${OVERLAYS[@]}"; do
		$o-overlay "$@"
	done
}

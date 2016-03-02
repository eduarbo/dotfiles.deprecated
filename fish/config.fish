set -g -x PATH ~/bin $PATH
set DOT ~/.dotfiles
set fish_greeting "GLaDOS v1.09 (c) 1982 Aperture Science, Inc."
set fisher_home ~/.local/share/fisherman
set fisher_config ~/.config/fisherman
source $fisher_home/config.fish
source $DOT/bash/aliases
bass source $DOT/bin/init-gpg-agent

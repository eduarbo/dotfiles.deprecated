set -U fish_user_paths ~/bin $fish_user_paths /usr/local/sbin
set DOT ~/.dotfiles
set fish_greeting "GLaDOS v1.09 (c) 1982 Aperture Science, Inc."

source $DOT/bash/aliases
if type bass >/dev/null ^/dev/null
  # bass allows me to source bash scripts
  bass source $DOT/bin/init-gpg-agent
end

set fisher_home ~/.local/share/fisherman
set fisher_config ~/.config/fisherman
source $fisher_home/config.fish

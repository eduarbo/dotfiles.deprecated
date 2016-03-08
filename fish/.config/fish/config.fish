set DOT ~/.dotfiles
set -U fish_user_paths $HOME/bin $DOT/bin /usr/local/sbin $fish_user_paths
set fish_greeting "GLaDOS v1.09 (c) 1982 Aperture Science, Inc."

source $DOT/lib/aliases
if type bass >/dev/null ^/dev/null
  # bass allows me to source bash scripts
  bass source $DOT/lib/init-gpg-agent
end

set fisher_home ~/.local/share/fisherman
set fisher_config ~/.config/fisherman
source $fisher_home/config.fish

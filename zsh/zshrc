DOT=$HOME/.dotfiles

source $DOT/lib/checks
source $DOT/bash/aliases
source $DOT/zsh/colors
source $DOT/zsh/history
source $DOT/zsh/options
source $DOT/zsh/bindings
source $DOT/zsh/prompt
source $DOT/zsh/title
source $DOT/zsh/plugins
source $DOT/zsh/completion

export EDITOR='vim'
export VISUAL=$EDITOR
export PAGER='less'

eval "$(fasd --init auto)"

# Portal FTW!
print -P "%F{231}GLaDOS v1.09 (c) 1982 Aperture Science, Inc.%f"

if [[ $TMUX = "" ]]; then
  export TERM="xterm-256color"
else
  export TERM="screen-256color"
fi
# force 256-color gruvbox palette
# https://github.com/morhetz/gruvbox-generalized/tree/master/iterm2
GRUVBOX="$HOME/.dotfiles/vim/plugged/gruvbox/gruvbox_256palette_osx.sh"
[[ -e $GRUVBOX ]] && source $GRUVBOX

[[ -e "$HOME/.secrets" ]] && source $HOME/.secrets
# Zsh-specific
[[ -e "$HOME/.zshrc.local" ]] && source $HOME/.zshrc.local

DOT=$HOME/.dotfiles

source $DOT/lib/utils
source $DOT/bash/aliases
source $DOT/zsh/colors
source $DOT/zsh/history
source $DOT/zsh/options
source $DOT/zsh/bindings
source $DOT/zsh/prompt
source $DOT/zsh/title
source $DOT/zsh/completion

export EDITOR='vim'
export VISUAL=$EDITOR
export PAGER='less'

if [[ $TMUX = "" ]]; then
  export TERM="xterm-256color"
else
  export TERM="screen-256color"
fi

export GOPATH=~/dev/go
export PATH="$PATH:$GOPATH/bin"
export PATH=$PATH:/usr/local/opt/go/libexec/bin
export PATH=$PATH:$HOME/.cabal/bin

# Portal FTW!
print -P "%F{231}GLaDOS v1.09 (c) 1982 Aperture Science, Inc.%f"

[[ -e "$HOME/.secrets" ]] && source $HOME/.secrets

# Zsh-specific
[[ -e "$HOME/.zshrc.local" ]] && source $HOME/.zshrc.local

# zsh-syntax-highlighting.zsh needs to be sourced at the end
source $DOT/zsh/plugins

# vim: set ft=zsh :

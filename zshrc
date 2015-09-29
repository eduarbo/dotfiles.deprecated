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

export GOPATH=~/dev/go
export PATH="$PATH:$GOPATH/bin"
export PATH=$PATH:/usr/local/opt/go/libexec/bin
export PATH=$PATH:$HOME/.cabal/bin

# FZF (move to plugins) {{{
export FZF_DEFAULT_OPTS="--extended --cycle"

# Setting ag as the default source for fzf
# export FZF_DEFAULT_COMMAND='ag -l -g ""'
export FZF_DEFAULT_COMMAND='
  (git ls-tree -r --name-only HEAD ||
   ag -l -g "" ||
   find * -name ".*" -prune -o -type f -print -o -type l -print) 2> /dev/null'

# To apply the command to CTRL-T as well
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
# }}}

[[ -e "$HOME/.secrets" ]] && source $HOME/.secrets

# Zsh-specific
[[ -e "$HOME/.zshrc.local" ]] && source $HOME/.zshrc.local
# vim: set ft=zsh :

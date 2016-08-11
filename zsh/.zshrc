#!/usr/bin/env zsh
source $DOT/bash/lib/aliases
source $DOT/zsh/lib/colors
source $DOT/zsh/lib/history
source $DOT/zsh/lib/options
source $DOT/zsh/lib/bindings
source $DOT/zsh/lib/prompt
source $DOT/zsh/lib/title
source $DOT/zsh/lib/completion
source $DOT/zsh/lib/plugins

source_file $HOME/.secrets
source_file $HOME/.zshrc.local

# zsh-syntax-highlighting.zsh needs to be sourced at the end
if [ "$HAS_BREW" ]; then
  source_file ${BREW_LOCATION}/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
fi

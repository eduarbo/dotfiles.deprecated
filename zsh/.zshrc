#!/usr/bin/env zsh
source $DOT/lib/aliases
source $DOT/lib/zsh/colors
source $DOT/lib/zsh/history
source $DOT/lib/zsh/options
source $DOT/lib/zsh/bindings
source $DOT/lib/zsh/prompt
source $DOT/lib/zsh/title
source $DOT/lib/zsh/completion

source_file $HOME/.secrets
source_file $HOME/.zshrc.local

# zsh-syntax-highlighting.zsh needs to be sourced at the end
if [[ "$HAS_BREW" ]]; then
  source_file ${BREW_LOCATION}/opt/zsh-history-substring-search/zsh-history-substring-search.zsh
  source_file ${BREW_LOCATION}/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
fi

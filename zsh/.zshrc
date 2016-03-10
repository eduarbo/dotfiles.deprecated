#!/usr/bin/env zsh
source $DOT/lib/utils
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
source $DOT/lib/zsh/plugins
source $DOT/lib/common_path

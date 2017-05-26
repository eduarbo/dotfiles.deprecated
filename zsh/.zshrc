#!/usr/bin/env zsh
source $DOT/bash/lib/aliases
source $DOT/zsh/lib/colors
source $DOT/zsh/lib/history
source $DOT/zsh/lib/options
source $DOT/zsh/lib/bindings
source $DOT/zsh/lib/title
source $DOT/zsh/lib/completion
source $DOT/zsh/lib/plugins

source_file $HOME/.secrets
source_file $HOME/.zshrc.local

### Added by Zplugin's installer
source_file '/Users/eduarbo/.zplugin/bin/zplugin.zsh'

if ! type_exists 'zplugin'; then
  # Install zplugin when missing
  sh -c "$(curl -fsSL https://raw.githubusercontent.com/psprint/zplugin/master/doc/install.sh)"
  source_file '/Users/eduarbo/.zplugin/bin/zplugin.zsh'
fi

autoload -Uz _zplugin
(( ${+_comps} )) && _comps[zplugin]=_zplugin
### End of Zplugin's installer chunk

zplugin load psprint zsh-navigation-tools
zplugin load zdharma/zui
zplugin load zsh-users/zsh-completions
zplugin load zsh-users/zsh-autosuggestions
zplugin load psprint/history-search-multi-word
zplugin load zdharma/fast-syntax-highlighting
zplugin snippet "https://github.com/mafredri/zsh-async/blob/master/async.zsh"
zplugin load eduarbo/simpl

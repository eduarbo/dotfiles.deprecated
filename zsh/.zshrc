#!/usr/bin/env zsh
source $DOT/bash/lib/aliases
source $DOT/zsh/lib/colors
source $DOT/zsh/lib/options
source $DOT/zsh/lib/bindings
source $DOT/zsh/lib/title
source $DOT/zsh/lib/completion
source $DOT/zsh/lib/plugins

source_file $HOME/.secrets
source_file $HOME/.zshrc.local

# I went for zplugin as plugin manager for being the fastest between antibody,
# antigen and zplug in my tests. Zplug is the worst! forget to include it in
# future tests

ZPLG_HOME="$HOME/.zplugin"
source_file "$ZPLG_HOME/bin/zplugin.zsh"

# Install zplugin if missing
if ! type_exists 'zplugin'; then
  if ! test -d "$ZPLG_HOME"; then
    mkdir "$ZPLG_HOME"
    chmod g-rwX "$ZPLG_HOME"
  fi

  echo ">>> Downloading zplugin to $ZPLG_HOME/bin"
  if test -d "$ZPLG_HOME/bin/.git"; then
    cd "$ZPLG_HOME/bin"
    git pull origin master
  else
    cd "$ZPLG_HOME"
    git clone https://github.com/psprint/zplugin.git bin
  fi

  source "$ZPLG_HOME/bin/zplugin.zsh"
fi

zplugin ice blockf
zplugin light zdharma/zui
zplugin ice blockf
zplugin light zsh-users/zsh-completions
zplugin light zsh-users/zsh-autosuggestions
zplugin light zdharma/history-search-multi-word
zplugin light zdharma/fast-syntax-highlighting
zplugin light mafredri/zsh-async
zplugin light eduarbo/simpl

export NVM_NO_USE=true
export NVM_LAZY_LOAD=true
zplugin light lukechilds/zsh-nvm

# Load and initialize the completion system with a cache time of 20 hours, so it
# should almost always regenerate the first time a shell is opened each day.
autoload -Uz compinit
compfiles=(${ZDOTDIR:-$HOME}/.zcompdump(Nm-20))
if [[ $#compfiles > 0 ]]; then
  compinit -C
else
  compinit
fi

# execute compdefs provided by rest of plugins
zplugin cdreplay -q # -q is for quiet

_stop_startup_timer

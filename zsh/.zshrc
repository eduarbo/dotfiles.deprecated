#!/usr/bin/env zsh
source $DOT/bash/lib/utils
source $DOT/bash/lib/aliases
source $DOT/zsh/lib/options
# source $DOT/zsh/lib/colors
source $DOT/zsh/lib/bindings
source $DOT/zsh/lib/title
source $DOT/zsh/lib/completion
source $DOT/zsh/lib/init-gpg-agent

source_file $HOME/.secrets
source_file $HOME/.zshrc.local

# FASD
fasd_cache=$HOME/.fasd-init-$SHELL_NAME
if type_exists 'fasd'; then
  if [ "$(command -v fasd)" -nt "$fasd_cache" -o ! -s "$fasd_cache" ]; then
    if [[ -n $IS_ZSH ]]; then
      fasd --init posix-alias zsh-hook zsh-ccomp zsh-ccomp-install zsh-wcomp zsh-wcomp-install >| "$fasd_cache"
    elif [[ -n $IS_BASH ]]; then
      fasd --init posix-alias bash-hook bash-ccomp bash-ccomp-install >| "$fasd_cache"
    fi
  fi
fi
source_file "$fasd_cache"
unset fasd_cache

#
# Plugins
#

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

# zplugin ice blockf
# zplugin light zdharma/zui
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

# Execute code that does not affect the current session in the background.
{
  # Compile the completion dump to increase startup speed.
  zcompdump="${ZDOTDIR:-$HOME}/.zcompdump"
  if [[ "$zcompdump" -nt "${zcompdump}.zwc" || ! -s "${zcompdump}.zwc" ]]; then
    zcompile "$zcompdump"
  fi
} &!

_stop_startup_timer

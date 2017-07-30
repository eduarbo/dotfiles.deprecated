#!/usr/bin/env zsh
# zshrc
# By Eduardo Ruiz <eduarbo@gmail.com>

if is_interactive; then
  load shell/lib/bindings.zsh
  # load shell/lib/bindings.hlissner.zsh
  load shell/lib/title.zsh
  load shell/lib/completion.zsh
  # load shell/lib/completion.hlissner.zsh
  load shell/lib/init-gpg-agent.zsh

  # FASD
  cache fasd --init posix-alias zsh-hook zsh-ccomp zsh-ccomp-install zsh-wcomp zsh-wcomp-install
fi

load shell/lib/aliases.sh
loadall aliases.sh

source_file $HOME/.secrets
source_file $HOME/.zshrc.local

#
# Plugins
#

# I choosed zplugin as my plugin manager for being the fastest between antibody,
# antigen and zplug in my tests. Zplug is the worst! forget to include it in
# future tests

ZPLG_HOME="$HOME/.zplugin"
source_file "$ZPLG_HOME/bin/zplugin.zsh"

# Install zplugin if missing
if ! is_callable 'zplugin'; then
  cache_clear

  if [[ ! -d "$ZPLG_HOME" ]]; then
    mkdir "$ZPLG_HOME"
    chmod g-rwX "$ZPLG_HOME"
  fi

  echo ">>> Downloading zplugin to $ZPLG_HOME/bin"
  if [[ -d "$ZPLG_HOME/bin/.git" ]]; then
    cd "$ZPLG_HOME/bin"
    git pull origin master
  else
    cd "$ZPLG_HOME"
    git clone https://github.com/psprint/zplugin.git bin
  fi

  source "$ZPLG_HOME/bin/zplugin.zsh"
fi

zplugin light mafredri/zsh-async

if is_interactive; then
  # zplugin light zdharma/zui
  zplugin ice blockf # Disallow zsh-ompletions to modify fpath
  zplugin light zsh-users/zsh-completions
  zplugin light zsh-users/zsh-autosuggestions
  zplugin light zdharma/history-search-multi-word
  is_ssh || zplugin light zdharma/fast-syntax-highlighting
  zplugin light eduarbo/simpl
  zplugin light djui/alias-tips
fi

loadall plugins.zsh

# Load and initialize the completion system with a cache time of 20 hours, so it
# should almost always regenerate the first time a shell is opened each day.
# Compinit should be called after loading of all plugins and before possibly calling cdreply
autoload -Uz compinit

zcompdump="$CACHE_DIR"/zcompdump
zcompfiles=($zcompdump(Nm-20))
if [[ $#zcompfiles > 0 ]]; then
  compinit -C -d "$zcompdump"
else
  compinit -d "$zcompdump"
fi
unset zcompfiles

# execute compdefs provided by rest of plugins
zplugin cdreplay -q # -q is for quiet

# Execute code that does not affect the current session in the background.
{
  # Compile the completion dump to increase startup speed
  if [[ "$zcompdump" -nt "${zcompdump}.zwc" || ! -s "${zcompdump}.zwc" ]]; then
    zcompile "$zcompdump"
  fi
} &!

_stop_startup_timer

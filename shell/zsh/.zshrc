# zshrc
# By Eduardo Ruiz <eduarbo@gmail.com>


#
# Plugins
#

# Load zplugin and install it when missing
_load_repo zdharma/zplugin $ZPL_DIR zplugin.zsh

# Dependencies go first
zpl light mafredri/zsh-async

# load plugins from enabled topics
_load_all plugins.zsh

# TODO: Stop zplugin trying to compile binaries
zpl ice from"gh-r" as"program"; zpl light junegunn/fzf-bin
zpl light zsh-users/zsh-history-substring-search
zpl light zdharma/history-search-multi-word
zpl light supercrabtree/k
zpl light djui/alias-tips
zpl light eduarbo/simpl

export ZSH_AUTOSUGGEST_USE_ASYNC=1
export ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20
zpl light zsh-users/zsh-autosuggestions

zpl ice blockf; zpl light zsh-users/zsh-completions # Disallow zsh-completions to modify fpath
[[ -z $SSH_CONNECTION ]] && zpl light zdharma/fast-syntax-highlighting


#
# Speed up zsh load
#

# Load and initialize the completion system with a cache time of 20 hours, so it
# should almost always regenerate the first time a shell is opened each day.
# Compinit should be called after loading of all plugins and before possibly calling cdreply
autoload -Uz compinit

zcompfiles=($ZPLGM[ZCOMPDUMP_PATH](Nm-20))
if (( $#zcompfiles )); then
  # -C  ignore checking at all
  compinit -C -d $ZPLGM[ZCOMPDUMP_PATH]
else
  compinit -d $ZPLGM[ZCOMPDUMP_PATH]
fi

# Execute code that does not affect the current session in the background.
{
  # Compile the completion dump to increase startup speed
  if [[ "${ZPLGM[ZCOMPDUMP_PATH]}" -nt "${ZPLGM[ZCOMPDUMP_PATH]}.zwc" || ! -s "${ZPLGM[ZCOMPDUMP_PATH]}.zwc" ]]; then
    zcompile "${ZPLGM[ZCOMPDUMP_PATH]}"
  fi
} &!

# execute compdefs provided by rest of plugins
zpl cdreplay -q # -q is for quiet


#
# configs
#

_load shell/zsh/config.zsh
_load shell/zsh/completion.zsh
_load shell/zsh/keybinds.zsh

# load aliases from enabled topics
# source them after compinit to be able to use compdef
_load_all aliases.zsh

export _FASD_DATA="$XDG_CACHE_HOME/fasd"
export _FASD_VIMINFO="$XDG_CACHE_HOME/viminfo"
_cache fasd --init posix-alias zsh-{hook,{c,w}comp{,-install}}

# vim:set ft=sh:

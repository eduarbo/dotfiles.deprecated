# zshrc
# By Eduardo Ruiz <eduarbo@gmail.com>

#
# zplugin
#

# I choosed zplugin as my plugin manager for being the fastest between antibody,
# antigen and zplug in my tests. Zplug is the worst! forget to include it in
# future tests

# Load zplugin and install it when missing
_load_repo zdharma/zplugin $ZPL_DIR zplugin.zsh

zplugin light mafredri/zsh-async # dependency

_load_all plugins.zsh
zplugin ice from"gh-r" as"program"; zplugin light junegunn/fzf-bin
zplugin light zsh-users/zsh-autosuggestions
zplugin light zsh-users/zsh-history-substring-search
zplugin ice blockf; zplugin light zsh-users/zsh-completions # Disallow zsh-completions to modify fpath
zplugin light zdharma/history-search-multi-word
zplugin light eduarbo/simpl
[[ -z $SSH_CONNECTION ]] && zplugin light zdharma/fast-syntax-highlighting

_load shell/zsh/config.zsh
_load shell/zsh/completion.zsh
_load shell/zsh/keybinds.zsh

# Load and initialize the completion system with a cache time of 20 hours, so it
# should almost always regenerate the first time a shell is opened each day.
# Compinit should be called after loading of all plugins and before possibly calling cdreply
autoload -Uz compinit

zcompdump="$ZSH_CACHE"/zcompdump
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

#
export _FASD_DATA="$XDG_CACHE_HOME/fasd"
export _FASD_VIMINFO="$XDG_CACHE_HOME/viminfo"
_cache fasd --init posix-alias zsh-{hook,{c,w}comp{,-install}}

#
_load_all aliases.zsh

# vim:set ft=sh:

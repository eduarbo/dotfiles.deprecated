# zshrc
# By Eduardo Ruiz <eduarbo@gmail.com>

# Load zplugin and install it when missing
_load_repo zdharma/zplugin $ZPL_DIR zplugin.zsh

zplugin light mafredri/zsh-async # dependency

_load_all plugins.zsh
zplugin ice from"gh-r" as"program"; zplugin light junegunn/fzf-bin
zplugin light zsh-users/zsh-history-substring-search
zplugin light zdharma/history-search-multi-word
zplugin light zsh-users/zsh-autosuggestions
zplugin ice blockf; zplugin light zsh-users/zsh-completions # Disallow zsh-completions to modify fpath
zplugin light supercrabtree/k
zplugin light eduarbo/simpl
[[ -z $SSH_CONNECTION ]] && zplugin light zdharma/fast-syntax-highlighting

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
zplugin cdreplay -q # -q is for quiet

_load shell/zsh/config.zsh
_load shell/zsh/completion.zsh
_load shell/zsh/keybinds.zsh

#
_load_all aliases.zsh

# vim:set ft=sh:

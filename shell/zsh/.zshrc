# zshrc
# By Eduardo Ruiz <eduarbo@gmail.com>

# Set vi style bindings before sourcing fzf to prevent reset for TAB key binding
bindkey -v

#
# Plugins
#

# Load zplugin and install it when missing
_load_repo zdharma/zplugin $ZPL_DIR zplugin.zsh

# load plugins from enabled topics
_load_all plugins.zsh

# TODO: Stop zplugin trying to compile binaries
zpl ice from"gh-r" as"program"; zpl light junegunn/fzf-bin
zpl ice as"command" pick"bin/fzf-tmux"; zplugin load junegunn/fzf
zpl snippet "https://raw.githubusercontent.com/junegunn/fzf/master/shell/completion.zsh"
zpl snippet "https://raw.githubusercontent.com/junegunn/fzf/master/shell/key-bindings.zsh"
zpl light zsh-users/zsh-history-substring-search
zpl light zdharma/history-search-multi-word
zpl light supercrabtree/k
zpl light djui/alias-tips

typeset -gA PROMPT_SIMPL_HOSTNAME_SYMBOL_MAP
PROMPT_SIMPL_HOSTNAME_SYMBOL_MAP=(
  eduarbook "ᚱ"
  lavos "𝔫𝔰"
  other "ᛟ"
  htpc "♆"
)
export SIMPL_GIT_DIRTY_SYMBOL="⌁"
zpl ice pick"async.zsh" src"simpl.zsh"
zpl light eduarbo/simpl

export ZSH_AUTOSUGGEST_USE_ASYNC=1
export ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20
zpl light zsh-users/zsh-autosuggestions

zpl ice blockf; zpl light zsh-users/zsh-completions # Disallow zsh-completions to modify fpath
[[ -z $SSH_CONNECTION ]] && zpl light zdharma/fast-syntax-highlighting

#
# configs
#

# ensure EXTENDED_GLOB is set before looking for expired zcompdump with glob
# qualifiers
_load shell/zsh/config.zsh
_load shell/zsh/completion.zsh
_load shell/zsh/keybinds.zsh
_load shell/zsh/fzf.zsh
_load ~/.secrets

#
# Speed up zsh load
#

# Compinit should be called after loading of all plugins and before possibly calling cdreply
autoload -Uz compinit

# Load and initialize the completion system with a cache time of 20 hours, so it
# should almost always regenerate the first time a shell is opened each day.
# The globbing is a little complicated here:
#
# - '#q' is an explicit glob qualifier that makes globbing work within zsh's [[ ]] construct.
# - 'N' makes the glob pattern evaluate to nothing when it doesn't match (rather than throw a globbing error)
# - '.' matches "regular files"
# - 'mh+20' matches files (or directories or whatever) that are older than 20 hours.
if [[ -n $ZPLGM[ZCOMPDUMP_PATH](#qN.mh+20) ]]; then
	compinit -d $ZPLGM[ZCOMPDUMP_PATH];
  # update the timestamp on compdump file
  compdump
else
	compinit -C -d $ZPLGM[ZCOMPDUMP_PATH];
fi;

# Execute code that does not affect the current session in the background.
{
  # Compile the completion dump to increase startup speed
  if [[ "${ZPLGM[ZCOMPDUMP_PATH]}" -nt "${ZPLGM[ZCOMPDUMP_PATH]}.zwc" || ! -s "${ZPLGM[ZCOMPDUMP_PATH]}.zwc" ]]; then
    zcompile "${ZPLGM[ZCOMPDUMP_PATH]}"
  fi
} &!

# execute compdefs provided by rest of plugins
zpl cdreplay -q # -q is for quiet

# load aliases from enabled topics
# source them after compinit to be able to use compdef
_load_all aliases.zsh

export _FASD_DATA="$XDG_CACHE_HOME/fasd"
export _FASD_VIMINFO="$XDG_CACHE_HOME/viminfo"
_cache fasd --init posix-alias zsh-{hook,{c,w}comp{,-install}}

# vim:set ft=sh:

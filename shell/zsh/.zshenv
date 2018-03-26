# This file is sourced by all instances of Zsh, and thus, it should be kept as
# small as possible and should only define environment variables.

unsetopt GLOBAL_RCS  # disable global zsh config; we'll handle it ourselves
source $(cd ${${(%):-%x}:A:h}/../.. && pwd -P)/env

# Move ZDOTDIR from $HOME to reduce dotfile pollution.
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"
export ZPL_DIR="$XDG_CACHE_HOME/zpl"
export ZGEN_DIR="$XDG_CACHE_HOME/zgen"
export ZSH_CACHE="$XDG_CACHE_HOME/zsh"

# Ensure path arrays do not contain duplicates
typeset -gU cdpath fpath mailpath manpath path
# typeset -gUT INFOPATH infopath
path=( $XDG_BIN_HOME $DOTFILES/sell/bin $DOTFILES_DATA/*.topic/bin(N) $path )
fpath=( $ZDOTDIR/functions $XDG_BIN_HOME $fpath )

# envvars
export SHELL=$(command -v zsh)
export LANG=${LANG:-en_US.UTF-8}
export PAGER=less
export LESS='-R -i -w -M -z-4'
export LESSHISTFILE="$XDG_DATA_HOME/lesshst"
export PASSWORD_STORE_DIR="$XDG_DATA_HOME/password-store"

# initialize enabled topics
_load_all env.zsh
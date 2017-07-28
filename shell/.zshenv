# This file is sourced by all instances of Zsh, and thus, it should be kept as
# small as possible and should only define environment variables.

# Start shell startup timer
export LOAD_TIME_START=$(/usr/local/bin/gdate +%s%N 2> /dev/null || date +%s%N)

# Ensure path arrays do not contain duplicates.
typeset -gU cdpath fpath mailpath manpath path
typeset -gUT INFOPATH infopath
path=( /usr/local/{,s}bin /usr/{,s}bin /{,s}bin $path )

case "$OSTYPE" in
  darwin*)
    source "$(dirname $(greadlink -f ${(%):-%N}))/lib/common.sh" ;;
  linux*)
    source "$(dirname $(readlink -f ${(%):-%N}))/lib/common.sh" ;;
esac

path=("$ENABLED_DIR"/*/bin $path)
load shell/lib/env.options.zsh
loadall env.zsh

export LESS='-g -i -M -R -S -w -z-4'
if is_callable lesspipe; then
  export LESSOPEN='| /usr/bin/env lesspipe %s 2>&-'
fi

case "$OSTYPE" in
  darwin*)
    load shell/lib/env.macos.sh ;;
  linux*)
    load shell/lib/env.linux.sh ;;
esac

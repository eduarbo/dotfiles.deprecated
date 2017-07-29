# This file is sourced by all instances of Zsh, and thus, it should be kept as
# small as possible and should only define environment variables.

# Start shell startup timer
LOAD_TIME_START=$(/usr/local/bin/gdate +%s%N 2> /dev/null || date +%s%N)
export LOAD_TIME_START

# Ensure path arrays do not contain duplicates
typeset -gU cdpath fpath mailpath manpath path
typeset -gUT INFOPATH infopath

source "$HOME"/.utils.sh

load shell/lib/options.zsh
load shell/lib/env.sh
loadall env.sh
loadall env.zsh

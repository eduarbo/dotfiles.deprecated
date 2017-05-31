#!/usr/bin/env zsh
# This file is sourced by all instances of Zsh, and thus, it should be kept as
# small as possible and should only define environment variables.

# Start shell startup timer
export LOAD_TIME_START=$(/usr/local/bin/gdate +%s%N 2> /dev/null || date +%s%N)

source $HOME/.dotfiles/bash/lib/env


TMPPREFIX="${TMPDIR%/}/zsh"
if [[ ! -d "$TMPPREFIX" ]]; then
  mkdir -p "$TMPPREFIX"
fi

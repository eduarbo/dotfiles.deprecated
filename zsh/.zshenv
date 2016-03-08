#!/usr/bin/env zsh
# This file is sourced by all instances of Zsh, and thus, it should be kept as
# small as possible and should only define environment variables.
source "$HOME/.dotfiles/lib/common_env"

TMPPREFIX="${TMPDIR%/}/zsh"
if [[ ! -d "$TMPPREFIX" ]]; then
  mkdir -p "$TMPPREFIX"
fi

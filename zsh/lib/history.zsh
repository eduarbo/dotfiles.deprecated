#!/bin/zsh
# Keep 9999 lines of history within the shell and save it to ~/.zsh_history:
HISTSIZE=9999
SAVEHIST=9999
HISTFILE=~/.zsh_history

setopt appendhistory
setopt extendedhistory
setopt histexpiredupsfirst
setopt histignorealldups
setopt histignoredups
setopt histignorespace
setopt histverify
setopt incappendhistory
setopt sharehistory

#!/bin/bash
source $HOME/.dotfiles/bash/lib/env

# Ignore duplicate commands in the history
export HISTCONTROL=ignoredups:erasedups

# Increase the maximum number of lines contained in the history file
# (default is 500)
export HISTFILESIZE=500000

# Make some commands not show up in history
export HISTIGNORE="l[sla]:rm:mv:mkdir:cd:[bf]g:exit:logout"

# Don't clear the screen after quitting a manual page
export MANPAGER="less -X"

# Make new shells get the history lines from all previous
# shells instead of the default "last window closed" history
export PROMPT_COMMAND="history -a; $PROMPT_COMMAND"

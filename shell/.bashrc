# Start shell startup timer
LOAD_TIME_START=$(/usr/local/bin/gdate +%s%N 2> /dev/null || date +%s%N)
export LOAD_TIME_START

[ -n "$PS1" ] && source ~/.bash_profile

# cache fasd --init posix-alias bash-hook bash-ccomp bash-ccomp-install

load shell/lib/aliases.sh
load shell/lib/prompt.bash
# load shell/lib/prompt.hlissner.bash
load shell/lib/completion.bash

source_file "$HOME"/.secrets
source_file "$HOME"/.bashrc.local

_stop_startup_timer

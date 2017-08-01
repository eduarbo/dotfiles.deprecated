addpath "$ENABLED_DIR"/*/bin /usr/local/{,s}bin /usr/{,s}bin /{,s}bin

export EDITOR=$(is_callable nvim && echo 'nvim' || echo 'vim')
export VISUAL=$EDITOR
export GPG_TTY=$(tty)
export PAGER="less"

export LSCOLORS="exfxcxdxbxbxbxbxbxbxbx"
export LS_COLORS="di=34;40:ln=35;40:so=32;40:pi=33;40:ex=31;40:bd=31;40:cd=31;40:su=31;40:sg=31;40:tw=31;40:ow=31;40:"
export CLICOLOR=1
export CLICOLOR_FORCE=1

export HISTSIZE='32768';           # Increase history size. Allow 32³ entries
export HISTFILESIZE="${HISTSIZE}"
export SAVEHIST="${HISTSIZE}"      # The maximum number of events to save in the history file
export HISTFILE="$HOME"/.history   # The path to the history file

 # Ignore duplicates and commands that begin with a space from history
export HISTCONTROL=ignoreboth:erasedups
# Make some commands not show up in history
export HISTIGNORE="l[sla]:rm:mv:mkdir:cd:[bf]g:exit:logout"

export LESS_TERMCAP_md="${yellow}" # Highlight section titles in manual pages
export MANPAGER='less -X'          # Don’t clear the screen after quitting a manual page

# Make new shells get the history lines from all previous shells instead of the
# default "last window closed" history
export PROMPT_COMMAND="history -a; $PROMPT_COMMAND"

# # Save and reload the history after each command finishes
# export PROMPT_COMMAND="history -a; history -c; history -r; $PROMPT_COMMAND"

[[ $LANG ]] || export LANG='en_US.UTF-8'
[[ $LC_ALL ]] || export LC_ALL='en_US.UTF-8'

# # Set the default Less options.
# # Mouse-wheel scrolling has been disabled by -X (disable screen clearing).
# # Remove -X and -F (exit if the content fits on one screen) to enable it.
# export LESS='-F -g -i -M -R -S -w -X -z-4'

export LESS='-g -i -M -R -S -w -z-4'
if is_callable lesspipe; then
  export LESSOPEN='| /usr/bin/env lesspipe %s 2>&-'
fi

# set 256 color profile where possible
if [[ $COLORTERM == gnome-* && $TERM == xterm ]] && infocmp gnome-256color >/dev/null 2>&1; then
  export TERM=gnome-256color
elif infocmp xterm-256color >/dev/null 2>&1; then
  export TERM=xterm-256color
fi

if [[ ! -d "$TMPDIR" ]]; then
  export TMPDIR="$(mktemp -d)"
fi
export TMPPREFIX="${TMPDIR%/}/zsh"

BORG_REPO="/media/nas/backups/bak"
[[ $(hostname) != "lab" ]] && BORG_REPO="eduarbo@192.168.0.10:$BORG_REPO"
export BORG_REPO

case "$OSTYPE" in
  darwin*)
    load shell/lib/env.macos.sh ;;
  linux*)
    load shell/lib/env.linux.sh ;;
esac

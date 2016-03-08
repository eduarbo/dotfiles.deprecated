#!/usr/bin/env bash
export DOT="$HOME/.dotfiles"
export OS_NAME="$(uname)"

# source and evaluate a command if it is passed as second argument
source_file () {
  [[ -s "$1" ]] && source "$1"
  [[ "$2" && "$(type -p "$2")" ]] && eval "$2"
}

type_exists() {
  [[ "$(type -p $1)" ]] && return 0
  return 1
}

if [[ "$OS_NAME" == 'Linux' ]]; then
  export IS_LINUX=1
fi

if [[ "$OS_NAME" == 'Darwin' ]]; then
  export IS_MAC=1
fi

if [[ -x "$(which brew)" ]]; then
  export HAS_BREW=1
  # BREW_LOCATION=`brew --prefix`
  export BREW_LOCATION="/usr/local"
elif [[ -x "$(which apt-get)" ]]; then
  export HAS_APT=1
elif [[ -x "$(which yum)" ]]; then
  export HAS_YUM=1
fi
export PATH="\
  /usr/local/bin:\
  $HOME/lib/sbt/bin:\
  $PATH:\
  $HOME/bin:\
  /usr/share:\
  "&>/dev/null

# Exports ------------------------------------------------------------------ {{{
# Make vim the default editor
export EDITOR="vim"
export VISUAL=$EDITOR

export HISTFILE="$HOME/.history"

# Ignore duplicate commands in the history
export HISTCONTROL=ignoredups:erasedups

# Increase the maximum number of lines contained in the history file
# (default is 500)
export HISTFILESIZE=500000

# Increase the maximum number of commands to remember
# (default is 500)
export HISTSIZE=100000

# Make some commands not show up in history
export HISTIGNORE="l[sla]:rm:mv:mkdir:cd:[bf]g:exit:logout"

# Don't clear the screen after quitting a manual page
export MANPAGER="less -X"

if [[ $HAS_BREW ]]; then
  # Export PhantomJS bin location (be explicit in case Homebrew is not installed
  # in the default location)
  if [ -f "$BREW_LOCATION/bin/phantomjs" ]; then
    export PHANTOMJS_BIN="$BREW_LOCATION/bin/phantomjs"
  fi
fi

# Make new shells get the history lines from all previous
# shells instead of the default "last window closed" history
export PROMPT_COMMAND="history -a; $PROMPT_COMMAND"

# some settings to be more colorful
export CLICOLOR=1
export GREP_OPTIONS='--color=auto' GREP_COLOR='1;32'
export CLICOLOR=true
export LSCOLORS=ExGxFxdxCxDxDxBxBxExEx

export LESS="-R"
[[ -z "$DISPLAY" ]] && export DISPLAY=":0.0"

# set 256 color profile where possible
if [[ "$COLORTERM" == gnome-* && "$TERM" == xterm ]] && infocmp gnome-256color >/dev/null 2>&1; then
  export TERM=gnome-256color
elif infocmp xterm-256color >/dev/null 2>&1; then
  export TERM=xterm-256color
fi
# }}}
# Source ------------------------------------------------------------------- {{{
type_exists "fasd" && eval "$(fasd --init auto)"

source_file "$HOME/.nvm/nvm.sh"
source_file "$HOME/.avn/bin/avn.sh"

if [[ "$HAS_BREW" ]]; then
  source_file "$BREW_LOCATION/share/chruby/chruby.sh" "chruby ruby"

  # FZF {{{
  export FZF_DEFAULT_OPTS="--extended --cycle"

  # Setting ag as the default source for fzf
  export FZF_DEFAULT_COMMAND='
  (git ls-tree -r --name-only HEAD ||
    ag -l -g "" ||
    find * -name ".*" -prune -o -type f -print -o -type l -print) 2> /dev/null'

  # To apply the command to CTRL-T as well
  export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"

  source_file "$BREW_LOCATION/etc/bash_completion"
  source_file "$BREW_LOCATION/opt/fzf/shell/completion.bash"
  source_file "$BREW_LOCATION/opt/fzf/shell/key-bindings.bash"
  # }}}
else
  echo "Can't find Homebrew"
fi

source "$DOT/bash/.aliases"
source "$DOT/bin/init-gpg-agent"
# }}}
# Prompt ------------------------------------------------------------------- {{{
prompt () {
    local BGBLACK="\[\033[40m\]"
    local BGRED="\[\033[41m\]"
    local BGGREEN="\[\033[42m\]"
    local BGYELLOW="\[\033[43m\]"
    local BGBLUE="\[\033[44m\]"
    local BGMAGENTA="\[\033[45m\]"
    local BGCYAN="\[\033[46m\]"
    local BGWHITE="\[\033[47m\]"
    local BGTRANS="\[\033[49m\]"

    local BLACK="\[\033[30m\]"
    local RED="\[\033[31m\]"
    local GREEN="\[\033[32m\]"
    local YELLOW="\[\033[33m\]"
    local BLUE="\[\033[34m\]"
    local MAGENTA="\[\033[35m\]"
    local CYAN="\[\033[36m\]"
    local WHITE="\[\033[37m\]"
    local TRANS="\[\033[39m\]"

    local BOLD="\[\033[1m\]"
    local RESET="\[\033[0m\]"

    export PS1="\n${BOLD}${GREEN}\w${YELLOW}\$(__git_ps1 ' on ${RESET}${YELLOW}%s') ${RESET}${YELLOW}❱${BOLD}${YELLOW}❱${GREEN}❱ ${RESET}"

    export PS2="${BOLD}${YELLOW}❯ ${RESET}"
}

prompt
# }}}
# Options ------------------------------------------------------------------ {{{
# Autocorrect typos in path names when using `cd`
shopt -s cdspell

# Check the window size after each command and, if necessary, update the values
# of LINES and COLUMNS.
shopt -s checkwinsize

# append to the history file, don't overwrite it
shopt -s histappend >/dev/null 2>&1

# Case-insensitive globbing (used in pathname expansion)
shopt -s nocaseglob

# Node Completion - Auto-generated, do not touch.
shopt -s progcomp
# }}}
# Customize to your needs -------------------------------------------------- {{{
# no duplicates in bash history and ignore same sucessive entries.
set match-hidden-files off
set page-completions off
set completion-query-items 350

# Enable some Bash 4 features when possible:
# * `autocd`, e.g. `**/qux` will enter `./foo/bar/baz/qux`
# * Recursive globbing, e.g. `echo **/*.txt`
for option in autocd globstar; do
  shopt -s "$option" 2> /dev/null
done

# Autocomplete for 'g' as well
complete -o default -o nospace -F _git g
# }}}

source_file "$HOME/.secrets"
source_file "$HOME/.bash_profile.local"

# vim: set ft=sh :

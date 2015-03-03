#!/usr/bin/env bash
DOT=$HOME/.dotfiles

source $DOT/lib/checks

export PATH="\
/usr/local/bin:\
$PATH:\
$HOME/bin:\
/usr/share:\
$HOME/lib/sbt/bin:\
"&>/dev/null

_brew_prefix='/usr/local'
# Exports ------------------------------------------------------------------ {{{
# Make vim the default editor
export EDITOR="vim"
export VISUAL=$EDITOR

export HISTFILE=~/.history

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
  if [ -f $_brew_prefix/bin/phantomjs ]; then
    export PHANTOMJS_BIN="$_brew_prefix/bin/phantomjs"
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
[[ -z $DISPLAY ]] && export DISPLAY=":0.0"

# set 256 color profile where possible
if [[ $COLORTERM == gnome-* && $TERM == xterm ]] && infocmp gnome-256color >/dev/null 2>&1; then
    export TERM=gnome-256color
elif infocmp xterm-256color >/dev/null 2>&1; then
    export TERM=xterm-256color
fi
# }}}
# Source ------------------------------------------------------------------- {{{
if [[ $HAS_BREW ]]; then
  _files=(
  $_brew_prefix/etc/bash_completion
  )
  for file in $_files; do
    [[ -f $file ]] && source $file
  done
  unset _files
fi

source "$DOT/bash/aliases"
source "$DOT/bash/prompt"
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

unset _brew_prefix;

[[ -e "$HOME/.secrets" ]] && source $HOME/.secrets
# Bash-specific
[[ -e "$HOME/.bash_profile.local" ]] && source $HOME/.bash_profile.local
# }}}

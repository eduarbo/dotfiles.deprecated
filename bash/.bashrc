#!/bin/bash
source "$HOME/.bash_profile"
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

source "$DOT/lib/aliases"
source "$DOT/lib/init-gpg-agent"
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

    export PS1="\n${BOLD}${GREEN}\w${YELLOW}\$(type_exists __git_ps1 && __git_ps1' on ${RESET}${YELLOW}%s') ${RESET}${YELLOW}❱${BOLD}${YELLOW}❱${GREEN}❱ ${RESET}"

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

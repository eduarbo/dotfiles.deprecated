DOTFILES="$(cd "$(dirname "$(dirname "$(dirname "$(greadlink -f "$0")")")")" && pwd)"
CACHE_DIR="$HOME/.cache/${SHELL##*/}"
ENABLED_DIR="$DOTFILES/.enabled.d"

is_callable()    { command -v "$1" >/dev/null; }
is_interactive() { [[ $- == *i* ]]; }
is_root()        { [[ "$UID" -eq 0 ]]; }
is_ssh()         { [[ $SSH_CONNECTION ]]; }

info()    { printf "\r[ \033[00;34m..\033[0m ] %s\n" "$1"; }
success() { printf "\r\033[2K[ \033[00;32mOK\033[0m ] %s\n" "$1"; }
fail()    { printf "\r\033[2K[\033[0;31mFAIL\033[0m] %s\n" "$1"; echo; exit; }

load() { source "$DOTFILES/$1"; }

# source and evaluate a command if it is passed as second argument
source_file() { [[ -s "$1" ]] && source "$1"; }

loadall() {
  local files=( "$ENABLED_DIR"/*/"${1:-*.*sh}" )
  for file in "${files[@]}"; do
    [[ -e "$file" ]] && source "$file"
  done
}

# Header logging
e_header() { printf "\n$(tput setaf 5)%s$(tput sgr0)\n" "$@"; }

# Success logging
e_success() { printf "$(tput setaf 10)✓ %s$(tput sgr0)\n" "$@"; }

# Error logging
e_error() { printf "$(tput setaf 9)x %s$(tput sgr0)\n" "$@"; }

# Warning logging
e_warning() { printf "$(tput setaf 11)! %s$(tput sgr0)\n" "$@"; }

# Ask for confirmation before proceeding
seek_confirmation() {
  printf "\n"
  e_warning "$@"
  read -rp "Continue? (y/n) " -n 1
  printf "\n"
}

# Test whether the result of an 'ask' is a confirmation
is_confirmed() {
  [[ "$REPLY" =~ ^[Yy]$ ]] && return 0
  return 1
}

cache() {
  [[ -z "$CACHE_DIR" ]] && error "Cache not set!"
  [[ -d "$CACHE_DIR" ]] || mkdir -p "$CACHE_DIR"

  is_callable "$1" || return 1
  local cache="${CACHE_DIR}/$1"
  if [[ ! -f "$cache" || ! -s "$cache" ]]; then
    echo "Caching $1"
    $@ > "$cache" 2> /dev/null
  fi
  source $cache
}

cache_clear() {
  [[ -d $CACHE_DIR ]] && rm -f "$CACHE_DIR"/*
}

repo() {
  [[ -d $2 ]] || git clone --recursive "https://github.com/$1" "$2"
}

_stop_startup_timer() {
  [[ -n "$DISABLE_LOAD_TIME" ]] && return 1
  local end
  # Stop shell startup timer
  end=$(/usr/local/bin/gdate +%s%N 2> /dev/null || date +%s%N)
  local total=$(((end - LOAD_TIME_START) / 1000000))

  printf "Load time: %0.2fs\n" "$(bc <<< scale=2\;$total/1000)"
}

load shell/lib/env.sh

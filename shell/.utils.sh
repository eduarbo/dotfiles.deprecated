case "$OSTYPE" in
  # Get the real path no matter where the dotfiles have been cloned
  darwin*)
    SOURCE="${BASH_SOURCE[0]:-$0}"
    while [ -h "$SOURCE" ]; do # resolve $SOURCE until the file is no longer a symlink
      DOTFILES="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
      SOURCE="$(readlink "$SOURCE")"
      [[ $SOURCE != /* ]] && SOURCE="$DOTFILES/$SOURCE" # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
    done
    export DOTFILES="$( cd -P "$(dirname "$(dirname "$SOURCE")")" && pwd )"
    ;;
  linux*) export DOTFILES="$(dirname "$(dirname "$(readlink -f "${BASH_SOURCE[0]:-$0}")")")" ;;
esac

export CACHE_DIR="$HOME/.cache/${SHELL##*/}"
export ENABLED_DIR="$DOTFILES/.enabled.d"

is_callable()    { command -v "$1" >/dev/null; }
is_interactive() { [[ $- == *i* ]]; }
is_root()        { [[ "$UID" -eq 0 ]]; }
is_ssh()         { [[ $SSH_CONNECTION ]]; }

load() { source "$DOTFILES/$1"; }

loadall() {
  local files=( "$ENABLED_DIR"/*/"${1:-*.*sh}" )
  for file in "${files[@]}"; do
    [[ -e "$file" ]] && source "$file"
  done
}

# source and evaluate a command if it is passed as second argument
source_file() { [[ -s "$1" ]] && source "$1"; }

source_and_eval () {
  if [[ -s "$1" ]]; then
    source "$1"
    test "$2" && type_exists "$2" && eval "$2"
  fi
}

lazy_source () {
  eval "$1 () { [ -f $2 ] && source $2 && $1 \$@ }"
}

# Test whether a Homebrew formula is already installed
# $1 - formula name (may include options)
formula_exists() {
  test "$(brew list "$1" 2&>/dev/null)" && return 0
  return 1
}

# Logging

e_header() { printf "\n$(tput setaf 5)%s$(tput sgr0)\n" "$@"; }
e_success() { printf "$(tput setaf 10)✓ %s$(tput sgr0)\n" "$@"; }
e_error() { printf "$(tput setaf 9)x %s$(tput sgr0)\n" "$@"; }
e_warning() { printf "$(tput setaf 11)! %s$(tput sgr0)\n" "$@"; }

_info()    { printf "\r[ \033[00;34m..\033[0m ] %s\n" "$1"; }
_success() { printf "\r\033[2K[ \033[00;32mOK\033[0m ] %s\n" "$1"; }
_fail()    { printf "\r\033[2K[\033[0;31mFAIL\033[0m] %s\n" "$1"; echo; exit; }

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

addpath() {
  for bin in "$@"; do
    case ":$PATH:" in
      *:"$bin":*) ;; # already there
      *) export PATH="$bin:$PATH";;
    esac
  done
}

cleanpath() {
  if [ -n "$PATH" ]; then
    OLDPATH=$PATH:; PATH=
    while [ -n "$OLDPATH" ]; do
      x=${OLDPATH%%:*}       # the first remaining entry
      case $PATH: in
        *:"$x":*) ;;        # already there
        *) PATH=$PATH:$x;;  # not there yet
      esac
      OLDPATH=${OLDPATH#*:}
    done
    PATH=${PATH#:}
    unset OLDPATH x
  fi
}

installcmd() {
  $installcmd "$1"
}

install-deps() {
  local osdepsvar="${PACKAGE_MANAGER}_deps"
  local osdeps="${osdepsvar}[@]"

  # prevent install deps in unsupported package managers
  [[ ${SUPPORTED_PACKAGE_MANAGERS[@]} =~ ${PACKAGE_MANAGER} ]] || return 1

  [[ -n $basedeps ]] && $installcmd "${basedeps[@]}"

  if [[ -n ${!osdepsvar} ]]; then
    installcmd "${!osdeps}"
  elif [[ -n $deps ]]; then
    installcmd "${deps[@]}"
  fi
}

pop-and-missing() {
  local pkg=$1
  local pm=$2
  local depsclone=$deps
  # fail if the available PM is not the same as the passed one
  [[ $# -eq 2 && $pm != ${PACKAGE_MANAGER} ]] && return 1
  # fail when deps is empty
  [[ $deps ]] || return 1
  # Pop pkg from array
  deps=( ${deps[@]#$pkg} )
  # Compare arrays and fail if no pkg was popped
  [[ ${depsclone[@]} == ${deps[@]} ]] && return 1
  ! is_callable "$pkg"
}

if [[ -x "$(which brew)" ]]; then
  # BREW_LOCATION=`brew --prefix`
  export BREW_LOCATION="/usr/local"
fi

case "$OSTYPE" in
  darwin*)
    export PACKAGE_MANAGER=brew
    installcmd="brew install"
    ;;
  linux*)
    if [[ -f /etc/arch-release ]]; then
      export PACKAGE_MANAGER=pacman
      installcmd="sudo pacman --needed --noconfirm -S"
    elif [[ -f /etc/fedora-release ]]; then
      if is_callable dnf; then
        export PACKAGE_MANAGER=dnf
        installcmd="sudo dnf install -y"
      else
        export PACKAGE_MANAGER=yum
        installcmd="sudo yum -y install"
      fi
    elif [[ -f /etc/debian-version ]]; then
      if is_callable apt; then
        export PACKAGE_MANAGER=apt
        installcmd="apt install -y"
      else
        export PACKAGE_MANAGER=apt-get
        installcmd="apt-get install -y"
      fi
    elif [[ -f /etc/SuSE-release ]]; then
      export PACKAGE_MANAGER=zypper
      installcmd="zypper install -y"
    fi
    ;;
esac

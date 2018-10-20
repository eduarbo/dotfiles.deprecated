
_init_path() {
  # if running bash
  if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
      . "$HOME/.bashrc"
    fi
  fi

  shopt -s nullglob
  paths=( ~/.local/bin $DOTFILES/bin $DOTFILES_CACHE/*.topic/bin )
  export PATH="$(printf '%s:' "${paths[@]}"):$PATH"
}

_init_path

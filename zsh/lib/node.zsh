# Loads the Node Version Manager and enables npm completion.

# Load NVM into the shell session.
if [[ -s "$HOME/.nvm/nvm.sh" ]]; then
  source "$HOME/.nvm/nvm.sh"
fi

# Return if requirements are not found.
if (( ! $+commands[node] )); then
  return 1
fi

# Load NPM completion.
eval "$(npm completion 2>/dev/null)"

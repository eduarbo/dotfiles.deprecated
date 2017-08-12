export NVM_NO_USE=false
export NVM_LAZY_LOAD=true
export NVM_AUTO_USE=true
zplugin light lukechilds/zsh-nvm

if is_interactive; then
  zplugin light lukechilds/zsh-better-npm-completion
fi

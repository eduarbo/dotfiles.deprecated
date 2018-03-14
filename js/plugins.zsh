export NVM_NO_USE=false
export NVM_LAZY_LOAD=false
export NVM_AUTO_USE=true
zplugin ice lucid wait'1'
zplugin light lukechilds/zsh-nvm

if is_interactive; then
  zplugin light lukechilds/zsh-better-npm-completion
fi

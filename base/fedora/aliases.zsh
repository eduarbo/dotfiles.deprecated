alias ls="${aliases[ls]:-ls} --color=auto --group-directories-first"

alias sc=systemctl
alias jc=journalctl
alias ssc='sudo systemctl'

alias reboot='sudo systemctl reboot'
alias shutdown='sudo systemctl poweroff'

alias di='sudo dnf install'
alias diy='sudo dnf install -y'
alias dr='sudo dnf remove -y'
alias dc='sudo dnf clean all && sudo dnf autoremove'

# Clipboard pipes
if _is_callable xclip; then
  alias y='xclip -selection clipboard -in'
  alias p='xclip -selection clipboard -out'
elif _is_callable xsel; then
  alias y='xsel -i --clipboard'
  alias p='xsel -o --clipboard'
fi

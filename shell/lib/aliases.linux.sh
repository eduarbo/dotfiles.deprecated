alias ls="${aliases[ls]:-ls} --color=auto --group-directories-first"

alias sys='systemctl'
alias sys!='sudo systemctl'

alias localip='ip route get 1 | awk "{print \$NF;exit}"'

if (( $+commands[xclip] )); then
    alias y='xclip -selection clipboard -in'
    alias p='xclip -selection clipboard -out'
elif (( $+commands[xsel] )); then
    alias y='xsel --clipboard --input'
    alias p='xsel --clipboard --output'
fi

alias reboot="systemctl reboot"
alias shutdown="systemctl poweroff"

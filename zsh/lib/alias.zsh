#!/bin/zsh
# Detect which `ls` flavor is in use
if ls --color > /dev/null 2>&1; then # GNU `ls`
	colorflag="--color"
else # OS X `ls`
	colorflag="-G"
fi

# Shortcuts ---------------------------------------------------------------- {{{
alias open='xdg-open'
alias o='xdg-open'
alias scp='rsync --rsh=ssh -CarvP'
alias top='top -o cpu'

alias dl="cd ~/Downloads"
alias dt="cd ~/Desktop"
alias dev='cd ~/dev'

alias ef='vim ~/.config/fish/config.fish'
alias ev='vim ~/.vimrc'
alias ed='vim ~/.vim/custom-dictionary.utf-8.add'
alias eo='vim ~/Dropbox/Org'
alias ek='vim ~/lib/dotfiles/keymando/keymandorc.rb'
alias et='vim ~/.tmux.conf'
alias eg='vim ~/.gitconfig'
alias e='emacsclient -t'
alias ec='emacsclient -c'

alias r='rails'
alias rg='rails g'
alias rn='rails new'
alias rs='rails s'
alias rc='rails c'

alias v='gvim'
alias V='gvim .'

alias n="node"
alias upd="sudo apt-get update"
alias install="sudo apt-get install"
alias remove="sudo apt-get remove"

alias j="z"
alias tm="tmux -u2"
alias cl="clear"

# Files & Directories ------------------------------------------------------ {{{
alias -- -='cd -'
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'

alias l="ls -l ${colorflag}" # List all files colorized in long format
alias la="ls -lA ${colorflag}" # List all files colorized in long format, including dot files
alias lsd='ls -l ${colorflag} | grep "^d"' # List only directories
alias ls="command ls -h ${colorflag}"
alias lt='ls -ltr'

alias md='mkdir -p'
alias rd='rmdir'
alias RM='rm -vrf'
# alias rm='mv --target-directory ~/.local/share/Trash/files'

duh() { # disk usage for humans
  test $# -eq 0 && set -- *
  du -sch "$@" | sort -h
}

bam() { # backup with move
  for file; do
    mv -v $file{,.bak}
  done
}

bum() { # undo backup move
  for file; do
    mv -v "$file" "${file%.bak}"
  done
}

bac() { # backup with copy
  for file; do
    cp -Rpv "$file" "$file~$(date -Ins)~"
  done
}

buc() { # undo backup copy
  for file; do
    dest=${file%%\~*}
    test -d "$dest" && mv -v "$dest" "$file.orig"
    mv -v "$file" "$dest"
  done
}

mount-dir-ro() {
  sudo mount --bind "$@" &&
  sudo mount -o remount,ro,bind "$@"
}
# }}}

# Git ---------------------------------------------------------------------- {{{
alias g='git'
alias gs='git status'
alias ga='git add'
alias gc='git commit'
alias gca='git commit -a'
alias gco='git checkout'
alias gd='git diff'
alias gl='git log'
alias gb='git branch'
alias gm='git merge'
alias gg='git grep --color -n'
alias ggi='git grep -ni'
alias gsa='git submodule add $argv=' end
alias gll="git log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit --date=relative"
alias gt='git stash'
alias gp='git stash apply'

# setopt nocompletealiases      # treat `gco` like `git checkout`
# compdef _git tig=git-checkout # treat `tig` like `git checkout`
# compdef hub=git               # treat `hub` like `git`
# }}}

# Misc --------------------------------------------------------------------- {{{
alias server='python -m SimpleHTTPServer'

alias ip="dig +short myip.opendns.com @resolver1.opendns.com"
alias localip="ipconfig getifaddr en1"
alias ips="ifconfig -a | grep -o 'inet6\? \(\([0-9]\+\.[0-9]\+\.[0-9]\+\.[0-9]\+\)\|[a-fA-F0-9:]\+\)' | sed -e 's/inet6* //'"

alias whois="whois -h whois-servers.net"

# Empty the Trash on all mounted volumes and the main HDD
# Also, clear Apple’s System Logs to improve shell startup speed
alias emptytrash="sudo rm -rfv /Volumes/*/.Trashes; sudo rm -rfv ~/.Trash; sudo rm -rfv /private/var/log/asl/*.asl"

# Copy my public key to my clipboard
alias pubkey="more ~/.ssh/id_rsa.pub | pbcopy | echo '=> Public key copied to pasteboard.'"

# Recursively delete `.DS_Store` files
alias cleanup="find . -type f -name '*.DS_Store' -ls -delete"

# }}}

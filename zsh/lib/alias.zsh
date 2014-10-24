#!/bin/zsh
# Detect which `ls` flavor is in use
if ls --color > /dev/null 2>&1; then # GNU `ls`
	colorflag="--color"
else # OS X `ls`
	colorflag="-G"
fi

# Shortcuts ---------------------------------------------------------------- {{{
# alias open='xdg-open'
alias o='open'
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

alias v='mvim'
alias V='mvim .'

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
alias gt='git tag -n'                       # show tags with <n> lines of each tag message

alias gc='git commit'
alias gcm='git commit -m'
alias gca='git commit -am'

alias gco='git checkout'                    # checkout
alias gnb='git checkout -b'                 # create and switch to a new branch (mnemonic: "git new branch branchname...")

alias gd='git diff'                         # diff unstaged changes
alias gdc='git diff --cached'               # diff staged changes

alias gl="git log --color --date=short --pretty=format:'%C(auto)%h %Cgreen%cD %C(cyan)%cr %Cblue[%cn]%C(auto)%d%n%Creset%B'"
alias glg="git log --color --graph --date=short --pretty=format:'%C(auto)%h %Cgreen%cD %C(cyan)%cr %Cblue[%cn]%C(auto)%d%n%Creset%B'"
alias gll="git log --color --date=short --pretty=format:'%C(auto)%h %Cgreen%cr %Cblue[%cn]%C(auto) %d%Creset%s'"
alias glm='git log --color --author="$(git config user.name)" --pretty=format:"%C(auto)%h %Cgreen%cr%Creset %s" --date=short'

alias gb='git branch'
alias gbb='git branch -v'
alias gm='git merge'
alias gf='git fetch'
alias gps='git push'
alias gpl='git pull'
alias grc='git rebase --continue'
alias grs='git rebase --skip'
alias gmt='git mergetool'                   # fire up the merge tool
alias gfl='git log -u'                      # show changes to a file

alias gg='git grep --break --heading --line-number'
alias ggi='git grep -ni'

# stash
alias gss='git stash'                       # stash changes
alias gsa='git stash apply'                 # apply stash
alias gsl='git stash list'                  # list stashes
alias gsp='git stash pop'                   # pop stash
alias gsd='git stash drop'                  # drop stashes

alias gcp='git cherry-pick -x'              # grab a change from a branch

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

alias todo="vim ~/todo.md"


# Google translate cli aliases
alias trse="trs {=es}"
alias trsf="trs {=fr}"
# }}}

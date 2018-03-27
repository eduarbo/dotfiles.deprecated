alias ls="gls -Fh --color --group-directories-first"
alias date=gdate

alias y=pbcopy
alias p=pbpaste

alias b=brew
alias bu="brew update && brew upgrade && brew cleanup"
alias c="brew cask"

alias open="open"
alias o="open"
alias oo="open ."

# Empty the Trash on all mounted volumes and the main HDD
# Also, clear Apple’s System Logs to improve shell startup speed
alias emptytrash="sudo rm -rfv /Volumes/*/.Trashes; sudo rm -rfv ~/.Trash; sudo rm -rfv /private/var/log/asl/*.asl"

# Get OS X Software Updates, and update installed Ruby gems, Homebrew, npm, and their installed packages
alias update="sudo softwareupdate -i -a; brew update; brew upgrade; brew cleanup; npm install npm -g; npm update -g; sudo gem update --system; sudo gem update"

# Recursively delete `.DS_Store` files
alias cleanup="find . -type f -name '*.DS_Store' -ls -delete"

alias unfuckcamera="sudo killall VDCAssistant"

# OS X has no `md5sum`, so use `md5` as a fallback
_is_callable md5sum  || alias md5sum="md5"
# OS X has no `sha1sum`, so use `shasum` as a fallback
_is_callable sha1sum || alias sha1sum="shasum"

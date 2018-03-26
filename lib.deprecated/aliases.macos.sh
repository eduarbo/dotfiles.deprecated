alias open="open"
alias o="open"
alias oo="open ."

# Empty the Trash on all mounted volumes and the main HDD
# Also, clear Apple’s System Logs to improve shell startup speed
alias emptytrash="sudo rm -rfv /Volumes/*/.Trashes; sudo rm -rfv ~/.Trash; sudo rm -rfv /private/var/log/asl/*.asl"

# Get OS X Software Updates, and update installed Ruby gems, Homebrew, npm, and their installed packages
alias update='sudo softwareupdate -i -a; brew update; brew upgrade; brew cleanup; npm install npm -g; npm update -g; sudo gem update --system; sudo gem update'

# Recursively delete `.DS_Store` files
alias cleanup="find . -type f -name '*.DS_Store' -ls -delete"

alias b="brew"
alias c="brew cask"

alias y='pbcopy'
alias p='pbpaste'

alias bru='brew update && brew upgrade && brew cleanup'

alias localip="ipconfig getifaddr en1"

alias unfuckcamera="sudo killall VDCAssistant"

# macOS has no `md5sum`, so use `md5` as a fallback
is_callable md5sum || alias md5sum="md5"
# macOS has no `sha1sum`, so use `shasum` as a fallback
is_callable sha1sum || alias sha1sum="shasum"

if is_callable gls; then
  alias ls='gls -Fh --color --group-directories-first'
else
  alias ls='ls -Fh --color'
fi

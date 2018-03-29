autoload -U zmv

zman() { PAGER="less -g -s '+/^       "$1"'" man zshall; }

r() {
  local time=$1; shift
  sched "$time" "notify-send --urgency=critical 'Reminder' '$@'; ding";
}; compdef r=sched

alias q="exit"
alias clr="clear"
# Allow aliases to be with sudo
alias sudo="sudo "

alias gurl="curl --compressed"
alias rsyncd="rsync -va --delete"   # Hard sync two directories
alias wget="wget -c"                # Resume dl if possible

alias pubkey="cat ~/.ssh/id_rsa.pub"

# Always enable colored `grep` output
# Note: `GREP_OPTIONS="--color=auto"` is deprecated, hence the alias usage.
alias grep="grep --color=auto"
alias fgrep="fgrep --color=auto"
alias egrep="egrep --color=auto"

alias ag="noglob ag -p $XDG_CONFIG_HOME/ag/agignore"
alias rg='noglob rg'

alias mk="make"

# For example, to list all directories that contain a certain file: find . -name
# .gitattributes | map dirname
alias map="xargs -n1"


#
# Shortcuts
#
alias scp="rsync --rsh=ssh -CarvP"
alias top="top -o cpu"

alias dl="cd ~/Downloads"
alias dt="cd ~/Desktop"
alias dev="cd ~/dev"

alias ec="v ~/.editorconfig"
alias ef="v ~/.config/fish/config.fish"
alias ej="v ~/.jshintrc"
alias todo="v ~/todo.md"
alias etp="v ~/tp.md"

alias cl="clear"

alias fa="fasd -a"        # any
alias fs="fasd -si"       # show / search / select
alias fd="fasd -d"        # directory
alias f="fasd -f"         # file
alias fsd="fasd -sid"     # interactive directory selection
alias fsf="fasd -sif"     # interactive file selection
alias j="fasd_cd -d"      # cd, same functionality as j in autojump
alias jj="fasd_cd -d -i"  # cd with interactive selection

alias d="docker"

# Reload the shell (i.e. invoke as a login shell)
alias reload="exec $SHELL -l"

# Lists the ten most used commands.
alias historystat="history 0 | awk '{print \$2}' | sort | uniq -c | sort -n -r | head"

alias ssh="TERM=xterm-256color ssh"


#
# Files & Directories
#

alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
alias ......="cd ../../../../.."
alias -- -="cd -"

alias ln="${aliases[ln]:-ln} -v"  # verbose ln

alias l="ls -1"
alias ll="ls -l" # List all files in long format
alias la="ls -lA" # List all files in long format, including dot files
alias lsd="ls -l | grep '^d'" # List only directories
alias lt="ls -ltr"

alias mkdir="mkdir -p"
alias md="mkdir -p"
alias rd="rmdir"
alias RM="rm -vrf"

# notify me before clobbering files
alias rm="rm -i"
alias cp="cp -i"
alias mv="mv -i"

# Securely Erase Files
alias shred="gshred -zuvn5"
alias vanish="shred"

#
# Misc
#

alias ip="dig +short myip.opendns.com @resolver1.opendns.com"
alias localip="ipconfig getifaddr en1"
alias ips="ifconfig -a | grep -o 'inet6\? \(\([0-9]\+\.[0-9]\+\.[0-9]\+\.[0-9]\+\)\|[a-fA-F0-9:]\+\)' | sed -e 's/inet6* //'"

alias whois="whois -h whois-servers.net"

# Copy my public key to my clipboard
alias pubkey="more ~/.ssh/id_rsa.pub | pbcopy | echo '=> Public key copied to pasteboard.'"

# Kill all the tabs in Chrome to free up memory
# [C] explained: http://www.commandlinefu.com/commands/view/402/exclude-grep-from-your-grepped-output-of-ps-alias-included-in-description
alias chromekill="ps ux | grep '[C]hrome Helper --type=renderer' | grep -v extension-process | tr -s ' ' | cut -d ' ' -f2 | xargs kill"

# Schedule sleep in X minutes, use like: sleep-in 60
sleep-in() {
  local minutes=$1
  local datetime=`date -v+${minutes}M +"%m/%d/%y %H:%M:%S"`
  sudo pmset schedule sleep "$datetime"
}

# Reload the sell and return the load time
loadtime() {
  export DISABLE_LOAD_TIME=1
  local times=${1:-1}
  for i in $(seq 1 "$times"); do
    /usr/bin/time $SHELL -lic exit;
  done
  unset DISABLE_LOAD_TIME
}

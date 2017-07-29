#
# These aliases are common to Bash and Zsh shells
#

alias q='exit'
alias clr='clear'
# Allow aliases to be with sudo
alias sudo="sudo "

alias gurl='curl --compressed'
alias rsyncd='rsync -va --delete'   # Hard sync two directories
alias wget='wget -c'                # Resume dl if possible

alias pubkey='cat ~/.ssh/id_rsa.pub'

# Always enable colored `grep` output
# Note: `GREP_OPTIONS="--color=auto"` is deprecated, hence the alias usage.
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

alias mk='make'

# For example, to list all directories that contain a certain file: find . -name
# .gitattributes | map dirname
alias map="xargs -n1"


# Shortcuts ---------------------------------------------------------------- {{{
alias scp='rsync --rsh=ssh -CarvP'
alias top='top -o cpu'

alias dl="cd ~/Downloads"
alias dt="cd ~/Desktop"
alias dev='cd ~/dev'

alias v='vim'
alias V='vim .'

alias ec='v ~/.editorconfig'
alias ed='v ~/.vim/custom-dictionary.utf-8.add'
alias ef='v ~/.config/fish/config.fish'
alias eg='v ~/.gitconfig'
alias ej='v ~/.jshintrc'
alias et='v ~/.tmux.conf'
alias ev='v ~/.vim/vimrc'
alias todo="v ~/todo.md"
alias etp="v ~/tp.md"

alias tm="tmux -u2"
alias tmkillall="tmux ls | grep : | cut -d. -f1 | awk '{print substr($1, 0, length($1)-1)}' | xargs -n1 tmux kill-session -t"
alias mux="tmuxinator"
alias cl="clear"

alias fa='fasd -a'        # any
alias fs='fasd -si'       # show / search / select
alias fd='fasd -d'        # directory
alias f='fasd -f'         # file
alias fsd='fasd -sid'     # interactive directory selection
alias fsf='fasd -sif'     # interactive file selection
alias j='fasd_cd -d'      # cd, same functionality as j in autojump
alias jj='fasd_cd -d -i'  # cd with interactive selection

alias d='docker'

# Reload the shell (i.e. invoke as a login shell)
alias reload="exec $SHELL -l"

# Lists the ten most used commands.
alias historystat="history 0 | awk '{print \$2}' | sort | uniq -c | sort -n -r | head"

alias py='python'
alias py2='python2'
alias py3='python3'
alias ipy='ipython'
alias mkv='mkvirtualenv'
alias workoff='deactivate'

alias ssh='TERM=xterm-256color ssh'
# }}}

# Files & Directories ------------------------------------------------------ {{{
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'
alias ......='cd ../../../../..'
alias -- -='cd -'

alias ln="${aliases[ln]:-ln} -v"  # verbose ln

alias l='ls -1'
alias ll="ls -l" # List all files in long format
alias la="ls -lA" # List all files in long format, including dot files
alias lsd='ls -l | grep "^d"' # List only directories
alias lt='ls -ltr'

alias md='mkdir -p'
alias rd='rmdir'
alias RM='rm -vrf'

# notify me before clobbering files
alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'
# }}}

# Git ---------------------------------------------------------------------- {{{
alias g='git'
alias gs='git status'
alias ga='git add'
alias gl='git log'
alias gt='git tag -n'                       # show tags with <n> lines of each tag message

alias gc='git commit'
alias gcm='git commit -m'
alias gca='git commit -am'

alias gco='git checkout'                    # checkout
alias gnb='git checkout -b'                 # create and switch to a new branch (mnemonic: "git new branch branchname...")

alias gd='git diff'                         # diff unstaged changes
alias gdt='git difftool'                    # Git difftool
alias gdc='git diff --cached'               # diff staged changes

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
alias gh='git help'

# Misc --------------------------------------------------------------------- {{{
alias server='python -m SimpleHTTPServer'

alias nvmupgrade='cd "$NVM_DIR" && git fetch origin && git checkout `git describe --abbrev=0 --tags` && cd -'

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
# }}}


# Load OS-specific aliases
case "$OSTYPE" in
  darwin*)
    load shell/lib/aliases.macos.sh ;;
  linux*)
    load shell/lib/aliases.linux.sh ;;
esac
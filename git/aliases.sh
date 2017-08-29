is_callable hub && alias git='hub'

alias eg='v ~/.gitconfig'

alias g='git'
alias gi='git init'
alias gs='git status'
alias ga='git add'
alias gl='git log --graph --pretty="format:%C(yellow)%h%Creset %C(red)%G?%Creset%C(green)%d%Creset %s %Cblue(%cr) %C(bold blue)<%aN>%Creset"'
alias gL='gl --stat'
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
# alias gpl='git pull --rebase --autostash'
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

# copy
alias gcb="git rev-parse --abbrev-ref HEAD | pbcopy | echo '=> Copied branch name'"
alias gch="git rev-parse --short HEAD | pbcopy | echo '=> Copied short commit hash'"

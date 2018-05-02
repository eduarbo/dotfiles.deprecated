# g = git status
# g ... = git $@
g() { [[ $# = 0 ]] && git status --short . || git $*; }
compdef g=hub

if _is_callable hub; then
  alias git='noglob hub'
else
  alias git='noglob git'
fi

alias gbr='git browse'
alias gi='git init'
alias gf='git fetch'
alias gs='git status'
alias gcl='git clone'
alias gsu='git submodule'
alias gcp='git cherry-pick -S'
alias gbo='git orphan'
alias gbl='git blame'
alias grp='git rev-parse'
alias gpl='git pull --rebase -S --autostash'
# alias gpl='git pull'

# stash
alias gss='git stash save'
alias gsa='git stash apply'
alias gsl='git stash list'
alias gsp='git stash pop'
alias gsd='git stash drop'

# reset
alias gr='git reset HEAD'
alias grs='git reset --soft HEAD^'
alias grh='git reset --hard'
alias gundo='git reset --hard HEAD~1'

# rebase
alias grb='git rebase -S --autostash -i'
alias grc='git rebase --continue'
alias grk='git rebase --skip'

# checkout
alias gco='git checkout'
alias gcoo='git checkout --'
alias gcob='git checkout -b'

# commit
alias gc='git commit -S'
alias gca='noglob git commit -S -am'
alias gcm='noglob git commit -S -m'
alias gcma='noglob git commit --amend -S -m'
alias gcf='noglob git commit -S --fixup'
alias gC='git commit'
alias gCm='git commit -m'
alias gCma='git commit --amend -m'
alias gCf='git commit --fixup'

# diff
alias gd='git diff'
alias gdc='git diff --cached'
alias gdt='git difftool'

# push
alias gp='git push'
alias gpb='git push origin'
alias gpt='git push --follow-tags'

# add
alias ga='git add'
alias gau='git add -u'
alias gap='git add --patch'

# branch
alias gb='git branch'
alias gbb='git branch -v'
alias gbd='git branch -D'

# log
alias gl='git log --graph --pretty="format:%C(yellow)%h%Creset %C(red)%G?%Creset%C(green)%d%Creset %s %Cblue(%cr) %C(bold blue)<%aN>%Creset"'
alias gll='git log --pretty="format:%C(yellow)%h%Creset %C(red)%G?%Creset%C(green)%d%Creset %s %Cblue(%cr) %C(bold blue)<%aN>%Creset"'
alias gL='gl --stat'

# tag
alias gt='git tag --sort=v:refname'
alias gtd='gt -d'
alias gta='gt -a'

# do not get VCS status (much faster)
alias k="k -Ah --no-vcs"

# checkout git branch (including remote branches), sorted by most recent commit,
# limit 30 last branches
fco() {
  local branches branch
  branches=$(git for-each-ref --count=30 --sort=-committerdate refs/heads/ --format="%(refname:short)") &&
  branch=$(echo "$branches" |
           fzf-tmux -d $(( 2 + $(wc -l <<< "$branches") )) +m) &&
  git checkout $(echo "$branch" | sed "s/.* //" | sed "s#remotes/[^/]*/##")
}

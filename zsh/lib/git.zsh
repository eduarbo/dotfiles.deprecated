#!/bin/zsh
# get the name of the branch we are on
function git_prompt_info() {
    # check if we're in a git repo
    command git rev-parse --is-inside-work-tree &>/dev/null || return

    echo "$GIT_PROMPT_PREFIX$vcs_info_msg_0_$GIT_PROMPT_SUFFIX"
}

# get the difference between the local and remote branches
function git_remote_status() {
    remote=${$(command git rev-parse --verify ${hook_com[branch]}@{upstream} --symbolic-full-name 2>/dev/null)/refs\/remotes\/}

    if [[ -n ${remote} ]] ; then
        ahead=$(command git rev-list ${hook_com[branch]}@{upstream}..HEAD 2>/dev/null | wc -l)
        behind=$(command git rev-list HEAD..${hook_com[branch]}@{upstream} 2>/dev/null | wc -l)

        if [ $ahead -eq 0 ] && [ $behind -gt 0 ]; then
            echo "$GIT_PROMPT_BEHIND_REMOTE"
        elif [ $ahead -gt 0 ] && [ $behind -eq 0 ]; then
            echo "$GIT_PROMPT_AHEAD_REMOTE"
        elif [ $ahead -gt 0 ] && [ $behind -gt 0 ]; then
            echo "$GIT_PROMPT_DIVERGED_REMOTE"
        fi
    fi
}


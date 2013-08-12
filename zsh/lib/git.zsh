# get the name of the branch we are on
function git_prompt_info() {
    ref=$(command git symbolic-ref HEAD 2> /dev/null) || \
    ref=$(command git rev-parse --short HEAD 2> /dev/null) || return

    echo "$GIT_PROMPT_PREFIX${ref#refs/heads/}$(parse_git_dirty)$GIT_PROMPT_SUFFIX"
}

# Checks if working tree is dirty
function parse_git_dirty() {
    local SUBMODULE_SYNTAX=''
    local GIT_STATUS=''
    local CLEAN_MESSAGE='nothing to commit (working directory clean)'

    if [[ "$(command git config --get oh-my-zsh.hide-status)" != "1" ]]; then
        if [[ $POST_1_7_2_GIT -gt 0 ]]; then
            SUBMODULE_SYNTAX="--ignore-submodules=dirty"
        fi

        if [[ "$DISABLE_UNTRACKED_FILES_DIRTY" == "true" ]]; then
            GIT_STATUS=$(command git status -s ${SUBMODULE_SYNTAX} -uno 2> /dev/null | tail -n1)
        else
            GIT_STATUS=$(command git status -s ${SUBMODULE_SYNTAX} 2> /dev/null | tail -n1)
        fi

        if [[ -n $GIT_STATUS ]]; then
            echo "$GIT_PROMPT_DIRTY"
        else
            echo "$GIT_PROMPT_CLEAN"
        fi
    else
        echo "$GIT_PROMPT_CLEAN"
    fi
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


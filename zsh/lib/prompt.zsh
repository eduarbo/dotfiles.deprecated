#!/bin/zsh
if [ $UID -eq 0 ]; then NCOLOR="red"; else NCOLOR="green"; fi
local return_code="%(?..%{$fg[red]%}%? ↵%{$reset_color%})"

PROMPT='%F{032}%~%f\
$(git_prompt_info) \
%F{214}%(!.#.%B%F{208}❱%f%F{214}❱%f%F{220}❱%f%b)%f '

PROMPT2='%F{214}❱%f '
RPS1='${return_code}'

RPROMPT='%F{208}※%f %F{246}%n at %B%m%b%f'

# GIT_PROMPT_PREFIX="%F{214} ⭠ %f%F{148}"
GIT_PROMPT_PREFIX="%F{214} on %f%F{148}"
GIT_PROMPT_CLEAN=""
GIT_PROMPT_DIRTY=""
GIT_PROMPT_SUFFIX="%f"
RUBY_PROMPT_PREFIX=""
RUBY_PROMPT_SUFFIX=""

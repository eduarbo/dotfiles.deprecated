if [ $UID -eq 0 ]; then NCOLOR="red"; else NCOLOR="green"; fi
local return_code="%(?..%{$fg[red]%}%? ↵%{$reset_color%})"

PROMPT='%F{208}※%f \
%F{032}%~%f\
$(git_prompt_info) \
%F{214}%(!.#.%B❱%b)%f '

PROMPT2='%F{214}❱%f '
RPS1='${return_code}'

RPROMPT='%F{240}%n@%m%f'

GIT_PROMPT_PREFIX="%F{106} ⭠ %f%F{154}"
GIT_PROMPT_CLEAN=""
GIT_PROMPT_DIRTY=""
GIT_PROMPT_SUFFIX="%f"
RUBY_PROMPT_PREFIX=""
RUBY_PROMPT_SUFFIX=""

prompt() {
  local BGBLACK="\[\033[40m\]"
  local BGRED="\[\033[41m\]"
  local BGGREEN="\[\033[42m\]"
  local BGYELLOW="\[\033[43m\]"
  local BGBLUE="\[\033[44m\]"
  local BGMAGENTA="\[\033[45m\]"
  local BGCYAN="\[\033[46m\]"
  local BGWHITE="\[\033[47m\]"
  local BGTRANS="\[\033[49m\]"

  local BLACK="\[\033[30m\]"
  local RED="\[\033[31m\]"
  local GREEN="\[\033[32m\]"
  local YELLOW="\[\033[33m\]"
  local BLUE="\[\033[34m\]"
  local MAGENTA="\[\033[35m\]"
  local CYAN="\[\033[36m\]"
  local WHITE="\[\033[37m\]"
  local TRANS="\[\033[39m\]"

  local BOLD="\[\033[1m\]"
  local RESET="\[\033[0m\]"

  export PS1="\n${BOLD}${GREEN}\w${YELLOW}\$(is_callable __git_ps1 && __git_ps1 ' on ${RESET}${YELLOW}%s') ${RESET}${YELLOW}❱${BOLD}${YELLOW}❱${GREEN}❱ ${RESET}"

  export PS2="${BOLD}${YELLOW}❯ ${RESET}"
}

prompt

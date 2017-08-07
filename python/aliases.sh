alias py='python'
alias py2='python2'
alias py3='python3'
alias ipy='ipython'

# virtualenv
alias mkv='mkvirtualenv'
alias workoff='deactivate'

alias server='python -m SimpleHTTPServer'

# Install or upgrade a global package
# Usage: gpip install –upgrade pip setuptools virtualenv
gpip(){
  PIP_REQUIRE_VIRTUALENV="" pip "$@"
}

gpip2(){
  PIP_REQUIRE_VIRTUALENV="" pip2 "$@"
}

gpip3(){
  PIP_REQUIRE_VIRTUALENV="" pip3 "$@"
}

if [[ $(_os) == macos ]]; then
    alias emacs='emacs -nw'
    alias e='./bin/emacs_macos'
else
    alias e='emacsclient -n'
fi

ediff() { e --eval "(ediff-files \"$1\" \"$2\")"; }

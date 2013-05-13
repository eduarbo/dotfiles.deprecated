# Useful functions {{{

function serve_this; python -m SimpleHTTPServer; end
function oldgcc; set -g CC /usr/bin/gcc-4.0 $argv; end
function tm; tmux -u2 $argv; end
function c; clear; end
function hl; less -R; end
function paththis; set PATH (pwd) $PATH $argv; end

function ef; vim ~/.config/fish/config.fish; end
function ev; vim ~/.vimrc; end
function ed; vim ~/.vim/custom-dictionary.utf-8.add; end
function eo; vim ~/Dropbox/Org; end
function ek; vim ~/lib/dotfiles/keymando/keymandorc.rb; end
function et; vim ~/.tmux.conf; end
function eg; vim ~/.gitconfig; end

function server
  set -l port "8000"

  # Open in the browser
  chromium-browser "http://localhost:$port/"

  python -m SimpleHTTPServer $port
end


function dev; cd ~/dev/switchfly; end

function fixopenwith
    /System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -kill -r -domain local -domain system -domain user
end

function ss; bcvi --wrap-ssh -- $argv; end
function bcvid; dtach -A /tmp/bcvi.socket bcvi --listener; end

function spotlight-off; sudo mdutil -a -i off ; and sudo mv /System/Library/CoreServices/Search.bundle/ /System/Library/CoreServices/SearchOff.bundle/ ; and killall SystemUIServer; end
function spotlight-on; sudo mdutil -a -i on ; and sudo mv /System/Library/CoreServices/SearchOff.bundle/ /System/Library/CoreServices/Search.bundle/ ; and killall SystemUIServer; end
function spotlight-wat; sudo fs_usage -w -f filesys mdworker | grep "open" ; end

function h; hg $argv; end

function g; git $argv; end
function gs; git status; end
function ga; git add $argv; end
function gc; git commit $argv; end
function gca; git commit -a; end
function gco; git checkout $argv; end
function gd; git diff $argv; end
function gl; git log; end
function gb; git branch $argv; end
function gm; git merge $argv; end
function gg; git grep --color -n $argv; end
function ggi; git grep -ni; end
function gsa; git submodule add $argv;  end
function gll; git log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit --date=relative; end
function stash; git stash; end
function pop; git stash apply; end

function Chrome; chromium-browser; end
function gv; gvim; end

function pbc; pbcopy; end
function pbp; pbpaste; end
function pbpb; pbp | pb; end

function collapse; sed -e 's/  */ /g'; end
function cuts; cut -d' ' $argv; end

function v; vim $argv; end
function V; vim . $argv; end

function o; nemo $argv; end
function oo; nemo .; end

function psg -d "Grep for a running process, returning its PID and full string"
    ps auxww | grep -i --color=always $argv | grep -v grep | collapse | cuts -f 2,11-
end

# }}}
# Bind Keys {{{

function fish_user_keybindings
    bind \cn accept-autosuggestion

    # Ignore iterm2 escape sequences.  Vim will handle them if needed.
    # bind \e\[I true
    # bind \e\[O true
    # ]]
end

# }}}
# Environment variables {{{

function prepend_to_path -d "Prepend the given dir to PATH if it exists and is not already in it"
    if test -d $argv[1]
        if not contains $argv[1] $PATH
            set -gx PATH "$argv[1]" $PATH
        end
    end
end
set -gx PATH "/usr/X11R6/bin"
prepend_to_path "/usr/texbin"
prepend_to_path "/sbin"
prepend_to_path "/usr/sbin"
prepend_to_path "/bin"
prepend_to_path "/usr/bin"
prepend_to_path "/usr/local/bin"
prepend_to_path "/usr/local/share/python"
prepend_to_path "/usr/local/sbin"
prepend_to_path "$HOME/bin"
prepend_to_path "/usr/local/share/python"
prepend_to_path "/usr/local/share/npm/bin"
prepend_to_path "/usr/share"

set BROWSER open

set -g -x fish_greeting ''
set -g -x EDITOR vim
set -g -x COMMAND_MODE unix2003
set -g -x RUBYOPT rubygems

set -g -x NODE_PATH "/usr/local/lib/node_modules"

# }}}
# Switchfly bash tools {{{
set -g -x TZ "UTC"
set -g -x HOME /home/eruiz
set -g -x JAVA_HOME=/usr/lib/jvm/jdk1.7.0
set -g -x JDK_HOME $JAVA_HOME
set -g -x ANT_HOME /usr/bin/ant
set -g -x MAVEN_HOME /usr/share/maven
set -g -x M2_HOME /usr/share/maven
set -g -x PLATFORM /usr/local/platform
set -g -x SWITCHFLY_HOME ~/dev/switchfly

prepend_to_path "$JAVA_HOME/bin"
prepend_to_path "$ANT_HOME/bin"
prepend_to_path "$MAVEN_HOME/bin"

###
# Ant & Maven options
set -g -x ANT_OPTS "-Xmx512m -Xms512m"
set -g -x MAVEN_OPTS "-Xmx2G -Xms2G -XX:PermSize=1G -XX:MaxPermSize=1G"
set -g -x JVM_ARGS "-Xms1024m -Xmx1024m -XX:MaxPermSize=512m"

# }}}
# Python variables {{{

set -g -x PIP_DOWNLOAD_CACHE "$HOME/.pip/cache"
set -g -x PYTHONSTARTUP "$HOME/.pythonrc.py"
set -g -x WORKON_HOME "$HOME/lib/virtualenvs"

# }}}
# Z {{{

. ~/src/z-fish/z.fish

function j; z $argv; end

# }}}
# Prompt {{{

set normal (set_color normal)
set magenta (set_color magenta)
set yellow (set_color yellow)
set green (set_color green)
set gray (set_color -o black)
set hg_promptstring "< on $magenta<branch>$normal>< at $yellow<tags|$normal, $yellow>$normal>$green<status|modified|unknown><update>$normal<
patches: <patches|join( â†’ )|pre_applied($yellow)|post_applied($normal)|pre_unapplied($gray)|post_unapplied($normal)>>" 2>/dev/null

function virtualenv_prompt
    if [ -n "$VIRTUAL_ENV" ]
        printf '(%s) ' (basename "$VIRTUAL_ENV")
    end
end

function hg_prompt
    hg prompt --angle-brackets $hg_promptstring 2>/dev/null
end

set fish_git_dirty_color FA0
function parse_git_dirty
         git diff --quiet HEAD ^&-
         if test $status = 1
            echo (set_color $fish_git_dirty_color)"✱"(set_color normal)
         end
end
function parse_git_branch
         # git branch outputs lines, the current branch is prefixed with a *
         set -l branch (git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/') 
         # set -l branch (git rev-parse --abbrev-ref HEAD)
         echo $branch (parse_git_dirty)
end

function git_prompt
         if test -z (git branch --quiet 2>| awk '/fatal:/ {print "no git"}')
          set_color CCC
          printf ' on '
          set_color -o 5AF
          # printf '⭠'
          printf '⎇ '
          printf (parse_git_branch)
          set_color normal
         end 
end

function prompt_pwd --description 'Print the current working directory, shortend to fit the prompt'
    echo $PWD | sed -e "s|^$HOME|~|"
end

function fish_prompt
    set last_status $status

    z --add "$PWD"

    echo

    # set_color blue
    # printf '%s' (whoami)
    # set_color normal
    # printf ' at '

    # set_color yellow
    # printf '%s' (hostname|cut -d . -f 1)
    # set_color normal
    # printf ' in '

    if test $last_status != 0
        set_color -o FA0
        printf '✘ %d ' $last_status
    end

    set_color 08D
    printf '%s' (prompt_pwd)

    git_prompt

    set_color -o F59
    printf ' ❱'
    set_color -o FE6
    printf '❱'
    set_color -o BE5
    printf '❱ '

    set_color normal
end

# }}}
# Directories {{{

function ..;    cd ..; end
function ...;   cd ../..; end
function ....;  cd ../../..; end
function .....; cd ../../../..; end

function md; mkdir -p $argv; end

function l1; tree --dirsfirst -ChFL 1 $argv; end
function l2; tree --dirsfirst -ChFL 2 $argv; end
function l3; tree --dirsfirst -ChFL 3 $argv; end
function l4; tree --dirsfirst -ChFL 4 $argv; end
function l5; tree --dirsfirst -ChFL 5 $argv; end
function l6; tree --dirsfirst -ChFL 6 $argv; end

function ll1; tree --dirsfirst -ChFupDaL 1 $argv; end
function ll2; tree --dirsfirst -ChFupDaL 2 $argv; end
function ll3; tree --dirsfirst -ChFupDaL 3 $argv; end
function ll4; tree --dirsfirst -ChFupDaL 4 $argv; end
function ll5; tree --dirsfirst -ChFupDaL 5 $argv; end
function ll6; tree --dirsfirst -ChFupDaL 6 $argv; end

function l;  l1 $argv; end
function ll; ll1 $argv; end

# }}}
# Misc {{{

# }}}
# Local Settings {{{

# set -g -x PGDATA "/home/eruiz/.pgdata"
set -g -x TERM xterm-256color

if test -s $HOME/.config/fish/local.fish
    . $HOME/.config/fish/local.fish
end

# }}}

true

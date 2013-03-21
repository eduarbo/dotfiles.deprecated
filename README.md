My vim & bash config files for Mac OSX (Eduardo Ruiz)
=====================================================

This is a collection of best of breed tools from across the web, from scouring other people's dotfiles repos, blogs, and projects.

Pre-requisites
--------------

You need to have installed:
* Git
* XCode Command Line Tools

Installation
------------
The installation step may overwrite existing dotfiles in your HOME and .vim
directories.

    git clone https://github.com/eduarbo/dotfiles.git ~/.dotfiles && cd ~/.dotfiles && source bin/dotfiles

The .gitconfig file is copied to the HOME directory so that any private git
configuration taking place is not accidentally committed. Everything else is
symlinked.

What else does it install?
--------------------------
**via Homebrew**

GNU core utilities
bash 4
git
ack
bash-completion
jpeg
libjpeg
macvim
node
optipng
tree
wget
gist
ctags
webkit2png
ffind
rename

**via npm**

bower
grunt
jshint
prettyjson
less
express
jade
wintersmith
jitsu
complete

Updating
--------

    dotfiles

Just run that command and it will update the repository, update vim plugins and
update packages.

Custom OS X defaults
--------------------
When setting up a new Mac, you may want to customise your OS X defaults after
installing the dotfiles. The script to do so can be found in bin/osxdefaults,
is added to the PATH, and can be executed by running:

    osxdefaults

Screenshots
-----------
![iTerm2 with thater itermcolor
theme](http://cloud.eduarbo.com/screenshots/Screen_Shot_Bash.png)

![My
Macvim](http://cloud.eduarbo.com/screenshots/Screen%20Shot%202012-10-28%20at%208.50.58%20PM.png)

Features
--------
* Ctrl + left/right arrows move forward/backward a word on bash
* Up/ down arrow partial search in history
* Added alias for Git, mvim and Directories
* Git repo information on Prompt
* Bash Completion
* Git Completion
* Nice Prompt

Credits
-------
Inspiration and code was taken from many sources, including:

* [@necolas](https://github.com/necolas) (Nicolas Gallagher) [https://github.com/necolas/dotfiles](https://github.com/necolas/dotfiles)
* [@mathiasbynens](https://github.com/mathiasbynens) (Mathias Bynens) [https://github.com/mathiasbynens/dotfiles](https://github.com/mathiasbynens/dotfiles)
* [@sjl](https://bitbucket.org/sjl) (Steve Losh) [https://bitbucket.org/sjl/dotfiles](https://bitbucket.org/sjl/dotfiles)

Also, I would like to thank [@Framallo](https://github.com/framallo) for transmit me the passion for Vim :)

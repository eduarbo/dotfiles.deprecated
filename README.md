My beatufil config files for vim, zsh, tmux and more in OSX
===========================================================

This is a collection of my configuration files under OSX.

Installation
------------
The installation step requires the XCode Command Line Tools and may overwrite
existing dotfiles in your HOME, .vim and .emacs.d directories.

To install only the essential brew packages (Latest versions of bash, zsh,
git, coreutils and rsync) simply run:

```bash
$ bash -c "$(curl -fsSL raw.github.com/eduarbo/dotfiles/master/bin/dotfiles)" -- --minimal
```

**Note:** Later you can install the full list of packages with the commands
`brewbootstrap` and `npmbootstrap`

However, if you have enough time to do a full installation (Install all Homebrew
and Node packages listed below) run the same command without the `--minimal`
parameter:

```bash
$ bash -c "$(curl -fsSL raw.github.com/eduarbo/dotfiles/master/bin/dotfiles)"
```

To install some common OSX Apps (listed below) in this step, just run this
command instead:

```bash
$ bash -c "$(curl -fsSL raw.github.com/eduarbo/dotfiles/master/bin/dotfiles)" -- --with-apps
```

Aliases
-------

There are nice aliases for git, ruby, shortcuts, files & directories and some
OS-specific aliases (Linux and OSX).


Vim Plugins (outdated)
----------------------

###Ag
Ag can be used as a replacement for ack but faster. This plugin will allow you
to run ag from vim, and shows the results in a split window.

`<leader>a` to run Ag

###Airline
Fast and lightweight alternative to powerline.

###Commentary
Comment or uncomment lines with `<leader>c`

###CtrlP
Full path fuzzy file, buffer, Most Recent Used (MRU), tag, ... finder with an
intuitive interface.

MAPPINGS:
  - `<leader>,` Open the CtrlP prompt

Once inside the prompt:
  - `<C-f>` Scroll to the 'next' search mode in the sequence.
  - `<C-b>` Scroll to the 'previous' search mode in the sequence.

###DelimitMate
Automatic closing of quotes, parenthesis, brackets, etc.

###Emmet
Editor plugin for high-speed HTML, XML, XSL (or any other structured code
format) coding and editing

MAPPINGS:
  - `<C-g>,` Expand abbreviation
  - `<C-g>n` Go to Next Edit Point in insert mode
  - `<C-g>N` Go to Previous Edit Point in insert mode
  - `<C-g>k` Remove tag
  - `<C-g>/` Toggle comment

###Endwise
Helps to end certain structures automatically. Useful for Lua, Ruby, sh,
VimScript, C and others.

###Fugitive
A Git wrapper so awesome, it should be illegal

MAPPINGS:
  - `<leader>gd` Git diff
  - `<leader>ggs` Git status
  - `<leader>ggw` Git write
  - `<leader>gga` Git add
  - `<leader>ggb` Git blame
  - `<leader>ggco` Git checkout
  - `<leader>ggci` Git commit
  - `<leader>ggm` Git move
  - `<leader>ggr` Git remove
  - `<leader>ggl` Git log

###Gundo
Graph your undo tree with `<F5>` so you can actually USE it.

###NERD Tree
Allows you to explore your filesystem and to open files and directories.

MAPPINGS:
  - `<leader>n` Toggle NERD Tree
  - `<leader>N` Find the current file in the tree

###Syntastic
Syntax checking on the fly.

###Tabular
Configurable, flexible, intuitive text aligning.

MAPPINGS:
  - `<leader>T,` Line up text at the commas
  - `<leader>Tr` Line up text at the rockets =>
  - `<leader>T=` Line up text at the equal sign
  - `<leader>T:` Line up text at the colons
  - `<leader>T/` Line up text at the slash

###Tagbar
Display tags of a file ordered by scope with `<F8>`

###UltiSnips
Snippet management for the Vim editor. Expand trigger with `<C-l>`


Notes
-----
Install Node via `npm` or `n`. It is less problematic than installed via
homebrew


Credits
-------
Inspiration and some code was taken from many sources, including:

* [@joedicastro](https://github.com/joedicastro/dotfiles)
* [@necolas](https://github.com/necolas/dotfiles)
* [@mathiasbynens](https://github.com/mathiasbynens/dotfiles)
* [@sjl](https://bitbucket.org/sjl/dotfiles)


TODO
----
* Make .gitconfig more general. Get rid of my harcoded email and name
* simplify bootstrap scripts in one file

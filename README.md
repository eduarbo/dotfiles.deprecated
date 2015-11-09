My beautiful config files for vim, zsh, tmux and more in OSX
============================================================

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

**Note:** Later you can install the full list of packages with command
`bootstrap`

However, if you have enough time to install all the packages (Install all
Homebrew, Node, Ruby and Python packages listed on `bootstrap`) run the same
command without any parameter:

```bash
$ bash -c "$(curl -fsSL raw.github.com/eduarbo/dotfiles/master/bin/dotfiles)"
```

To do a full installation with all the packages and some common OSX Apps in one
step, just run this command instead:

```bash
$ bash -c "$(curl -fsSL raw.github.com/eduarbo/dotfiles/master/bin/dotfiles)" -- --with-apps
```

Any user specific global git command should go into `~/.zshrc.local` instead of
`~/.gitconfig` to prevent them being commited to the repository. To setup your
email and name just copy the following lines into your `~/.zshrc.local`:

    export GIT_AUTHOR_NAME="Your name goes here"
    export GIT_AUTHOR_EMAIL=foo@example.org
    export GIT_COMMITTER_NAME="$GIT_AUTHOR_NAME"
    export GIT_COMMITTER_EMAIL="$GIT_AUTHOR_EMAIL"


Content
-------

- Zsh
- Vim
- Tmux
- Fonts
- Seil
- Karabiner
- Scripts
- linters config
- OS X defaults


ZSH
---

### Aliases

There are nice aliases for git, ruby, shortcuts, files & directories and some
OS-specific aliases (Linux and OSX).

### Dependencies

Homebrew
Node
Ruby
Python
Go

VIM
---

### Vim Dependencies

Vim needs to be compiled with lua, cscope and python2 support. If you already
run `dotfiles` or `bootstrap` script you can skip this section.

Otherwise install vim with this command:

    brew install macvim --with-cscope --with-lua --env-std --override-system-vim

And install the following dependencies:
- [FZF](https://github.com/junegunn/fzf): `brew install fzf`
- [Livedown](https://github.com/shime/livedown): `npm install -g livedown`
- [Ag (aka The Silver Searcher)](https://github.com/ggreer/the_silver_searcher):
    `brew install ag`


### Vim Plugins

- [**Bracketed Paste**](http://github.com/ConradIrwin/vim-bracketed-paste): Enables transparent pasting into vim. (i.e. no more :set paste!)
- [**CammelCaseMotion**](http://github.com/bkad/CamelCaseMotion): Provide CamelCase motion through words
- [**Commentary**](http://github.com/tpope/vim-commentary): Comment or uncomment lines with `<leader>c`
- [**CtrlP**](http://github.com/ctrlpvim/ctrlp.vim): Full path fuzzy file, buffer, Most Recent Used (MRU), tag, ... finder with an intuitive interface. You can take advantage of some code-searching tools like Ag or Pt that are faster than Ack and find
- [**CtrlSF**](http://github.com/dyng/ctrlsf.vim): An ack/ag powered code search and view tool, like ack.vim or :vimgrep but together with more context, and let you edit in-place with powerful edit mode
- [**DelimitMate**](http://github.com/Raimondi/delimitMate): Automatic closing of quotes, parentheses, brackets, etc
- [**Devicons**](http://github.com/ryanoasis/vim-devicons): Adds font icons (glyphs ★♨☢) to programming languages, libraries, and web developer filetypes for: NERDTree, powerline, vim-airline, ctrlp, unite, lightline.vim, vimfiler, and flagship
- [**Emmet**](http://github.com/mattn/emmet-vim): Editor plugin for high-speed HTML, XML, XSL (or any other structured code format) coding and editing
- [**Endwise**](http://github.com/tpope/vim-endwise): Helps to end certain structures automatically. Useful for Lua, Ruby, sh, VimScript, C and others
- [**Expand Region**](http://github.com/): Allows you to visually select increasingly larger regions of text using the same key combination
- [**FZF**](http://github.com/junegunn/fzf.vim): A command-line fuzzy finder written in Go. It is blazingly fast!
- [**Fugitive**](http://github.com/tpope/vim-fugitive): A Git wrapper so awesome, it should be illegal
- [**GitGutter**](http://github.com/airblade/vim-gitgutter): A Vim plugin which shows a git diff in the 'gutter' (sign column). It shows whether each line has been added, modified, and where lines have been removed. You can also stage and revert individual hunks
- [**Gruvbox**](http://github.com/morhetz/gruvbox): My favorite color scheme for Vim, heavily inspired by [badwolf](https://github.com/sjl/badwolf), [jellybeans](https://github.com/nanotech/jellybeans.vim) and [solarized](http://ethanschoonover.com/solarized)
- [**Gundo**](http://github.com/sjl/gundo.vim): Graph your undo tree with `<F5>` so you can actually USE it
- [**JsDoc**](http://github.com/heavenshell/vim-jsdoc): Generate JsDoc to your JavaScript code
- [**Airline**](https://github.com/bling/vim-airline): lean & mean status/tabline for vim that's light as air
- [**Linediff**](http://github.com/AndrewRadev/linediff.vim): The linediff plugin provides a simple command, `:Linediff`, which is used to diff two separate blocks of text
- [**ListToggle**](http://github.com/Valloric/ListToggle): Toggling the display of the quickfix list and the location-list
- [**Livedown**](http://github.com/shime/vim-livedown): Live Markdown previews for your favourite editor
- [**NERD Tree**](http://github.com/scrooloose/nerdtree): Allows you to explore your filesystem and to open files and directories
- [**Pad**](http://github.com/fmoralesc/vim-pad): A quick notetaking plugin for vim
- [**Repeat**](http://github.com/tpope/vim-repeat): Enable repeating supported plugin maps with "."
- [**Signature**](http://github.com/kshenoy/vim-signature): Place, toggle and display marks
- [**Snippets**](http://github.com/honza/vim-snippets): Snippets files for various programming languages
- [**Surround**](http://github.com/tpope/vim-surround): Mappings to easily delete, change and add such surroundings in pairs
- [**Syntastic**](http://github.com/scrooloose/syntastic): Syntax checking on the fly that runs files through external syntax checkers and displays any resulting errors to the user
- [**Tagbar**](http://github.com/majutsushi/tagbar): Display tags of a file ordered by scope with `<F8>`
- [**Tmux Navigator**](http://github.com/christoomey/vim-tmux-navigator): Seamless navigation between tmux panes and vim splits
- [**UltiSnips**](http://github.com/majutsushi/tagbar): Snippet management. Expand trigger with `<C-l>`
- [**Unimpaired**](http://github.com/tpope/vim-unimpaired): Pairs of handy bracket mappings 
- [**Vimproc**](http://github.com/Shougo/vimproc.vim): Asynchronous execution plugin for Vim
- [**Vimux**](http://github.com/benmills/vimux): Easily interact with tmux from vim
- [**YouCompleteMe**](http://github.com/Valloric/YouCompleteMe): Code-completion engine

Other plugins that provides a better syntax highlighting, indentation and
mappings for:
 [CSS3](http://github.com/hail2u/vim-css3-syntax),
 [CSS](http://github.com/JulesWang/css.vim),
 [Git](http://github.com/tpope/vim-git),
 [Go](http://github.com/fatih/vim-go),
 [Haml](http://github.com/tpope/vim-haml),
 [Handlebars/Mustache](http://github.com/mustache/vim-mustache-handlebars),
 [Html5](http://github.com/othree/html5.vim),
 [JS Beautify](http://github.com/maksimr/vim-jsbeautify),
 [JS Indent](http://github.com/jason0x43/vim-js-indent),
 [JS Libraries](http://github.com/othree/javascript-libraries-syntax.vim),
 [JSON](http://github.com/elzr/vim-json),
 [Javascript](http://github.com/othree/yajs.vim),
 [LESS](http://github.com/groenewege/vim-less),
 [Markdown](http://github.com/plasticboy/vim-markdown),
 [Rails](http://github.com/tpope/vim-rails),
 [Ruby](http://github.com/vim-ruby/vim-ruby),
 [SCSS](http://github.com/cakebaker/scss-syntax.vim),
 [Vim](http://github.com/tejr/vim-tmux)


### Credits
Inspiration and some code was taken from many sources, including:

* [Prezto](https://github.com/sorin-ionescu/prezto/)
* [@joedicastro](https://github.com/joedicastro/dotfiles)
* [@necolas](https://github.com/necolas/dotfiles)
* [@mathiasbynens](https://github.com/mathiasbynens/dotfiles)
* [@sjl](https://bitbucket.org/sjl/dotfiles)


### TODO

* Avoid reinstalling packages when bootstrap script is executed
* Setup Ruby properly with chruby in bootstrap
* Simplify installation script... maybe use stow?

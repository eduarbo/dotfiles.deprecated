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
run `install` or `brewbootstrap` script you can skip this section.

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
- [**Easy Align**](http://github.com/junegunn/vim-easy-align): A simple, easy-to-use alignment plugin
- [**Easy Motion**](http://github.com/Lokaltog/vim-easymotion): EasyMotion provides a much simpler way to use some motions in vim. It takes the <number> out of <number>w or <number>f{char} by highlighting all possible choices and allowing you to press one key to jump directly to the target
- [**Emmet**](http://github.com/mattn/emmet-vim): Editor plugin for high-speed HTML, XML, XSL (or any other structured code format) coding and editing
- [**Endwise**](http://github.com/tpope/vim-endwise): Helps to end certain structures automatically. Useful for Lua, Ruby, sh, VimScript, C and others
- [**Eunuch**](http://github.com/tpope/vim-eunuch): Vim sugar for the UNIX shell commands that need it the most
- [**Expand Region**](http://github.com/): Allows you to visually select increasingly larger regions of text using the same key combination
- [**FZF**](http://github.com/junegunn/fzf.vim): A command-line fuzzy finder written in Go. It is blazingly fast!
- [**Fugitive**](http://github.com/tpope/vim-fugitive): A Git wrapper so awesome, it should be illegal
- [**GitGutter**](http://github.com/airblade/vim-gitgutter): A Vim plugin which shows a git diff in the 'gutter' (sign column). It shows whether each line has been added, modified, and where lines have been removed. You can also stage and revert individual hunks
- [**Gruvbox**](http://github.com/morhetz/gruvbox): My favorite color scheme for Vim, heavily inspired by [badwolf](https://github.com/sjl/badwolf), [jellybeans](https://github.com/nanotech/jellybeans.vim) and [solarized](http://ethanschoonover.com/solarized)
- [**Gundo**](http://github.com/sjl/gundo.vim): Graph your undo tree with `<F5>` so you can actually USE it
- [**JsDoc**](http://github.com/heavenshell/vim-jsdoc): Generate JsDoc to your JavaScript code
- [**LightLine**](http://github.com/itchyny/lightline.vim): Fast and lightweight alternative to powerline with nice themes
- [**Linediff**](http://github.com/AndrewRadev/linediff.vim): The linediff plugin provides a simple command, `:Linediff`, which is used to diff two separate blocks of text
- [**ListToggle**](http://github.com/Valloric/ListToggle): Toggling the display of the quickfix list and the location-list
- [**Livedown**](http://github.com/shime/vim-livedown): Live Markdown previews for your favourite editor
- [**NERD Tree**](http://github.com/scrooloose/nerdtree): Allows you to explore your filesystem and to open files and directories
- [**Pad**](http://github.com/fmoralesc/vim-pad): A quick notetaking plugin for vim
- [**Rainbow Parentheses**](http://github.com/kien/rainbow_parentheses.vim): Better Rainbow Parentheses
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
 [C++](http://github.com/octol/vim-cpp-enhanced-highlight),
 [CSS3](http://github.com/hail2u/vim-css3-syntax),
 [CSS](http://github.com/JulesWang/css.vim),
 [Clojure](http://github.com/guns/vim-clojure-static),
 [Cucumber](http://github.com/tpope/vim-cucumber),
 [Docker](http://github.com/docker/docker/contrib/syntax/vim/),
 [Elixir](http://github.com/elixir-lang/vim-elixir),
 [Git](http://github.com/tpope/vim-git), [Go](http://github.com/fatih/vim-go),
 [Haml](http://github.com/tpope/vim-haml),
 [Handlebars/Mustache](http://github.com/mustache/vim-mustache-handlebars),
 [Haskell](http://github.com/neovimhaskell/haskell-vim),
 [Html5](http://github.com/othree/html5.vim),
 [JS Beautify](http://github.com/maksimr/vim-jsbeautify),
 [JS Indent](http://github.com/jason0x43/vim-js-indent),
 [JS Libraries](http://github.com/othree/javascript-libraries-syntax.vim),
 [JSON](http://github.com/elzr/vim-json),
 [Javascript](http://github.com/othree/yajs.vim),
 [LESS](http://github.com/groenewege/vim-less),
 [Markdown](http://github.com/plasticboy/vim-markdown),
 [Python](http://github.com/hdima/python-syntax),
 [Rails](http://github.com/tpope/vim-rails),
 [Rspec](http://github.com/skwp/vim-rspec),
 [Ruby](http://github.com/vim-ruby/vim-ruby),
 [SCSS](http://github.com/cakebaker/scss-syntax.vim),
 [TypeScript](http://github.com/leafgarland/typescript-vim),
 [Vim](http://github.com/tejr/vim-tmux)


### Credits
Inspiration and some code was taken from many sources, including:

* [Prezto](https://github.com/sorin-ionescu/prezto/)
* [@joedicastro](https://github.com/joedicastro/dotfiles)
* [@necolas](https://github.com/necolas/dotfiles)
* [@mathiasbynens](https://github.com/mathiasbynens/dotfiles)
* [@sjl](https://bitbucket.org/sjl/dotfiles)


### TODO

* Make .gitconfig more general. Get rid of my harcoded email and name
* simplify bootstrap scripts in one file

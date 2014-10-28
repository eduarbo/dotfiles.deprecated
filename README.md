My config files for Vim, bash, zsh and more in OSX
==================================================

This is a collection of my configuration files under OSX (I'm working to get the
installation script working in Linux)

Installation
------------
The installation step requires the XCode Command Line Tools and may overwrite
existing dotfiles in your HOME, .vim and .emacs.d directories.


To install with only the essential brew packages (Latest versions of bash, zsh,
git, coreutils and rsync. Later you can install the full list of packages with
the commands `brewbootstrap` and `npmbootstrap`) simply run:

```bash
$ bash -c "$(curl -fsSL raw.github.com/eduarbo/dotfiles/master/install)" -- --minimal
```

However, if you have enough time to do a full installation (Install all Homebrew
and Node packages listed below) run the same command without the `--minimal`
parameter:

```bash
$ bash -c "$(curl -fsSL raw.github.com/eduarbo/dotfiles/master/install)"
```

To install some common OSX Apps (listed below) in this step, just run this
command instead:

```bash
$ bash -c "$(curl -fsSL raw.github.com/eduarbo/dotfiles/master/install)" -- --with-apps
```

###Vim plugins

You will need to complie YCM.
If you want semantic support for C-family languages run:

```bash
cd ~/.vim/bundle/YouCompleteMe
./install.sh --clang-completer
```

However, *without* semantic support for C-family languages run:

```bash
cd ~/.vim/bundle/YouCompleteMe
./install.sh
```

###Aliases

There are nice aliases for git, ruby, shortcuts, files & directories and some
OS-specific aliases (Linux and OSX).

Credits
-------
Inspiration and some code was taken from many sources, including:

* [@joedicastro](https://github.com/joedicastro/dotfiles)
* [@necolas](https://github.com/necolas/dotfiles)
* [@mathiasbynens](https://github.com/mathiasbynens/dotfiles)
* [@sjl](https://bitbucket.org/sjl/dotfiles)

TODO
----
* Make .gitconfig more general. Get rid of my harcoded email and name, maybe?
* Make the installation script compatible with Linux

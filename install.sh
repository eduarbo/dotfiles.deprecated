#!/bin/bash
DOTFILES="$HOME/.dotfiles"

cd ${DOTFILES}
source ./lib/utils

update_submodules() {
  e_header "Updating Submodules..."
  git pull
  git submodule update --init
}

link() {
  # Force create/replace the symlink.
  ln -fs ${DOTFILES}/${1} ${HOME}/${2}
}

mirrorfiles() {
  # Copy `.gitconfig`.
  # Any global git commands in `~/.bash_profile.local` will be written to
  # `.gitconfig`. This prevents them being committed to the repository.
  rsync -avz --quiet ${DOTFILES}/git/gitconfig  ${HOME}/.gitconfig

  # Force remove the vim directory if it's already there.
  if [ -e "${HOME}/.vim" ] ; then
    mv "${HOME}/.vim" "${HOME}/.vim.old"
  fi

  # Force remove the vim directory if it's already there.
  if [ -e "${HOME}/.emacs.d" ] ; then
    mv "${HOME}/.emacs.d" "${HOME}/.emacs.d.old"
  fi

  mkdir -p ~/bin

  # Create the necessary symbolic links between the `.dotfiles` and `HOME`
  # directory. The `bash_profile` sources other files directly from the
  # `.dotfiles` repository.

  # link "bash/bashrc"        ".bashrc"
  # link "bash/bash_profile"  ".bash_profile"
  link "zsh/zshrc"                      ".zshrc"
  link "zsh/zshenv"                     ".zshenv"
  link "git/gitattributes"              ".gitattributes"
  link "git/gitignore"                  ".gitignore"
  link "vim"                            ".vim"
  link "emacs.d"                        ".emacs.d"
  link "vim/vimrc"                      ".vimrc"
  link "jshintrc"                       ".jshintrc"
  link "tmux.conf"                      ".tmux.conf"
  link "org.pqrs.KeyRemap4MacBook"      "Library/Preferences/org.pqrs.KeyRemap4MacBook"
  link "bin/*"                          "bin/"
  # link "fish/config.fish"   ".config/fish/config.fish"

  #Copy fonts to Fonts directory
  rsync -avz --quiet ${DOTFILES}/fonts/*  ${HOME}/Library/Fonts

  e_success "Dotfiles Installation complete!"
}

# Verify that the user wants to proceed before potentially overwriting files
echo
e_warning "Warning: This may overwrite your existing dotfiles."
read -p "Continue? (y/n) " -n 1
echo

if [[ $REPLY =~ ^[Yy]$ ]] ; then
  mirrorfiles

  echo "Time to change your default shell to zsh!"
  chsh -s `which zsh`
  /usr/bin/env zsh
  source ${HOME}/.zshrc
  update_submodules

  # Install Homebrew formulae 
  ./brewbootstrap
  # Install Node packages
  ./npmbootstrap
else
  echo "Aborting..."
  exit 1
fi

e_success "AWESOME! We have finished it. Enjoy! ಠ‿ಠ "

DOTFILES="$HOME/.dotfiles"

cd ${DOTFILES}
source ./lib/utils

if [ -d $DOTFILES ]
then
  echo "\033[0;33mYou already have dotfiles installed.\033[0m You'll need to remove $DOTFILES if you want to install"
  exit
fi

echo "\033[0;34mCloning dotfiles...\033[0m"
hash git >/dev/null && /usr/bin/env git clone https://github.com/eduarbo/dotfiles.git ~/.dotfiles || {
  echo "git not installed"
  exit
}

echo "\033[0;34mLooking for an existing zsh config...\033[0m"
if [ -f ~/.zshrc ] || [ -h ~/.zshrc ]; then
  echo "\033[0;33mFound ~/.zshrc.\033[0m \033[0;32mBacking up to ~/.zshrc.old\033[0m";
  mv ~/.zshrc ~/.zshrc.old;
  mv ~/.zshrc ~/.zshrc.old;
fi

if [ -f ~/.zshenv ] || [ -h ~/.zshenv ]; then
  echo "\033[0;33mFound ~/.zshenv.\033[0m \033[0;32mBacking up to ~/.zshenv.old\033[0m";
  mv ~/.zshenv ~/.zshenv.old;
  mv ~/.zshenv ~/.zshenv.old;
fi

echo "\033[0;34mCreating a symbolic links of ~/.zshrc and ~/.zshenv for you.\033[0m"
ln -s "$DOTFILES/zsh/zshrc" "$HOME/.zshrc"
ln -s "$DOTFILES/zsh/zshenv" "$HOME/.zshenv"

echo "\033[0;34mTime to change your default shell to zsh!\033[0m"
chsh -s `which zsh`
/usr/bin/env zsh
source ~/.zshrc

echo "\n\n \033[0;32mAWESOME! We have finished. Enjoy! ಠ‿ಠ\033[0m"

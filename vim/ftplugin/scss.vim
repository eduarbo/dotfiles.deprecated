" Only do this when not done yet for this buffer
if exists("b:did_ftplugin")
  finish
endif

runtime! ftplugin/css.vim ftplugin/css*.vim ftplugin/css/*.vim

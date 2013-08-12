# Smart URLs
autoload -Uz url-quote-magic
zle -N self-insert url-quote-magic

# stuff from default oh-my-zsh configuration
setopt auto_cd
setopt cdablevarS
setopt long_list_jobs
setopt multios
setopt prompt_subst
setopt promptpercent
setopt promptsubst
unsetopt menu_complete   # do not autoselect the first completion entry
unsetopt flowcontrol
setopt auto_menu         # show completion menu on succesive tab press
setopt complete_in_word
setopt always_to_end

zmodload -i zsh/complist

# remove HEAD^ escaping madness
unsetopt nomatch

autoload colors; colors;
export LSCOLORS="Gxfxcxdxbxegedabagacad"

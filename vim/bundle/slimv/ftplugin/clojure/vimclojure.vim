" This file is basically Vimclojure's ftplugin/clojure.vim, with a bunch of
" unnecessary stuff cut out.

" Only do this when not done yet for this buffer
if exists("b:did_ftplugin_vimclojure")
	finish
endif

let b:did_ftplugin_vimclojure = 1

let s:cpo_save = &cpo
set cpo&vim

let b:undo_ftplugin = "setlocal fo< com< cms< cpt< isk< def<"

setlocal iskeyword+=?,-,*,!,+,=,<,>,.,:

setlocal define=^\\s*(def\\(-\\|n\\|n-\\|macro\\|struct\\|multi\\)?

" Set 'formatoptions' to break comment lines but not other lines,
" and insert the comment leader when hitting <CR> or using "o".
setlocal formatoptions-=t formatoptions+=croql
setlocal commentstring=;%s

" Set 'comments' to format dashed lists in comments.
setlocal comments=sO:;\ -,mO:;\ \ ,n:;

" When the matchit plugin is loaded, this makes the % command skip parens and
" braces in comments.
let b:match_words = &matchpairs
let b:match_skip = 's:comment\|string\|character'

let &cpo = s:cpo_save

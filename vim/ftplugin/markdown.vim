normal! zR
setlocal foldlevel=1
setlocal nofoldenable    " disable folding"

" Use <localleader>1/2/3 to add headings.
nnoremap <buffer> <localleader>1 yypVr=:redraw<cr>
nnoremap <buffer> <localleader>2 yypVr-:redraw<cr>
nnoremap <buffer> <localleader>3 mzI###<space>`zllll <ESC>

" TODO: this should be applied to all html* FileTypes
setlocal foldlevel=99
setlocal foldmethod=manual
setlocal shiftwidth=4
setlocal softtabstop=4
setlocal tabstop=4

" Use <localleader>f to fold the current tag.
nnoremap <buffer> <localleader>f Vatzf

" Indent tag
nnoremap <buffer> <localleader>- Vat=

" handlebars tags
inoremap <buffer> <c-b> {{<space><space>}}<left><left><left>

" HTML tag closing
inoremap <buffer> <C-_> <space><bs><esc>:call InsertCloseTag()<cr>a

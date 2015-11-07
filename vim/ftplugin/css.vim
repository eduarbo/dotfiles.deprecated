" TODO: applicable to *.css, *.scss and *.less FileTypes
setlocal foldmethod=marker
setlocal foldmarker={,}
setlocal iskeyword+=-

" Make {<cr> insert a pair of brackets in such a way that the cursor is correctly
" positioned inside of them AND the following code doesn't get unfolded.
inoremap <buffer> {<cr> {}<left><cr><space><space>.<cr><esc>kA<bs>

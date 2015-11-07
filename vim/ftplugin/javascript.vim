setlocal foldmethod=marker
setlocal foldmarker={,}
setlocal foldtext=getline(v:foldstart)
setlocal shiftwidth=4 softtabstop=4 tabstop=4
setlocal foldlevel=99
call MakeSpacelessBufferIabbrev('clog', 'console.log();<left><left>')

" Make {<cr> insert a pair of brackets in such a way that the cursor is correctly
" positioned inside of them AND the following code doesn't get unfolded.
inoremap <buffer> {<cr> {}<left><cr><space><space><space><space>.<cr><esc>kA<bs>
nnoremap <buffer> <localleader>b :call <SID>toggleStatement("debugger;  // XXX BREAKPOINT")<CR>

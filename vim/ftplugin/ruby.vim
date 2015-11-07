setlocal foldmethod=syntax
nnoremap <buffer> <localleader>b :call <SID>toggleStatement("require 'pry'; binding.pry  # XXX BREAKPOINT")<CR>

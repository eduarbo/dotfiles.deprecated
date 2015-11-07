let b:dispatch = 'mvn -B package install'
compiler maven

setlocal foldmethod=marker
setlocal foldmarker={,}

nnoremap <buffer> <localleader>S :SortScalaImports<cr>
nnoremap <buffer> M :call scaladoc#Search(expand("<cword>"))<cr>
vnoremap <buffer> M "ry:call scaladoc#Search(@r)<cr>
nmap <buffer> <localleader>( ysiwbi
nmap <buffer> <localleader>[ ysiwri

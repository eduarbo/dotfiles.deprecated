setlocal foldmethod=marker
setlocal foldmarker={,}
setlocal makeprg=javac\ %
setlocal errorformat=%A%f:%l:\ %m,%-Z%p^,%-C%.%#

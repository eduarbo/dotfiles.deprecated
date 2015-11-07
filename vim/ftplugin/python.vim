setlocal define=^\s*\\(def\\\\|class\\)
setlocal shiftwidth=4 softtabstop=4 tabstop=4

" Jesus, Python.  Five characters of punctuation for a damn string?
inoremap <buffer> <c-g> _(u'')<left><left>
inoremap <buffer> <c-b> """"""<left><left><left>

" Jesus tapdancing Christ, built-in Python syntax, you couldn't let me
" override this in a normal way, could you?
if exists("python_space_error_highlight")
  unlet python_space_error_highlight
endif

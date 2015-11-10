function! fn#ToggleStatement(statement)           " {{{
  " Adds desired text if current line is different than text, otherwise it
  " removes the line. Useful to add breakpoints
  let lineNumber = line('.')
  let line = getline(lineNumber)
  if strridx(line, a:statement) != -1
    normal dd
  else
    let plnum = prevnonblank(lineNumber)
    call append(line('.')-1, repeat(' ', indent(plnum)).a:statement)
    normal k
  endif
endfunction "}}}

function! fn#CycleCommand(cmd, onReach)           " {{{
  " Cycle through commands to call `a:onReach` when error 'E553 No more items'
  " is given by command `a:cmd`. Useful for some QuickFix and Location-list
  " commands to jump to the top when last error in QuickFix is reached and
  " viceversa
  " Eg:
  "   nnoremap ]q <Plug>QuickFixNext :call <SID>CycleCommand('cnext', 'cfirst')<CR>
  try
    exec a:cmd
  catch /^Vim\%((\a\+)\)\=:E553/
    exec a:onReach
  endtry
endfunction "}}}

function! fn#ZoomToggle()                         " {{{
  " Maximize current window or restore previous size of all windows
  if exists('t:zoomed') && t:zoomed
    execute t:zoom_winrestcmd
    let t:zoomed = 0
  else
    let t:zoom_winrestcmd = winrestcmd()
    resize
    vertical resize
    let t:zoomed = 1
  endif
endfunction "}}}

function! fn#GoogleIt(query)                      " {{{
  " Search `a:query` into Google.com
  silent! exec 'silent! !open "https://www.google.com/search?q=' . a:query . '"'
endfunction "}}}

function! fn#SynStack()                           " {{{
  " Show syntax highlighting groups for word under cursor
  if !exists("*synstack")
    return
  endif
  echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunc "}}}

function! fn#GenerateUnicodeTable(first, last)    " {{{
  " Generates a table of Unicode characters from `a:first` to `a:last` and
  " prints it in a new vertical buffer
  " Eg:
  "   :call GenerateUnicodeTable(0xf000, 0xffff)
  vnew
  vertical resize 58
  let i = a:first
  while i <= a:last
    if (i%256 == 0)
      $put ='----------------------------------------------------'
      $put ='     0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F '
      $put ='----------------------------------------------------'
    endif
    let c = printf('%04X ', i)
    for j in range(16)
      let c = c . nr2char(i) . ' '
      let i += 1
    endfor
    $put =c
  endwhile
endfunction "}}}

function! fn#InTmuxSession()                      " {{{
  " Check if Vim was loaded in Tmux
  return !has('gui_running') && $TMUX != ''
endfunction "}}}

function! fn#MyFoldText()                         " {{{
  let line = getline(v:foldstart)

  let nucolwidth = &fdc + &number * &numberwidth
  let windowwidth = winwidth(0) - nucolwidth - 5
  let foldedlinecount = v:foldend - v:foldstart

  " expand tabs into spaces
  let onetab = strpart('          ', 0, &tabstop)
  let line = substitute(line, '\t', onetab, 'g')

  let maxlen = windowwidth - len(foldedlinecount) - 9
  let line = strpart(line, 0, maxlen)
  let fillcharcount = maxlen - len(line)
  return line . " \uf470 " . repeat(" ",fillcharcount) . ' ' . foldedlinecount . ' lines'
endfunction " }}}

function! fn#HL(group, fg, ...) "{{{
  " Arguments: group, guifg, guibg, gui
  " ----------------------------------------------
  " Function extracted from gruvbox's color scheme
  " Source: https://github.com/morhetz/gruvbox

  " foreground
  let fg = a:fg
  let none = ['NONE', 'NONE']

  " background
  if a:0 >= 1
    let bg = a:1
  else
    let bg = none
  endif

  " emphasis
  if a:0 >= 2 && strlen(a:2)
    let emstr = a:2
  else
    let emstr = 'NONE,'
  endif

  let histring = [ 'hi', a:group,
        \ 'guifg=' . fg[0], 'ctermfg=' . fg[1],
        \ 'guibg=' . bg[0], 'ctermbg=' . bg[1],
        \ 'gui=' . emstr[:-2], 'cterm=' . emstr[:-2]
        \ ]

  execute join(histring, ' ')
endfunction "}}}

function! fn#ToggleStatement(statement)           " {{{
  " Description: Adds desired text if current line is different than text,
  " otherwise it removes the line. Useful to add breakpoints

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
  " Description: Cycle through commands to call `a:onReach` when error 'E553 No
  " more items' is given by command `a:cmd`. Useful for some QuickFix and
  " Location-list commands to jump to the top when last error in QuickFix is
  " reached and viceversa
  " Eg:
  "   nnoremap ]q <Plug>QuickFixNext :call <SID>CycleCommand('cnext', 'cfirst')<CR>

  try
    exec a:cmd
  catch /^Vim\%((\a\+)\)\=:E553/
    exec a:onReach
  endtry
endfunction "}}}

function! fn#ZoomToggle()                         " {{{
  " Description: Maximize current window or restore previous size of all
  " windows.
  " Source: Taken from BenC's answer at on Stack Overflow at
  " <https://stackoverflow.com/a/26551079/457812>

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
  " Description: Search `a:query` into Google.com

  silent! exec 'silent! !open "https://www.google.com/search?q=' . a:query . '"'
endfunction "}}}

function! fn#SynStack()                           " {{{
  " Description: Show syntax highlighting groups for word under cursor
  " Source: Taken from someone's dotfiles... Don't remmeber who :(

  if !exists("*synstack")
    return
  endif
  echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunc "}}}

function! fn#GenerateUnicodeTable(first, last)    " {{{
  " Description: Generates a table of Unicode characters from `a:first` to
  " `a:last` and prints it in a new vertical buffer
  " Eg:
  "   :call fn#GenerateUnicodeTable(0xf000, 0xffff)

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
  " Description: Check if Vim was loaded in Tmux

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
  return line . " •••" . repeat(" ",fillcharcount) . ' ' . foldedlinecount . ' lines'

endfunction " }}}

function! fn#HL(group, fg, ...)                   " {{{
  " Description: Simple way to set highlighting for cterm and gui terminals
  " Arguments: group, guifg, guibg, gui, guisp
  " Eg:
  "   call fn#HL('TabLineSel', ['#fabd2f', 214], ['#1d2021', 234], 'bold,')
  "
  " Source: Taken from gruvbox's color scheme
  " <https://github.com/morhetz/gruvbox>

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

  " special fallback
  if a:0 >= 3
    let fg = a:3
  endif

  let histring = [ 'hi', a:group,
        \ 'guifg=' . fg[0], 'ctermfg=' . fg[1],
        \ 'guibg=' . bg[0], 'ctermbg=' . bg[1],
        \ 'gui=' . emstr[:-2], 'cterm=' . emstr[:-2]
        \ ]

  " special
  if a:0 >= 3
    call add(histring, 'guisp=' . a:3[0])
  endif

  execute join(histring, ' ')
endfunction "}}}

function! fn#ToggleProfile()                      " {{{
  " Description: I'm tired of profiling manually everytime. This function allows
  " me to start, pause and resume profiling so I can figure out what is slowing
  " Vim down easily.

  if !exists('s:is_profiling')
    let s:is_profiling = 1
    echo 'Start profiling. At this point do slow actions'
    profile start profile.log
    profile func *
    profile file *
  elseif s:is_profiling
    profile pause
    let s:is_profiling = 0
    echo 'pausing profiling...'
  else
    let s:is_profiling = 1
    echo 'resuming profiling...'
    profile continue
  endif
endfunction "}}}

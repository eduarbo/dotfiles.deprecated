scriptencoding utf-8
function! fn#CycleCommand(cmd, onReach) abort                             " {{{
  " Description: Cycle through commands to call `a:onReach` when error 'E553 No
  " more items' is given by command `a:cmd`. Useful for some QuickFix and
  " Location-list commands to jump to the top when last error in QuickFix is
  " reached and viceversa
  " Eg:
  "   nnoremap ]q <Plug>QuickFixNext :call <SID>CycleCommand('cnext', 'cfirst')<CR>

  try
    try
      exec a:cmd
    catch /^Vim\%((\a\+)\)\=:E553/
      exec a:onReach
    catch /^Vim\%((\a\+)\)\=:E776/
    endtry
  catch /^Vim\%((\a\+)\)\=:E42/
  endtry
endfunction "}}}

function! fn#OpenInSplitIfBufferDirty(file, ...) abort "{{{
  " Description: Open file in Split if Buffer not empty
  " Inspired By:
  " http://stackoverflow.com/questions/11060984/vim-split-if-the-buffer-not-empty

  " TODO: Do not open a new split if buffer exists, focus it instead
  if line('$') ==# 1 && getline(1) ==# ''
    exec 'e' a:file
  else
    if a:0
      exec 'vsplit' a:file
    else
      exec 'split' a:file
    endif
  endif
endfunction "}}}

function! fn#ZoomToggle() abort                                           " {{{
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

function! fn#GoogleIt(query) abort                                        " {{{
  " Description: Search `a:query` into Google.com

  silent! exec 'silent! !open "https://www.google.com/search?q=' . a:query . '"'
endfunction "}}}

function! fn#SynStack() abort                                             " {{{
  " Description: Show syntax highlighting groups for word under cursor
  " Source: Taken from someone's dotfiles... Don't remmeber who :(

  if !exists('*synstack')
    return
  endif
  echo map(synstack(line('.'), col('.')), "synIDattr(v:val, 'name')")
endfunction "}}}

function! fn#GenerateUnicodeTable(first, last) abort                      " {{{
  " Description: Generates a table of Unicode characters from `a:first` to
  " `a:last` and prints it in a new vertical buffer
  " Eg:
  "   :call fn#GenerateUnicodeTable(0xf000, 0xffff)

  vnew
  vertical resize 58
  let l:i = a:first
  while l:i <= a:last
    if (l:i%256 == 0)
      $put ='----------------------------------------------------'
      $put ='     0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F '
      $put ='----------------------------------------------------'
    endif
    let l:c = printf('%04X ', l:i)
    for l:j in range(16)
      let l:c = l:c . nr2char(l:i) . ' '
      let l:i += 1
    endfor
    $put = l:c
  endwhile
endfunction "}}}

function! fn#InTmuxSession() abort                                        " {{{
  " Description: Check if Vim was loaded in Tmux

  return !has('gui_running') && $TMUX !=# ''
endfunction "}}}

function! fn#BetterFoldText() abort                                       " {{{
  " Taken from: https://coderwall.com/p/usd_cw/a-pretty-vim-foldtext-function
  let l:lpadding = &foldcolumn
  redir => l:signs
  execute 'silent sign place buffer='.bufnr('%')
  redir End
  let l:lpadding += l:signs =~# 'id=' ? 2 : 0

  if exists('+relativenumber')
    if (&number)
      let l:lpadding += max([&numberwidth, strlen(line('$'))]) + 1
    elseif (&relativenumber)
      let l:lpadding += max([&numberwidth, strlen(v:foldstart - line('w0')), strlen(line('w$') - v:foldstart), strlen(v:foldstart)]) + 1
    endif
  else
    if (&number)
      let l:lpadding += max([&numberwidth, strlen(line('$'))]) + 1
    endif
  endif

  " expand tabs
  let l:start = substitute(getline(v:foldstart), '\t', repeat(' ', &tabstop), 'g')
  let l:end = substitute(substitute(getline(v:foldend), '\t', repeat(' ', &tabstop), 'g'), '^\s*', '', 'g')

  let l:info = ' (' . (v:foldend - v:foldstart) . ')'
  let l:infolen = strlen(substitute(l:info, '.', 'x', 'g'))
  let l:width = winwidth(0) - l:lpadding - l:infolen

  let l:separator = '  '
  let l:separatorlen = strlen(substitute(l:separator, '.', 'x', 'g'))
  let l:start = strpart(l:start , 0, l:width - strlen(substitute(l:end, '.', 'x', 'g')) - l:separatorlen)
  let l:text = l:start . l:separator . l:end

  return l:text . repeat(' ', l:width - strlen(substitute(l:text, '.', 'x', 'g'))) . l:info
endfunction "}}}

function! fn#HL(group, fg, ...) abort                                     " {{{
  " Description: Simple way to set highlighting for cterm and gui terminals
  " Arguments: group, guifg, guibg, gui, guisp
  " Eg:
  "   call fn#HL('TabLineSel', ['#fabd2f', 214], ['#1d2021', 234], 'bold,')
  "
  " Source: Taken from gruvbox's color scheme
  " <https://github.com/morhetz/gruvbox>

  " foreground
  let l:fg = a:fg
  let l:none = ['NONE', 'NONE']

  " background
  if a:0 >= 1
    let l:bg = a:1
  else
    let l:bg = l:none
  endif

  " emphasis
  if a:0 >= 2 && strlen(a:2)
    let l:emstr = a:2
  else
    let l:emstr = 'NONE,'
  endif

  " special fallback
  if a:0 >= 3
    let l:fg = a:3
  endif

  let l:histring = [ 'hi', a:group,
        \ 'guifg=' . l:fg[0], 'ctermfg=' . l:fg[1],
        \ 'guibg=' . l:bg[0], 'ctermbg=' . l:bg[1],
        \ 'gui=' . l:emstr[:-2], 'cterm=' . l:emstr[:-2]
        \ ]

  " special
  if a:0 >= 3
    call add(l:histring, 'guisp=' . a:3[0])
  endif

  execute join(l:histring, ' ')
endfunction "}}}

function! fn#ToggleProfile() abort                                        " {{{
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

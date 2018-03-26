" :set encoding should be described before :scriptencoding, otherwise Vim can
" not recognize the character code of your vimrc
set encoding=utf-8
scriptencoding utf-8

set nocompatible

" Plugins {{{
" Automatic installation
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
        \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')

Plug 'christoomey/vim-tmux-navigator'
Plug 'w0ng/vim-hybrid'

call plug#end() " Automatically executes filetype plugin indent on and syntax enable

" Preferences {{{
set laststatus=2             " Show statusbar
set nolist                   " Don't show whitespace chars (indentLine does it nicer)
set nospell                  " No spell check, please
set number                   " Line numbers
set visualbell               " No sounds!
set showmatch                " Show matching delimiters
set browsedir=buffer         " Sets File>Open to start in current file's path
set showmode                 " Show mode in cmdline
set autoread                 " Auto-update a file that's been edited externally
set backspace=indent,eol,start
set mouse=a
set lazyredraw               " Don't update screen while running macros
set hidden                   " Hide abandoned buffers
set nostartofline
set scrolloff=5
set shortmess+=filmnrxoOtTs

set ruler                " Show line/col no in statusline
set showcmd              " Show command issued

" Time out on key codes but not mappings.
" Basically this makes terminal Vim work sanely.
set notimeout
set ttimeout
set ttimeoutlen=0

set textwidth=88
set fillchars=vert:¦

set tags=./.tags;/,~/.tags,~/tags

set completeopt=longest,menu,preview
set wildmenu                    " Show menu instead of auto-complete
set wildmode=list:longest,full  " command <Tab> completion: list
" matches -> longest common -> then
" all.
set wildignore+=*.cache,cache/**,*~,*.swp,*.log,.sass-cache
set wildignore+=*.class,*.o,*.pyc,*.obj,*DS_Store*

" Shell {{{
" 256bit terminals
set t_Co=256
if (has('termguicolors'))
  " True colors
  set termguicolors
endif
set title
" }}}
" Search {{{
set incsearch            " find as you type
set hlsearch             " Highlight search terms
set ignorecase           " case insensitive search
set smartcase            " case sensitive when uc present
set gdefault             " global flag for substitute by default
" }}}
" Formatting {{{
set autoindent
set shiftround
set expandtab
set shiftwidth=4
set tabstop=4
set softtabstop=4
" Wrapping
set nowrap
" Backspace and cursor keys to wrap
set whichwrap=b,s,h,l,<,>,[,]
set backspace=indent,eol,start      " normal backspace behavior
" see :h fo-table
set formatoptions=qrn1lr
" }}}
" Navigation {{{
" netrw
let g:netrw_altv = 1            " open files on right
let g:netrw_banner = 0          " do not display info on the top of window
let g:netrw_browse_split = 0    " use the previous window to open file
" let g:netrw_fastbrowse = 2      " only obtains directory listings when the directory hasn't been seen before
let g:netrw_keepdir = 0
let g:netrw_liststyle = 3       " tree style
let g:netrw_preview = 1         " open previews vertically
let g:netrw_retmap = 1          " click once to select and open a file, double-click to return
let g:netrw_silent = 1
let g:netrw_sort_options = 'i'  " Ignore case when sortig
let g:netrw_sort_sequence = '[\/]$,*' " sort is affecting only: directories on the top, files below
let g:netrw_special_syntax= 1   " get additional highlighting of *.bak, *.gz, *.zip, *.bz2, etc groups
let g:netrw_winsize = 25        " absolute width of netrw window

noremap <leader>tn :Lexplore<cr>
" }}}
" Keymaps {{{
let mapleader = ","
noremap ; :
noremap : ;

nnoremap <leader><tab> <C-^>

nnoremap <tab> %
silent! unmap [%
silent! unmap ]%

vmap Q <Plug>FormatSelection
nmap Q <Plug>FormatLine

" Emacs bindings in command line mode
cnoremap <c-a> <home>
cnoremap <c-e> <end>

" Made D behave as C
nnoremap D d$

" Make the Y behavior similar to D & C
nnoremap Y y$

" Copy/Paste to and from Desktop Environment
" vnoremap <leader>y "*y
" vnoremap <leader>p "*p
" vnoremap <leader>Y "+y
" vnoremap <leader>P "+p
vmap <Leader>y "*y
vmap <Leader>d "*d
nmap <Leader>p "*p
nmap <Leader>P "*P
vmap <Leader>p "*p
vmap <Leader>P "*P

" Use sane regexes.
nnoremap / /\v
vnoremap / /\v

" Directional Keys
noremap j gj
noremap k gk
noremap gj j
noremap gk k

" Splits
nnoremap <leader>wv <C-w>v
nnoremap <leader>ws <C-w>s

" Keep search matches in the middle of the window.
nnoremap n nzzzv
nnoremap N Nzzzv
noremap <silent> <leader><space> :noh<cr>:call clearmatches()<cr>

" Don't move on *
nnoremap * *<c-o>

" Select (charwise) the contents of the current line, excluding indentation.
nnoremap vv ^vg_

" Keep the cursor in place while joining lines
nnoremap J mzJ`z

"identation
vnoremap < <gv
vnoremap > >gv

" Panic Button
nnoremap <leader>` mzggg?G`z
" }}}
" }}}

set background=dark
colorscheme hybrid

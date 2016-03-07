" .vimrc
" Author: Eduardo Ruiz <eduarbo@gmail.com>
" Source: https://github.com/eduarbo/dotfiles/tree/master/vim

" Preamble                                                                  {{{1
filetype off

let mapleader = " "
let maplocalleader = ","

let s:ag = executable('ag')
" }}}

" Plugins                                                                   {{{1
" Automatic installation
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
        \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')

" Dependencies                                                              {{{2
Plug 'xolox/vim-misc', {'for': ['python', 'c', 'cpp', 'javascript']}
" }}}

" File Finders                                                              {{{2
let ctrlP_opts = {'on': ['CtrlPTag', 'CtrlPBuffer', 'CtrlPMRUFiles', 'CtrlP']}
Plug 'ctrlpvim/ctrlp.vim', ctrlP_opts                                     " {{{3
let g:ctrlp_dont_split = 'NERD_tree_2'
let g:ctrlp_jump_to_buffer = 0
let g:ctrlp_working_path_mode = 0
let g:ctrlp_match_window_reversed = 1
let g:ctrlp_split_window = 0
let g:ctrlp_max_height = 20
let g:ctrlp_extensions = ['tag']
let g:ctrlp_map = ''
let g:ctrlp_custom_ignore = 'static'

let g:ctrlp_prompt_mappings = {
      \ 'PrtSelectMove("j")':   ['<c-j>', '<down>', '<s-tab>'],
      \ 'PrtSelectMove("k")':   ['<c-k>', '<up>', '<tab>'],
      \ 'PrtHistory(-1)':       ['<c-n>'],
      \ 'PrtHistory(1)':        ['<c-p>'],
      \ 'ToggleFocus()':        ['<c-space>'],
      \ }

let ctrlp_filter_greps = "".
      \ "egrep -iv '\\.(" .
      \ "jar|class|swp|swo|log|so|o|pyc|jpe?g|png|gif|mo|po" .
      \ ")$' | " .
      \ "egrep -v '^(\\./)?(" .
      \ ".git/|.hg/|.svn/" .
      \ ")'"

let my_ctrlp_user_command = "" .
      \ "find %s '(' -type f -or -type l ')' -maxdepth 15 -not -path '*/\\.*/*' | " .
      \ ctrlp_filter_greps

let my_ctrlp_hg_command = "hg --cwd %s locate -I ."

let my_ctrlp_git_command = "" .
      \ "cd %s && git ls-files --exclude-standard -co | " .
      \ ctrlp_filter_greps

let my_ctrlp_ag_command = 'ag %s -l --nocolor -g "" | ' . ctrlp_filter_greps

" Use The Silver Searcher https://github.com/ggreer/the_silver_searcher
if s:ag
  " Use Ag over Grep
  set grepprg="ag --nogroup --nocolor"

  " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
  let my_ctrlp_user_command = my_ctrlp_ag_command

  " ag is fast enough that CtrlP doesn't need to cache
  let g:ctrlp_use_caching = 0
endif

" Multiple VCS's:
let g:ctrlp_user_command = {
      \ 'types': {
      \ 1: ['.git', my_ctrlp_git_command],
      \ 2: ['.hg', my_ctrlp_hg_command],
      \ },
      \ 'fallback': my_ctrlp_user_command
      \ }

nnoremap <leader>, :CtrlP<cr>
nnoremap <leader>. :CtrlPTag<cr>
nnoremap <leader>b :CtrlPBuffer<cr>
nnoremap <leader>m :CtrlPMRUFiles<cr>
" }}}

let nerdTree_opts = {'on': ['NERDTreeToggle', 'NERDTreeFind']}
Plug 'scrooloose/nerdtree', nerdTree_opts                                 " {{{3
" Open the project tree and expose current file in the nerdtree with ,N
nnoremap <Leader>N :NERDTreeFind<CR>
noremap  <leader>n :NERDTreeToggle<cr>

let NERDTreeStatusline="%{getcwd()}"
let NERDTreeHighlightCursorline = 1
let NERDTreeIgnore = ['\~$', '.*\.pyc$']

let NERDTreeMinimalUI = 1
let NERDTreeDirArrows = 1
let NERDChristmasTree = 1
let NERDTreeChDirMode = 2
let NERDTreeMapJumpFirstChild = 'gK'

let g:NERDTreeDirArrowExpandable = "\uf114"
let g:NERDTreeDirArrowCollapsible = "\uf115"

augroup ps_nerdtree
  au!

  au Filetype nerdtree setlocal nolist
  au Filetype nerdtree nnoremap <buffer> H :vertical resize -10<cr>
  au Filetype nerdtree nnoremap <buffer> L :vertical resize +10<cr>
augroup END

" NERDTress File highlighting
function! NERDTreeHighlightFile(extension, fg, bg, guifg, guibg)
  exec 'autocmd FileType nerdtree highlight ' . a:extension .' ctermbg='. a:bg .' ctermfg='. a:fg .' guibg='. a:guibg .' guifg='. a:guifg
  exec 'autocmd FileType nerdtree syn match ' . a:extension .' #^\s\+.*'. a:extension .'$#'
endfunction

call NERDTreeHighlightFile('jade', 'green', 'none', 'green', '#151515')
call NERDTreeHighlightFile('ini', 'yellow', 'none', 'yellow', '#151515')
call NERDTreeHighlightFile('md', 'blue', 'none', '#3366FF', '#151515')
call NERDTreeHighlightFile('yml', 'yellow', 'none', 'yellow', '#151515')
call NERDTreeHighlightFile('config', 'yellow', 'none', 'yellow', '#151515')
call NERDTreeHighlightFile('conf', 'yellow', 'none', 'yellow', '#151515')
call NERDTreeHighlightFile('json', 'yellow', 'none', 'yellow', '#151515')
call NERDTreeHighlightFile('html', 'yellow', 'none', 'yellow', '#151515')
call NERDTreeHighlightFile('styl', 'cyan', 'none', 'cyan', '#151515')
call NERDTreeHighlightFile('css', 'cyan', 'none', 'cyan', '#151515')
call NERDTreeHighlightFile('coffee', 'Red', 'none', 'red', '#151515')
call NERDTreeHighlightFile('js', 'Red', 'none', '#ffa500', '#151515')
call NERDTreeHighlightFile('php', 'Magenta', 'none', '#ff00ff', '#151515')
call NERDTreeHighlightFile('ds_store', 'Gray', 'none', '#686868', '#151515')
call NERDTreeHighlightFile('gitconfig', 'Gray', 'none', '#686868', '#151515')
call NERDTreeHighlightFile('gitignore', 'Gray', 'none', '#686868', '#151515')
call NERDTreeHighlightFile('bashrc', 'Gray', 'none', '#686868', '#151515')
call NERDTreeHighlightFile('bashprofile', 'Gray', 'none', '#686868', '#151515')
" }}}

let ctrlsf_opts = {'on': ['<Plug>CtrlSFVwordPath', '<Plug>CtrlSFCwordPath',
      \'CtrlSFToggle', 'CtrlSF']}
Plug 'dyng/ctrlsf.vim', ctrlsf_opts                                       " {{{3
vmap <leader>a <Plug>CtrlSFVwordPath
nmap <leader>a <Plug>CtrlSFCwordPath
nmap <leader>A :CtrlSFToggle<CR>
vmap <leader>A :CtrlSFToggle<CR>
noremap <localleader>a :CtrlSF 

let g:ctrlsf_mapping = {
    \ "quit": "",
    \ }

" For some strage reason, CtrlSF maps q to a call. Don't fuck with macros CtrlSF!
noremap q q

let g:ctrlsf_ackprg = 'ag'
" }}}
"
Plug 'junegunn/fzf.vim', {'on': ['FZF', 'FZFMru']}                        " {{{3
" Open files in horizontal split
set rtp+=/usr/local/opt/fzf
let g:fzf_launcher = "~/.vim/in_a_new_term_function %s"

function! s:buflist()
  redir => ls
  silent ls
  redir END
  return split(ls, '\n')
endfunction

function! s:bufopen(e)
  execute 'buffer' matchstr(a:e, '^[ 0-9]*')
endfunction

command! FZFMru call fzf#run({
      \   'source':  'sed "1d" $HOME/.cache/neomru/file',
      \   'sink':    'e ',
      \   'down':    '30%'
      \ })

if fn#InTmuxSession()
  " Checking that we are in a tmux session because FZF opens in a tmux pane
  " Let's overwrite CtrlP mapping if we are in Tmux, otherwise it will open
  " terminal window
  nnoremap <leader>, :FZF<CR>
  nnoremap <leader>m :FZFMru<CR>
  nnoremap <silent> <Leader>b :call fzf#run({
        \   'source':  reverse(<sid>buflist()),
        \   'sink':    function('<sid>bufopen'),
        \   'options': '+m',
        \   'down':    len(<sid>buflist()) + 2
        \ })<CR>
endif
" }}}
Plug 'Shougo/neomru.vim'
" }}}

" General Code                                                              {{{2
Plug 'honza/vim-snippets'
Plug 'SirVer/ultisnips'                                                   " {{{3
" remap Ultisnips for compatibility for YCM
let g:UltiSnipsExpandTrigger = '<C-j>'
let g:UltiSnipsJumpForwardTrigger = '<C-j>'
let g:UltiSnipsJumpBackwardTrigger = '<C-k>'

let g:UltiSnipsSnippetDirectories=["UltiSnips", "ultisnippets"]
" }}}
"
Plug 'ervandew/supertab'

let ycm_opts = {'do': './install.py --clang-completer'}
" Plug 'Valloric/YouCompleteMe', ycm_opts                                 " {{{3

let g:ycm_auto_trigger = 1
let g:ycm_min_num_of_chars_for_completion = 2
let g:ycm_use_ultisnips_completer = 1
let g:ycm_autoclose_preview_window_after_completion = 1
let g:ycm_always_populate_location_list = 0
let g:ycm_enable_diagnostic_signs = 0
let g:ycm_enable_diagnostic_highlighting = 0
let g:ycm_echo_current_diagnostic = 0
let g:ycm_open_loclist_on_ycm_diags = 0
let g:ycm_seed_identifiers_with_syntax = 0
let g:ycm_key_list_select_completion = ['<C-n>']
let g:ycm_key_list_previous_completion = ['<C-p>']
let g:ycm_key_invoke_completion = '<C-n>'
let g:ycm_collect_identifiers_from_tags_files = 0

augroup ycm
  au!
  au FileType python* setlocal omnifunc=pythoncomplete#Complete
  au FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
  au FileType html*,markdown setlocal omnifunc=htmlcomplete#CompleteTags
  au Filetype css*,scss,less setlocal omnifunc=csscomplete#CompleteCSS
  au FileType ruby setlocal omnifunc=rubycomplete#Complete
  au FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
  au FileType haskell setlocal omnifunc=necoghc#omnifunc
augroup END
" }}}

Plug 'kien/rainbow_parentheses.vim', {'for': 'clojure'}                   " {{{3
augroup rainbow_parentheses
  au!
  au FileType clojure RainbowParenthesesActivate
  au syntax clojure RainbowParenthesesLoadRound
  au syntax clojure RainbowParenthesesLoadSquare
  au syntax clojure RainbowParenthesesLoadBraces
augroup END
" }}}

Plug 'tpope/vim-commentary'                                               " {{{3
nmap <leader>c <Plug>CommentaryLine
xmap <leader>c <Plug>Commentary

augroup plugin_commentary
  au!
  au FileType clojurescript setlocal commentstring=;\ %s
  au FileType puppet,tmux setlocal commentstring=#\ %s
augroup END
" }}}

Plug 'benekastah/neomake'                                                 " {{{3
let g:neomake_javascript_enabled_makers = ['eslint']
let g:neomake_json_enabled_makers = ['jsonlint']
let g:neomake_ruby_enabled_makers = ['mri']
let g:neomake_python_enabled_makers = ['flake8']
let g:neomake_c_enabled_makers = ['gcc', 'clang-tidy']
let g:neomake_cpp_enabled_makers = ['g++', 'clang-tidy']

let g:neomake_airline = 0

let g:neomake_error_sign = {
    \ 'text': ' ',
    \ 'texthl': 'WarningMsg'
    \ }

let g:neomake_warning_sign = {
    \ 'text': '',
    \ 'texthl': 'ModeMsg'
    \ }

let g:neomake_message_sign = {
    \ 'text': ''
    \ }

au! BufWritePost * Neomake
" }}}

Plug 'majutsushi/tagbar', {'on': 'TagbarToggle'}                          " {{{3
nmap <F3> :TagbarToggle<CR>
let g:tagbar_autofocus = 1
let g:tagbar_autoclose = 1
" }}}

Plug 'kshenoy/vim-origami'                                                " {{{3
let g:OrigamiFoldAtCol = "-3"
 "}}}

Plug 'bkad/CamelCaseMotion'                                               " {{{3
map <S-W> <Plug>CamelCaseMotion_w
map <S-B> <Plug>CamelCaseMotion_b
map <S-E> <Plug>CamelCaseMotion_e

" Replace default 'iw' text-object and define 'ib' and 'ie' motions:
omap <silent> iW <Plug>CamelCaseMotion_iw
xmap <silent> iW <Plug>CamelCaseMotion_iw
omap <silent> iB <Plug>CamelCaseMotion_ib
xmap <silent> iB <Plug>CamelCaseMotion_ib
omap <silent> iE <Plug>CamelCaseMotion_ie
xmap <silent> iE <Plug>CamelCaseMotion_ie
" }}}

Plug 'xolox/vim-easytags', {'for': ['python', 'c', 'cpp', 'javascript']}
Plug 'terryma/vim-expand-region'
Plug 'rstacruz/vim-closer'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-endwise'
" }}}

" Productivity                                                              {{{2

let linediff_opts = {'on': ['Linediff', 'LinediffReset']}
Plug 'AndrewRadev/linediff.vim', linediff_opts                            " {{{3
vnoremap <leader>d :Linediff<cr>
nnoremap <leader>D :LinediffReset<cr>
" }}}

Plug 'mbbill/undotree', {'on': 'UndotreeToggle'}                          " {{{3
nnoremap <F4> :UndotreeToggle<cr>
" If undotree is opened, it is likely one wants to interact with it.
let g:undotree_SetFocusWhenToggle=1
let g:undotree_WindowLayout = 2
"}}}

" This makes the autoread option work properly for terminal
Plug 'tmux-plugins/vim-tmux-focus-events'

Plug 'benmills/vimux'                                                     " {{{3
" Prompt for a command to run
map <LocalLeader>vp :VimuxPromptCommand<CR>
" Run last command executed by VimuxRunCommand
map <LocalLeader>vl :VimuxRunLastCommand<CR>
" Inspect runner pane
map <LocalLeader>vi :VimuxInspectRunner<CR>
" Close vim tmux runner opened by VimuxRunCommand
map <LocalLeader>vq :VimuxCloseRunner<CR>
" Interrupt any command running in the runner pane
map <LocalLeader>vx :VimuxInterruptRunner<CR>
" Zoom the runner pane (use <bind-key> z to restore runner pane)
map <LocalLeader>vz :call VimuxZoomRunner()<CR>"

augroup vimux
  au!
  au VimLeave * :VimuxCloseRunner  " Close Vimux pane on Leav

  au FileType sh nnoremap <buffer> <localleader>e :VimuxRunCommand 'sh '.bufname('%')<CR>

  au FileType python* nnoremap <buffer> <localleader>ve :VimuxRunCommand 'clear; ipython '.bufname('%')<CR>
  au FileType python* nnoremap <buffer> <localleader>vr :VimuxRunCommand 'clear; ipython'<CR>

  au FileType ruby nnoremap <buffer> <localleader>ve :VimuxRunCommand 'clear; ruby '.bufname('%')<CR>
  au FileType ruby nnoremap <buffer> <localleader>vr :VimuxRunCommand 'clear; irb'<CR>

  au FileType go nnoremap <buffer> <localleader>ve :VimuxRunCommand 'clear; go run '.bufname('%')<CR>
  au FileType go nnoremap <buffer> <localleader>vr :VimuxRunCommand 'clear; gore'<CR>

  au FileType javascript nnoremap <buffer> <localleader>ve :VimuxRunCommand 'clear; node '.bufname('%')<CR>
  au FileType javascript nnoremap <buffer> <localleader>vr :VimuxRunCommand 'clear; node'<CR>
augroup END

function! VimuxSlime()                                                    " {{{
  " Send selected text to Tmux
  call VimuxSendText(@v)
  call VimuxSendKeys("Enter")
endfunction "}}}

" If text is selected, save it in the v buffer and send that buffer it to tmux
vnoremap <silent> <Plug>SendSelectedLineToTmux "vy :<C-u>call VimuxSlime()<CR>j
      \:call repeat#set("\<Plug>SendSelectedLineToTmux")<CR>
" TODO: Is not sending the indentation properly. FIXIT!
"
vmap <LocalLeader>vs <Plug>SendSelectedLineToTmux

" Select current line and send it to tmux
nnoremap <silent> <Plug>SendCurrentLineToTmux mz^vg_"vy :<C-u>call VimuxSlime()<CR>`zj
      \:call repeat#set("\<Plug>SendCurrentLineToTmux")<CR>
nmap <LocalLeader>vs <Plug>SendCurrentLineToTmux
" }}}

Plug 'tpope/vim-fugitive'                                                 " {{{3
nnoremap <leader>gd :Gdiff<cr>
nnoremap <leader>gs :Gstatus<cr>
nnoremap <leader>gw :Gwrite<cr>
nnoremap <leader>ga :Gadd<cr>
nnoremap <leader>gb :Gblame<cr>
nnoremap <leader>go :Gcheckout<cr>
nnoremap <leader>gc :Gcommit<cr>
nnoremap <leader>gm :Gmove<cr>
nnoremap <leader>gr :Gremove<cr>
nnoremap <leader>gl :Shell git gl -18<cr>:wincmd \|<cr>

" Hub
nnoremap <leader>gh :Gbrowse<cr>
vnoremap <leader>gh :Gbrowse<cr>

augroup ft_fugitive
  au!
  au BufNewFile,BufRead .git/index setlocal nolist
augroup END
" }}}

Plug 'airblade/vim-gitgutter'                                             " {{{3
let g:gitgutter_map_keys = 0
nmap [h <Plug>GitGutterPrevHunk
nmap ]h <Plug>GitGutterNextHunk
" }}}

Plug 'christoomey/vim-tmux-navigator'
Plug 'vim-scripts/listmaps.vim', {'on': 'Listmaps'}
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-repeat'
Plug 'ConradIrwin/vim-bracketed-paste'
Plug 'kshenoy/vim-signature'

let scratch_opts = {'on': [
      \'Scratch', 'ScratchInsert', 'ScratchPreview', 'ScratchSelection',
      \'<Plug>(scratch-insert-reuse)', '<Plug>(scratch-insert-clear)', 
      \'<Plug>(scratch-selection-reuse)', '<Plug>(scratch-selection-clear)'
      \ ]}
Plug 'mtth/scratch.vim', scratch_opts                                     " {{{3
let g:scratch_horizontal = 0
let g:scratch_top = 0
let g:scratch_height = 80
let g:scratch_no_mappings = 1
let g:scratch_insert_autohide = 0

nmap <localleader>s <Plug>(scratch-insert-reuse)
nmap <localleader>S <Plug>(scratch-insert-clear)
xmap <localleader>s <Plug>(scratch-selection-reuse)
xmap <localleader>S <Plug>(scratch-selection-clear)
" }}}
" }}}

" Language-specific                                                         {{{2
Plug 'fatih/vim-go', {'for': 'go'}                                        " {{{3
let g:go_doc_keywordprg_enabled = 0

let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_structs = 1

" prevent "vim-go" from showing a quickfix window when |g:go_fmt_command| fails
let g:go_fmt_fail_silently = 1
" }}}
Plug 'tejr/vim-tmux'
Plug 'tpope/vim-git'
Plug 'lambdatoast/elm.vim'

" Markdown
Plug 'shime/vim-livedown', {'for': ['markdown']}
Plug 'plasticboy/vim-markdown', {'for': ['markdown']}

" Python
Plug 'hynek/vim-python-pep8-indent'
Plug 'davidhalter/jedi-vim'                                               " {{{3
let g:jedi#auto_initialization = 1
" this prevents jedi to mess with completeopt
let g:jedi#auto_vim_configuration = 0
let g:jedi#completions_enabled = 0
let g:jedi#smart_auto_mappings = 1
let g:jedi#show_call_signatures = 1

let g:jedi#goto_command = "<C-]>"
let g:jedi#documentation_command = '<localleader>m'
let g:jedi#goto_assignments_command = ""
let g:jedi#goto_definitions_command = ""
let g:jedi#usages_command = ""
let g:jedi#rename_command = ""
" }}}

" Css, Scss, Less
Plug 'hail2u/vim-css3-syntax'
Plug 'groenewege/vim-less'
Plug 'rstacruz/vim-hyperstyle'

" Html and templating languages
Plug 'othree/html5.vim'                                                   " {{{3
let g:event_handler_attributes_complete = 0
let g:rdfa_attributes_complete = 0
let g:microdata_attributes_complete = 0
let g:atia_attributes_complete = 0
" }}}
Plug 'mustache/vim-mustache-handlebars'
Plug 'tpope/vim-haml'

" Javascript
let jsbeautify_opts = {'do': 'npm install -g js-beautify'}
Plug 'maksimr/vim-jsbeautify', jsbeautify_opts                            " {{{3
augroup ft_jsbeautify
  au!
  autocmd FileType javascript nnoremap <buffer>  <localleader>= :call JsBeautify()<cr>
  autocmd FileType javascript vnoremap <buffer>  <localleader>= :call RangeJsBeautify()<cr>
  " for html
  autocmd FileType html* nnoremap <buffer> <localleader>= :call HtmlBeautify()<cr>
  autocmd FileType html* vnoremap <buffer> <localleader>= :call RangeHtmlBeautify()<cr>
  " for css or scss
  autocmd FileType css* nnoremap <buffer> <localleader>= :call CSSBeautify()<cr>
  autocmd FileType css* vnoremap <buffer> <localleader>= :call RangeCSSBeautify()<cr>
augroup END
" }}}
Plug 'elzr/vim-json'                                                      " {{{3
let g:vim_json_syntax_conceal = 0
" }}}
Plug 'jason0x43/vim-js-indent'                                            " {{{3
let g:js_indent_flat_switch = 1
" }}}
Plug 'heavenshell/vim-jsdoc'
" supports latest language features
Plug 'othree/es.next.syntax.vim', { 'for': 'javascript' } 
Plug 'othree/yajs.vim'
Plug 'othree/javascript-libraries-syntax.vim'

" Ruby
Plug 'vim-ruby/vim-ruby'
Plug 'tpope/vim-rails'
" }}}

" Style                                                                     {{{2
Plug 'morhetz/gruvbox'
Plug 'ryanoasis/vim-devicons'
" Plug 'inside/vim-search-pulse'
" }}}

call plug#end()
" }}}

" FileTypes                                                                 {{{1
augroup ft_common                                                         " {{{2
  au!
  au BufNewFile,BufRead *.hamlc set filetype=haml
  au BufNewFile,BufRead *jshintrc set filetype=json
augroup END "}}}

augroup ft_c                                                              " {{{2
  au!

  au FileType c setlocal foldmethod=marker
  au FileType c setlocal foldmarker={,}
augroup END "}}}

augroup ft_css                                                            " {{{2
  au!

  au FileType css,scss,less setlocal foldmethod=marker
  au FileType css,scss,less setlocal foldmarker={,}
  au FileType css,scss,less setlocal iskeyword+=-

  " Make {<cr> insert a pair of brackets in such a way that the cursor is correctly
  " positioned inside of them AND the following code doesn't get unfolded.
  au FileType css,scss,less inoremap <buffer> {<cr> {}<left><cr><space><space>.<cr><esc>kA<bs>
augroup END "}}}

augroup ft_clojure                                                        " {{{2
  au!

  au BufNewFile,BufRead riemann.config set filetype=clojure

  au FileType clojure noremap <buffer> () :<c-u>call PareditWrap("(", ")")<cr>
  au FileType clojure noremap <buffer> )( :<c-u>call PareditSplice()<cr>
  au FileType clojure noremap <buffer> (( :<c-u>call PareditMoveLeft()<cr>
  au FileType clojure noremap <buffer> )) :<c-u>call PareditMoveRight()<cr>
  au FileType clojure noremap <buffer> (j :<c-u>call PareditJoin()<cr>
  au FileType clojure noremap <buffer> (s :<c-u>call PareditSplit()<cr>
  au FileType clojure noremap <buffer> [ :<c-u>call PareditSmartJumpOpening(0)<cr>
  au FileType clojure noremap <buffer> ] :<c-u>call PareditSmartJumpClosing(0)<cr>

  " Indent top-level form.
  au FileType clojure nmap <buffer> <localleader>= mz99[(v%='z
augroup END "}}}

augroup ft_clojurescript                                                  " {{{2
  au!

  au BufNewFile,BufRead *.cljs set filetype=clojurescript

  au FileType clojurescript call TurnOnClojureFolding()
  au FileType clojurescript nmap <buffer> <localleader>= v((((((((((((=%
augroup END "}}}

augroup ft_help                                                           " {{{2
  au!

  au BufWinEnter *.txt if &ft == 'help' | wincmd L | endif

  au FileType help setlocal textwidth=78
augroup END "}}}

augroup ft_html_and_templates                                             " {{{2
  au!

  au BufNewFile,BufRead *.html set syntax=mustache
  au BufNewFile,BufRead *.hbs,*.handlebars set ft=mustache syntax=mustache

  au FileType html*,mustache setlocal foldlevel=99
  au FileType html*,mustache setlocal foldmethod=manual
  au FileType html*,mustache setlocal shiftwidth=4
  au FileType html*,mustache setlocal softtabstop=4
  au FileType html*,mustache setlocal tabstop=4

  au FileType mustache setlocal commentstring={{!\ %s\ }}
  au FileType handlebars setlocal commentstring={{!\ %s\ }}
  au FileType htmldjango setlocal commentstring={#\ %s\ #}

  " Use <localleader>f to fold the current tag.
  au FileType html*,mustache nnoremap <buffer> <localleader>f Vatzf

  " Indent tag
  au FileType html*,mustache nnoremap <buffer> <localleader>- Vat=

  " handlebars tags
  au FileType html*,mustache inoremap <buffer> <c-b> {{<space><space>}}<left><left><left>

  " HTML tag closing
  au FileType html*,mustache inoremap <buffer> <C-_> <space><bs><esc>:call InsertCloseTag()<cr>a
augroup END "}}}

augroup ft_java                                                           " {{{2
  au!

  au FileType java setlocal foldmethod=marker
  au FileType java setlocal foldmarker={,}
  au FileType java setlocal makeprg=javac\ %
  au FileType java setlocal errorformat=%A%f:%l:\ %m,%-Z%p^,%-C%.%#
augroup END "}}}

augroup ft_json                                                           " {{{2
au FileType json noremap <buffer> <localleader>= :<c-u>%!python -m json.tool<cr>
augroup END "}}}

augroup ft_javascript                                                     " {{{2
  au!

  au BufNewFile,BufRead *.es6 set filetype=javascript

  au FileType javascript setlocal foldmethod=marker
  au FileType javascript setlocal foldmarker={,}
  au FileType javascript setlocal foldtext=getline(v:foldstart)
  au FileType javascript setlocal shiftwidth=2 softtabstop=2 tabstop=2
  au FileType javascript setlocal foldlevel=99
  au FileType javascript call MakeSpacelessBufferIabbrev('clog', 'console.log();<left><left>')
augroup END "}}}

augroup ft_man                                                            " {{{2
  au!

  au FileType man nnoremap <buffer> <cr> :q<cr>
augroup END "}}}

augroup ft_markdown                                                       " {{{2
  au!

  au BufNewFile,BufRead *.{md,mdown,mkd,mkdn,markdown,mdwn} set filetype=markdown

  au FileType markdown normal! zR
  au FileType markdown setlocal foldlevel=1
  au FileType markdown setlocal nofoldenable    " disable folding

  " Use <localleader>1/2/3 to add headings.
  au FileType markdown nnoremap <buffer> <localleader>1 yypVr=:redraw<cr>
  au FileType markdown nnoremap <buffer> <localleader>2 yypVr-:redraw<cr>
  au FileType markdown nnoremap <buffer> <localleader>3 mzI###<space><ESC>`zllll
augroup END "}}}

augroup ft_nginx                                                          " {{{2
  au!

  au BufNewFile,BufRead /etc/nginx/conf/*                      setlocal ft=nginx
  au BufNewFile,BufRead /etc/nginx/sites-available/*           setlocal ft=nginx
  au BufNewFile,BufRead /usr/local/etc/nginx/sites-available/* setlocal ft=nginx
  au BufNewFile,BufRead vhost.nginx                            setlocal ft=nginx

  au FileType nginx setlocal foldmethod=marker
  au FileType nginx setlocal foldmarker={,}
augroup END "}}}

augroup ft_python                                                         " {{{2
  au!

  au BufNewFile,BufRead admin.py,urls.py,models.py,views.py,settings.py,forms.py,
        \common_settings.py set filetype=python.django
  au BufNewFile,BufRead settings.py,common_settings.py setlocal foldmethod=marker

  au FileType python* setlocal define=^\s*\\(def\\\\|class\\)
  au FileType python* setlocal shiftwidth=4 softtabstop=4 tabstop=4

  " Jesus, Python.  Five characters of punctuation for a damn string?
  au FileType python* inoremap <buffer> <c-g> _(u'')<left><left>
  au FileType python* inoremap <buffer> <c-b> """"""<left><left><left>

  " Jesus tapdancing Christ, built-in Python syntax, you couldn't let me
  " override this in a normal way, could you?
  au FileType python* if exists("python_space_error_highlight") | unlet python_space_error_highlight | endif
augroup END "}}}

augroup ft_qf                                                             " {{{2
  au!

  au FileType qf setlocal colorcolumn=0
  au FileType qf setlocal nolist
  au FileType qf setlocal nocursorline
  au FileType qf setlocal nowrap
  au FileType qf setlocal textwidth=0
augroup END "}}}

augroup ft_ruby                                                           " {{{2
  au!

  au BufNewFile,BufRead Vagrantfile,Capfile set filetype=ruby
  au FileType ruby setlocal foldmethod=syntax
  au FileType ruby nnoremap <buffer> <localleader>b :call <SID>toggleStatement("require 'pry'; binding.pry  # XXX BREAKPOINT")<CR>
augroup END "}}}

augroup ft_scala                                                          " {{{2
  au!

  au FileType scala let b:dispatch = 'mvn -B package install'
  au FileType scala compiler maven

  au FileType scala setlocal foldmethod=marker
  au FileType scala setlocal foldmarker={,}

  au FileType scala nnoremap <buffer> <localleader>S :SortScalaImports<cr>
  au FileType scala nnoremap <buffer> M :call scaladoc#Search(expand("<cword>"))<cr>
  au FileType scala vnoremap <buffer> M "ry:call scaladoc#Search(@r)<cr>
  au FileType scala nmap <buffer> <localleader>( ysiwbi
  au FileType scala nmap <buffer> <localleader>[ ysiwri
augroup END "}}}

augroup ft_switchfly                                                      " {{{2
  au!

  au BufRead,BufNewFile /Users/eruiz/switchfly/repos/* setlocal ts=4 sw=4 sts=4

  " Make {<cr> insert a pair of brackets in such a way that the cursor is correctly
  " positioned inside of them AND the following code doesn't get unfolded.
  au BufRead,BufNewFile /Users/eruiz/switchfly/repos/* inoremap <buffer> {<cr> {}<left><cr><space><space><space><space>.<cr><esc>kA<bs>
augroup END "}}}

augroup ft_sql                                                            " {{{2
  au!

  au BufNewFile,BufRead *.sql.pre,*.sql.post set filetype=sql
  au FileType sql setlocal foldmethod=indent
augroup END "}}}

augroup ft_vim                                                            " {{{2
  au!

  " Reload vimrc
  au BufWritePost vimrc so %

  au FileType vim setlocal foldmethod=marker
  au FileType vim setlocal shiftwidth=2
  au FileType vim setlocal softtabstop=2
  au FileType vim setlocal tabstop=2
augroup END "}}}

augroup ft_xml                                                            " {{{2
  au!

  au FileType xml setlocal foldmethod=manual

  " Use <localleader>f to fold the current tag.
  au FileType xml nnoremap <buffer> <localleader>f Vatzf

  " Indent tag
  au FileType xml nnoremap <buffer> <localleader>= Vat=
augroup END "}}}

" }}}

" Basic options                                                             {{{1
filetype plugin indent on

" Use Ag as default grep if available
if executable('ag')
  set grepprg=ag\ --nogroup\ --nocolor\ --column
  set grepformat=%f:%l:%c:%m
  command! -nargs=+ -bang Ag silent! grep <args> | redraw! | botright copen
endif

scriptencoding utf-8

set modeline
set autoindent                  " Automatically set the indent of a new line (local to buffer)
set noshowmode                  " Hide current mode down the bottom
set noshowcmd                   " Hide incomplete cmds down the bottom
set scrolljump=10
set hidden
set visualbell                  " No sounds
set t_vb=                       " Visual bell disabled on Linux
set ttyfast                     " Improves redrawing
" set ttyscroll=10
set noruler                     " Never show current positions along the bottom
set backspace=indent,eol,start  " Allow backspace in insert mode
set number                      " Line numbers are good
set laststatus=2                " Never show the status line
set history=1000                " Store lots of :cmdline history
set list                        " Hide invisible chars
set listchars=tab:▸\ ,eol:↵,extends:↷,precedes:↶,trail:·
set shell=/bin/zsh\ --login
set lazyredraw                  " Do not redraw while running macros
set matchtime=3                 " how many tenths of a second to blink
set showbreak=↪
set splitbelow
set splitright
set iskeyword+=-
set fillchars=diff:⣿,vert:┃,stl:━,stlnc:━     " Customize text for closed folds
" set fillchars=diff:⣿,vert:│,stl:-,stlnc:     " Customize text for closed folds
set autowrite
set autoread                    " Reload files changed outside vim
set shiftround                  " When at 3 spaces, and I hit > ... go to 4, not 5
set title                       " set the terminal title to the current file
set linebreak
set dictionary=/usr/share/dict/words
set spellfile=~/.vim/custom-dictionary.utf-8.add
set pastetoggle=<F2>            " Use it for pasting large amounts of text into Vim, disabling all kinds of smartness and just pasting a whole buffer of text
set ssop-=options    " do not store global and local values in a session
set ssop-=folds      " do not store folds
set nocursorline
set statusline=
" set statusline+=━

set statusline+=\ %#GruvboxYellow#%f%*
set statusline+=%(\ [%Y%R%W%M]%)
set statusline+=\ %=
" set statusline+=▏%(\ %l,%c%V%)▕\━\━
set statusline+=\ \ %P▕
set statusline+=%#GruvboxOrange#
set statusline+=%{neomake#statusline#LoclistStatus(' ')}
" set tags=./tags

if v:version > 702
  set norelativenumber            " It is much faster
  set undofile
  set undoreload=10000
  set colorcolumn=+1
  syntax sync minlines=256
endif

" Don't try to highlight lines longer than 255 characters.
set synmaxcol=255

" Time out on key codes but not mappings.
" Basically this makes terminal Vim work sanely.
set notimeout
set ttimeout
set ttimeoutlen=10

" Make Vim able to edit crontab files again.
set backupskip=/tmp/*,/private/tmp/*"

" Better Completion
" set complete=.,w,b,u,t
set complete=.,b,u,]
" get rid of `preview` option to prevent scratch window to pop up
set completeopt=longest,menuone

" Resize splits when the window is resized
augroup resize_splits
  au!
  au VimResized * :wincmd =
augroup END

" Wildmenu completion                                                       {{{2
set wildmenu
set wildmode=list:longest

set wildignore+=.hg,.git,.svn                    " Version control
set wildignore+=*.aux,*.out,*.toc                " LaTeX intermediate files
set wildignore+=*.jpg,*.bmp,*.gif,*.png,*.jpeg   " binary images
set wildignore+=*.o,*.obj,*.exe,*.dll,*.manifest " compiled object files
set wildignore+=*.bak,*.?~,*.??~,*.???~,*.~      " Backup files
set wildignore+=*.spl                            " compiled spelling word lists
set wildignore+=*.sw?                            " Vim swap files
set wildignore+=*.DS_Store                       " OSX bullshit
set wildignore+=*.jar                            " Java archives
set wildignore+=*.pyc                            " Python bite code
set wildignore+=*.luac                           " Lua byte code
set wildignore+=*.orig                           " Merge resolution files
" }}}

" Line Return                                                               {{{2
" Make sure Vim returns to the same line when you reopen a file.
" Thanks, Amit
augroup line_return
  au!
  au BufReadPost *
        \ if line("'\"") > 0 && line("'\"") <= line("$") |
        \     execute 'normal! g`"zvzz' |
        \ endif
augroup END
" }}}

" Tabs, spaces, wrapping                                                    {{{2
set tabstop=2
set shiftwidth=2
set softtabstop=2
set expandtab
set wrap
set textwidth=80
set formatoptions=qrn1

if v:version > 703 || v:version == 703 && has("patch541")
  set formatoptions+=j " Delete comment character when joining commented lines
endif
" }}}

" Backups                                                                   {{{2
set backup                        " enable backups
set noswapfile                    " It's 2012, Vim.

set undodir=$HOME/.vim/tmp/undo/     " undo files
set backupdir=$HOME/.vim/tmp/backup/ " backups
set directory=$HOME/.vim/tmp/swap/   " swap files

" Make those folders automatically if they don't already exist.
if !isdirectory(expand(&undodir))
  call mkdir(expand(&undodir), "p")
endif
if !isdirectory(expand(&backupdir))
  call mkdir(expand(&backupdir), "p")
endif
if !isdirectory(expand(&directory))
  call mkdir(expand(&directory), "p")
endif
" }}}

" }}}

" Abbreviations                                                             {{{1
function! EatChar(pat)
  let c = nr2char(getchar(0))
  return (c =~ a:pat) ? '' : c
endfunction

function! MakeSpacelessIabbrev(from, to)
  execute "iabbrev <silent> ".a:from." ".a:to."<C-R>=EatChar('\\s')<CR>"
endfunction
function! MakeSpacelessBufferIabbrev(from, to)
  execute "iabbrev <silent> <buffer> ".a:from." ".a:to."<C-R>=EatChar('\\s')<CR>"
endfunction

call MakeSpacelessIabbrev('gh/',  'http://github.com/')
call MakeSpacelessIabbrev('ghe/',  'http://github.com/eduarbo')
call MakeSpacelessIabbrev('me/',  'Eduardo Ruiz Macias')
call MakeSpacelessIabbrev('em/',  'eduarbo@gmail.com')

iabbrev ldis ಠ_ಠ
iabbrev lsad ಥ_ಥ
iabbrev lhap ಥ‿ಥ
iabbrev lmis ಠ‿ಠ
" }}}

" Convenience mappings                                                      {{{1
" Better than jj
inoremap jk <Esc>

" Terminal sends Nul when C-space is pressed
noremap <C-space> <Nul>

" nmap <Nul> <Esc>
" imap <Nul> <Esc>`^
" omap <Nul> <Esc>
" cmap <Nul> <Esc>
" xmap <Nul> <Esc>
" vmap <Nul> <Esc>gV

" Fuck you, help key.
noremap  <F1> <ESC>
inoremap <F1> <ESC>

" Stop it, hash key.
inoremap # X<BS>#

" Kill window
nnoremap K :q<cr>

" Sort lines
nnoremap <leader>S vip:!sort<cr>
vnoremap <leader>S :!sort<cr>

" Close Preview window
noremap <localleader>p <C-w>z

nnoremap ; :

" Use c-\ to do c-] but open it in a new split.
nnoremap <c-\> <c-w>v<c-]>zvzz

" Insert New Line (not compatible with terminal)
nnoremap <S-Enter> O<ESC> " awesome, inserts new line without going into insert mode
nnoremap <Enter> o<ESC>

" Go to previous file
nnoremap <leader>/ <C-^>

" Sigh.
nnoremap <leader><cr> :silent !/usr/local/bin/ctags -R . && sed -i .bak -E -e '/^[^	]+	[^	]+.py	.+v$/d' tags<cr>:redraw!<cr>

" Clean trailing whitespace
nnoremap <leader>w mz:%s/\s\+$//<cr>:let @/=''<cr>`z

" Copy/Paste to and from Desktop Environment
noremap <leader>y "+y
noremap <leader>p "+p

" Make the Y behavior similar to D & C
nnoremap Y y$

" Insert the directory of the current buffer in command line mode
cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h').'/' : '%%'

" Select entire buffer
nnoremap vaa ggvGg_
nnoremap Vaa ggVG

" Panic Button
nnoremap <leader>` mzggg?G`z

" Emacs bindings in command line mode
cnoremap <c-a> <home>
cnoremap <c-e> <end>

" teseasdf asfd sadf asdfasdfasdf asdfas dfasdfsadfasdfasdfsdf sdfasdf sadf asdf asdf asfd sad fas dfa sdf asdf 
" Formatting, TextMate-style
vnoremap <silent> <Plug>FormatSelection gq
      \:call repeat#set("\<Plug>FormatSelection")<CR>
nnoremap <silent> <Plug>FormatLine ^vg_gq$
      \:call repeat#set("\<Plug>FormatLine")<CR>

vmap Q <Plug>FormatSelection
nmap Q <Plug>FormatLine

" Easier linewise reselection
nnoremap <leader>V V`]

"identation
vnoremap < <gv
vnoremap > >gv

" Indent/dedent/autoindent what you just pasted.
nnoremap <lt>> V`]<
nnoremap ><lt> V`]>
nnoremap =- V`]=

" indent all
nnoremap <localleader>= mzgg=G`z

" Keep the cursor in place while joining lines
nnoremap J mzJ`z

" Split line (sister to [J]oin lines)
" The normal use of S is covered by cc, so don't worry about shadowing it.
nnoremap S i<cr><esc>^mwgk:silent! s/\v +$//<cr>:noh<cr>`w

" Select (charwise) the contents of the current line, excluding indentation.
" Great for pasting Python lines into REPLs.
nnoremap vv ^vg_

" Sudo to write
cnoremap w!! w !sudo tee % >/dev/null

" Typos
command! -bang E e<bang>
command! -bang Q q<bang>
command! -bang W w<bang>
command! -bang QA qa<bang>
command! -bang Qa qa<bang>
command! -bang Wa wa<bang>
command! -bang WA wa<bang>
command! -bang Wq wq<bang>
command! -bang WQ wq<bang>

" I suck at typing.
nnoremap <localleader>- ==
vnoremap - =

" Toggle [i]nvisible characters
nnoremap <leader>i :set list!<cr>

" Unfuck my screen
nnoremap U :syntax sync fromstart<cr>:redraw!<cr>

" my Function Mappings                                                      {{{2

" Removes current file and wipe out the buffer
command! Rm call delete(expand('%')) | bdelete!

" Zoom / Restore window
command! ZoomToggle call fn#ZoomToggle()
nnoremap <silent> <leader>z :ZoomToggle<CR>

" Google it
vnoremap <silent> <leader>? "gy<Esc>:call fn#GoogleIt(@g)<CR>:redraw!<CR>

" Show the stack of syntax hilighting groups affecting whatever is under the
" cursor.
command! SynStack call fn#SynStack()
nnoremap <F7> :call fn#SynStack()<CR>

command! ToggleProfile call fn#ToggleProfile()
nnoremap <localleader>\ :call fn#ToggleProfile()<CR>

" Encode and Decode

" TODO: Make a command to encode and decode and remove mappings
" command! Base64encode -nargs=1 silent! system('base64', <args>)
" vnoremap <localleader>d c<c-r>=system('base64 --decode', @")<cr><esc>
" vnoremap <localleader>e c<c-r>=system('base64', @")<cr><esc>

" QuickFix and Location-list cycle mappings compatible with repeat plugin. This
" fixes Syntastic error jumping too
nnoremap <silent> <Plug>QuickfixNext     :call fn#CycleCommand('cnext', 'cfirst')<CR>
      \:silent! call repeat#set("\<Plug>QuickfixNext")<CR>
nnoremap <silent> <Plug>QuickfixPrevious :call fn#CycleCommand('cprev', 'clast')<CR>
      \:silent! call repeat#set("\<Plug>QuickfixPrevious")<CR>
nnoremap <silent> <Plug>LocationNext     :call fn#CycleCommand('lnext', 'lfirst')<CR>
      \:silent! call repeat#set("\<Plug>LocationNext")<CR>
nnoremap <silent> <Plug>LocationPrevious :call fn#CycleCommand('lprev', 'llast')<CR>
      \:silent! call repeat#set("\<Plug>LocationPrevious")<CR>

" Override unimpaired mappings
augroup syntastic_mappings
  autocmd!
  autocmd VimEnter * nmap <silent> ]q <Plug>QuickfixNext
  autocmd VimEnter * nmap <silent> [q <Plug>QuickfixPrevious
  autocmd VimEnter * nmap <silent> ]l <Plug>LocationNext
  autocmd VimEnter * nmap <silent> [l <Plug>LocationPrevious
augroup END
" }}}
" }}}

" Quick editing                                                             {{{1
nnoremap <leader>ec :vsplit ~/.dotfiles/editorconfig<cr>
nnoremap <leader>ed :vsplit ~/.vim/custom-dictionary.utf-8.add<cr>
nnoremap <leader>eg :vsplit ~/.dotfiles/git/gitconfig<cr>
nnoremap <leader>ej :vsplit ~/.dotfiles/jshintrc<cr>
nnoremap <leader>et :vsplit ~/.dotfiles/tmux/tmux.conf<cr>
nnoremap <leader>ev :vsplit ~/.dotfiles/vim/.vimrc<cr>
nnoremap <leader>ez :vsplit ~/.dotfiles/zsh/zshrc<cr>

nnoremap <leader>ed :vsplit ~/.vim/custom-dictionary.utf-8.add<cr>
nnoremap <leader>eb :vsplit ~/.dotfiles/bash/bash_profile<cr>
nnoremap <leader>eg :vsplit ~/.dotfiles/git/gitconfig<cr>
" }}}

" Searching and movement                                                    {{{1
" Use sane regexes.
nnoremap / /\v
vnoremap / /\v

set ignorecase
set smartcase
set incsearch
set showmatch
set hlsearch
set gdefault

set scrolloff=3
set sidescroll=1
set sidescrolloff=10

set virtualedit+=block

nnoremap <tab> %
silent! unmap [%
silent! unmap ]%

" Made D behave
nnoremap D d$

" Don't move on *
nnoremap * *<c-o>

" Keep search matches in the middle of the window.
nnoremap n nzzzv
nnoremap N Nzzzv
noremap <silent> <leader><space> :noh<cr>:call clearmatches()<cr>

" Same when jumping around
nnoremap g; g;zz
nnoremap g, g,zz
nnoremap <c-o> <c-o>zz

" Easier to type, and I never use the default behavior.
noremap H ^
noremap L $
vnoremap L g_

" Heresy
inoremap <c-a> <esc>I
inoremap <c-e> <esc>A

" gi already moves to "last place you exited insert mode", so we'll map gI to
" something similar: move to last change
nnoremap gI `.

" Fix linewise visual selection of various text objects
nnoremap VV V
nnoremap Vit vitVkoj
nnoremap Vat vatV
nnoremap Vab vabV
nnoremap VaB vaBV

" Toggle "keep current line in the center of the screen" mode
nnoremap <leader>C :let &scrolloff=999-&scrolloff<cr>

" Directional Keys                                                          {{{2
" It's 2012.
noremap j gj
noremap k gk
noremap gj j
noremap gk k

" this allows all window commands in insert mode and i'm not accidentally deleting words anymore :-)
inoremap <C-w> <C-o><C-w>

noremap - <C-w>-
noremap + <C-w>+

nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

nnoremap <leader>v <C-w>v
nnoremap <leader>s <C-w>s
" }}}
" Visual Mode */# from Scrooloose                                           {{{2
function! s:VSetSearch()
  let temp = @@
  norm! gvy
  let @/ = '\V' . substitute(escape(@@, '\'), '\n', '\\n', 'g')
  let @@ = temp
endfunction

vnoremap * :<C-u>call <SID>VSetSearch()<CR>//<CR><c-o>
vnoremap # :<C-u>call <SID>VSetSearch()<CR>??<CR><c-o>
" }}}
" }}}

" Folding                                                                   {{{1
set foldlevelstart=0
set foldmethod=marker

" Space to toggle folds.
nnoremap <localleader><localleader> za
vnoremap <localleader><localleader> za

" Make zO recursively open whatever top level fold we're in, no matter where the
" cursor happens to be.
nnoremap zO zCzO

" "Focus" the current line.  Basically:
"
" 1. Close all folds.
" 2. Open just the folds containing the current line.
" 3. Move the line to a little bit (15 lines) above the center of the screen.
" 4. Pulse the cursor line.  My eyes are bad.
"
" This mapping wipes out the z mark, which I never use.
"
" I use :sus for the rare times I want to actually background Vim.
if exists('g:vim_search_pulse_mode')
  nnoremap <c-z> mzzMzvzz15<c-e>`z:call search_pulse#Pulse()<CR>
else
  nnoremap <c-z> mzzMzvzz15<c-e>`z
endif

set foldtext=fn#MyFoldText()

" }}}

" Environments (GUI/Console)                                                {{{1

" Colorscheme                                                               {{{2
syntax on

set background=dark

" Guvbox specific
let g:gruvbox_contrast_dark = "hard"
let g:gruvbox_invert_selection = 0
let g:gruvbox_sign_column = "dark0_hard"

colorscheme gruvbox

" Keep same color as linenumber column

" Override template's colors
call fn#HL('Folded', ['#7c6f64', 243])
call fn#HL('Comment', ['#665c54', 241])
call fn#HL('LineNr', ['#3c3836', 237], ['#1d2021', 234])
call fn#HL('TabLineSel', ['#fabd2f', 214], ['#1d2021', 234], 'bold,')
call fn#HL('jsonCommentError', ['#fabd2f', 167])
call fn#HL('helpExample', ['#a89984', 246])
call fn#HL('StatusLine', ['#fabd2f', 214])
call fn#HL('StatusLineNC', ['#3c3836', 237])
call fn#HL('VertSplit', ['#3c3836', 237])
call fn#HL('ColorColumn', ['NONE', 'NONE'], ['#282828', 235])
" Setting background past textwidth characters

let &colorcolumn=join(range(81,999),",")
" }}}

if has('gui_running')  " GUI Vim

  " Remove all the UI cruft
  set go-=T                       " Hide the toolbar
  set go-=m                       " Hide the menu
  set go-=l
  set go-=L
  set go-=r
  set go-=R
  set go+=c

  highlight SpellBad term=underline gui=undercurl guisp=Orange

  map <silent> <F11>
        \    :call system("wmctrl -ir " . v:windowid . " -b toggle,fullscreen")<CR>

  " Use First tmux pane on Macvim
  let g:VimuxUseExistingPaneWithIndex = 1

  augroup on_gui_running
    autocmd!
    " custom event to use it on-demand loading of plugins
    autocmd VimEnter * silent! GuiRunning
  augroup END

  if has("gui_macvim")
    " Full screen means FULL screen
    set fuoptions=maxvert,maxhorz

  elseif has("gui_gtk2")
    " Maximize gvim window.
    set lines=999 columns=999

  else
    " Non-MacVim GUI
  end

elseif $TMUX != ''  " In tmux session

  " These sequences tell tmux to change the cursor shape to a bar or block.

  " When entering insert mode, change the cursor to a bar.
  let &t_SI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=1\x7\<Esc>\\"
  " When exiting insert mode, change it back to a block.
  let &t_EI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=0\x7\<Esc>\\"

else  " Console Vim. For me, this means iTerm2

  " These sequences tell iterm2 to change the cursor shape to a bar or block.

  " When entering insert mode, change the cursor to a bar.
  let &t_SI = "\<Esc>]50;CursorShape=1\x7"
  " When exiting insert mode, change it back to a block.
  let &t_EI = "\<Esc>]50;CursorShape=0\x7"

  set t_Co=256

  " Mouse support
  set mouse=a

endif

if has('nvim')
  " Show cursor bar in insert mode for neovim
  let $NVIM_TUI_ENABLE_CURSOR_SHAPE=1
endif

set guifont=Hack:h13
" }}}

" TODO                                                                      {{{1
" * Add more customized snippets
" * Fix annoying bug with YCM that's clears whole word when type
" * Check if it's worth using Unite
" }}}
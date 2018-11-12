""""""""""
" kuzzmi .vimrc
"
""""""""""

" Plugins {{{
" ============
call plug#begin('~/.vim/plugged')

Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }

Plug 'w0rp/ale'

" Elm support
Plug 'pbogut/deoplete-elm'
Plug 'ElmCast/elm-vim'

" Haskell stuff
Plug 'neovimhaskell/haskell-vim'
Plug 'parsonsmatt/intero-neovim'
Plug 'alx741/vim-hindent'

" Distraction free editing
Plug 'junegunn/goyo.vim'
Plug 'junegunn/limelight.vim'

" Commenting stuff
Plug 'tomtom/tcomment_vim'

" Subvert + case switching
Plug 'tpope/vim-abolish'

" Speeddating
Plug 'tpope/vim-speeddating'

" Surround plugin
Plug 'tpope/vim-surround'

" Repeater for plugins
Plug 'tpope/vim-repeat'

" Some libraries
Plug 'tomtom/tlib_vim'

" Go support
Plug 'fatih/vim-go'

" Syntax checking
Plug 'scrooloose/syntastic'
Plug 'othree/javascript-libraries-syntax.vim'

" Unite. Cool stuff
Plug 'Shougo/unite.vim'

" Plugin for Unite to search MRU sources
Plug 'Shougo/neomru.vim'

" Vimproc
Plug 'Shougo/vimproc.vim'

" Vim Addon MW utils
Plug 'MarcWeber/vim-addon-mw-utils'

" Vim Airline
Plug 'bling/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Snippets
Plug 'garbas/vim-snipmate'
Plug 'honza/vim-snippets'

" WOOOHOOO Emmet!!!
Plug 'mattn/emmet-vim'

" Javascript
Plug 'pangloss/vim-javascript'

" Table mode
Plug 'dhruvasagar/vim-table-mode'

" ES6
Plug 'isRuslan/vim-es6'

" Markdown folding
Plug 'nelstrom/vim-markdown-folding'

" Presentation making tool
Plug 'sotte/presenting.vim'

" Silver Searcher
Plug 'rking/ag.vim'

" Vim Easy Align
Plug 'junegunn/vim-easy-align'

" Argumentative
Plug 'PeterRincker/vim-argumentative'

" Vimwiki
Plug 'vimwiki/vimwiki', { 'branch': 'dev' }

call plug#end()
" }}}
" Syntax highlighting {{{
syntax on
filetype plugin indent on
filetype plugin on
set omnifunc=syntaxcomplete#Complete
" }}}
" FileTypes {{{
" ============
"
" Filetype specific omnifuncs
augroup omnifuncs
    autocmd FileType css,scss setlocal omnifunc=csscomplete#CompleteCSS
    autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
    autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
    autocmd FileType elm setlocal omnifunc=elm#Complete
augroup END

"
" Some Markdown settings
autocmd BufNewFile,BufRead *.md set filetype=markdown
autocmd FileType markdown setlocal linebreak wrap

" Set 2 spaces for HTML
autocmd FileType html setlocal shiftwidth=2 tabstop=2
autocmd FileType handlebars setlocal shiftwidth=2 tabstop=2
autocmd FileType unite setlocal shiftwidth=2 tabstop=2
" }}}
" Line numbers {{{
" ============
"
" Enable
set number

" Display absolute numbers only for current line
autocmd InsertEnter * silent! :set number
autocmd InsertLeave,BufNewFile,VimEnter * silent! :set relativenumber
" }}}
" Leader key {{{
" ==========
"
" Has to be defined before all mappings
" Set leader key as a Space key
let mapleader = " "
" }}}
" Mappings {{{
" ========

" Swap go to first non-blank char with go to line beginning
nnoremap ^ 0
nnoremap 0 ^
vnoremap ^ 0
vnoremap 0 ^

" Navigation by logical lines
nnoremap j gj
nnoremap gj j
nnoremap k gk
nnoremap gk k
vnoremap j gj
vnoremap gj j
vnoremap k gk
vnoremap gk k

" Highlight and stay
nnoremap * *N

" Emmet.vim
let g:user_emmet_leader_key='<C-q>'

" Emmet extends jsx with js
let g:user_emmet_settings = {
\  'javascript' : {
\      'extends' : 'jsx',
\  },
\}

" Toggle distraction modee
let g:limelight_default_coefficient = 0.8
let g:goyo_width = 84 " To include line numbers
nnoremap <silent> <leader>] :Goyo<CR>

" Highlight last inserted text
nnoremap gV `[v`]

" Spell checker toggle
nnoremap <silent> <F9> :set spell!<CR>
inoremap <silent> <F9> <Esc>:set spell!<CR>gi

" Switch between windows
nnoremap <silent> <C-K> <C-W>k
nnoremap <silent> <C-J> <C-W>j
nnoremap <silent> <C-H> <C-W>h
nnoremap <silent> <C-L> <C-W>l

" Open a file under cursor in a vertical split
nnoremap <C-w>V <C-w>vgf

" Rename current word
nnoremap <leader>d yiw:%s:<C-R>":

" Search for current word in cwd
nnoremap <silent> <F11> yiw:Ag <C-R>"<CR>

" Toggle paste mode
set pastetoggle=<F12>

" Shortcut to enter visual block substitution
vnoremap <leader>d y<Esc>:%s:<C-R>":

" Append to end of file
nnoremap <leader>a Go

" Open a presentation with Goyo
nnoremap <silent> <leader>p :PresentingStart<CR>:Goyo<CR>

" Increment selected numbers
function! Incr()
    let a = line('.') - line("'<")
    let c = virtcol("'<")
    if a > 0
        execute 'normal! '.c.'|'.a."\<C-a>"
    endif
    normal `<
endfunction
vnoremap <C-a> :call Incr()<CR>

" Duplicate current selection
nnoremap <C-D> yyP
inoremap <C-D> <Esc>yyPi
vnoremap <C-D> y/\V<C-R>"<CR>

" Indentation without losing selection
vnoremap > >gv
vnoremap < <gv

" Insert one character
nnoremap <silent> <leader>i i<Space><Esc>r

" Remove search highlighting
nnoremap <silent> <C-n> :noh<CR>

" Create Blank Newlines and stay in Normal mode
nnoremap <silent> <leader>j o<Esc>
nnoremap <silent> <leader>k O<Esc>

" Break current line with <CR> and stay in normal mode
nnoremap <F3> i<CR><Esc><F5>

nnoremap <leader>x Y:s:\[ \]:[x]:<CR>Go[updated at: <Esc>:pu=strftime('%c')<CR>kJA]<Esc>:nohl<CR>po<Esc><C-o>

" Search mappings:
" These will make it so that going to the next one in a search
" will center on the line it's found in.
nnoremap n nzz
nnoremap N Nzz

" Swap ; and :
" Convenient.
nnoremap ; :
nnoremap : ;
vnoremap ; :
vnoremap : ;

" Remove trailing spaces on F5
nnoremap <silent> <F5> :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar>:nohl<CR>

" Copy whole file to clipboard
nnoremap <F4> <Esc>ggVG"*y

" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

" }}}
" Elm Vim {{{
" ==========
"
" Autoformat on save
let g:elm_format_autosave = 1
let g:elm_setup_keybindings = 0
let g:elm_syntastic_show_warnings = 1
let g:elm_detailed_complete = 1
au BufWritePost *.elm ElmMake
" }}}
" Unite.vim {{{
" =========
"
let g:unite_source_history_yank_enable = 1
call unite#filters#matcher_default#use(['matcher_fuzzy'])
let g:unite_source_rec_async_command = ['ag', '--follow', '--nogroup', '--nocolor', '--hidden', '-g', '']
" ignore node_modules and bower_components
call unite#custom#source('file_rec', 'ignore_pattern', 'node_modules/\|bower_components/\|dist/\|elm-stuff/')
" Custom mappings for the unite buffer
autocmd FileType unite call s:unite_settings()
call unite#custom#profile('default', 'context', {
\   'winheight': 20,
\   'direction': 'dynamicbottom'
\ })

" Initialize Unite's global list of menus
if !exists('g:unite_source_menu_menus')
    let g:unite_source_menu_menus = {}
endif

" Create an entry for our new menu of commands
let g:unite_source_menu_menus.bookmarks = {
\    'description': 'Quick bookmarks and actions'
\ }

" Define our list of [Label, Command] pairs
let g:unite_source_menu_menus.bookmarks.command_candidates = [
\   ['init.vim                           [file]', 'e $HOME/.config/nvim/init.vim'],
\   ['js.snippets                        [file]', 'e $HOME/.config/nvim/snippets/javascript/js.snippets'],
\   ['----------------- Files -----------------', ''],
\   ['Install plugins                 [command]', 'PlugInstall'],
\   ['Find in sources...              [command]', 'exe "Ag " input("pattern: ")'],
\   ['Remove and close current file   [command]', 'call delete(expand("%")) | bdelete!'],
\   ['New file here...                [command]', 'exe "e %:p:h/" . input("pattern: ")'],
\   ['New file here (split)...        [command]', 'exe "split %:p:h/" .  input("pattern: ")'],
\   ['---------------- Commands ---------------', ''],
\ ]

function! s:unite_settings()
    " Enable navigation with control-j and control-k in insert mode
    imap <buffer> <C-j> <Plug>(unite_select_next_line)
    imap <buffer> <C-k> <Plug>(unite_select_previous_line)
endfunction

" Unite key mappings
nnoremap <leader>f :<C-u>Unite -auto-resize -start-insert file_mru<cr>
nnoremap <leader>r :<C-u>Unite -auto-resize -buffer-name=files -start-insert file_rec/async:!<cr>
nnoremap <leader>e :<C-u>Unite -start-insert -buffer-name=buffer buffer<cr>
nnoremap <leader>b :<C-u>Unite -start-insert menu:bookmarks <cr>
nnoremap <leader>g :<C-u>Unite -auto-resize -buffer-name=files -start-insert file_rec:<C-r>=substitute(expand('%:h'), "\\", "\/", "g")<cr><cr>
" }}}
" Colors and font settings {{{
set t_Co=256
set background=dark
colorscheme solarized
" }}}
" Folding {{{
" =======
"
set foldenable
set foldlevelstart=10
set foldmethod=indent

" }}}
" Vim-go {{{
" =========
"
let g:go_disable_autoinstall = 0
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_structs = 1
let g:go_highlight_operators = 1
let g:go_highlight_build_constraints = 1
" }}}
" Syntastic {{{
" =========

let g:syntastic_check_on_wq = 0
let g:syntastic_check_on_open = 1
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_javascript_checkers = ['eslint']

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_loc_list_height = 5
let g:syntastic_auto_loc_list = 0
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 1
let g:syntastic_javascript_checkers = ['eslint']
let g:syntastic_javascript_eslint_exec = 'eslint_d'

let g:syntastic_error_symbol = '✖'
let g:syntastic_style_error_symbol = '☠'
let g:syntastic_warning_symbol = '☸'
let g:syntastic_style_warning_symbol = '💩'

highlight link SyntasticErrorSign SignColumn
highlight link SyntasticWarningSign SignColumn
highlight link SyntasticStyleErrorSign SignColumn
highlight link SyntasticStyleWarningSign SignColumn
" }}}
" Vim-airline {{{
set laststatus=2
let g:airline#extensions#tabline#enabled = 0
let g:airline_theme="tomorrow"
set encoding=utf-8

"
let g:airline_symbols = {}
let g:airline_left_sep = ""
let g:airline_left_alt_sep = ""
let g:airline_right_sep = ""
let g:airline_right_alt_sep = ""
let g:airline_symbols.space = " "
let g:airline_symbols.branch = ""
let g:airline_symbols.readonly = ""
let g:airline_symbols.linenr = "■"
let g:airline_symbols.maxlinenr = "■"
" let g:airline_left_sep = ""
" let g:airline_left_alt_sep = ""
" let g:airline_right_sep = ""
" let g:airline_right_alt_sep = ""
" let g:airline_symbols.branch = ""
" let g:airline_symbols.readonly = ""
" let g:airline_symbols.linenr = " "
" let g:airline_symbols = {}
" let g:airline_left_sep = "▓▒"
" let g:airline_left_alt_sep = "▒░"
" let g:airline_right_sep = "▒▓"
" let g:airline_right_alt_sep = "░▒"
" let g:airline_symbols.branch = "br"
" let g:airline_symbols.readonly = "ro"
" let g:airline_symbols.linenr = "ln"

" }}}
" Misc {{{
" ====

" Set up initial size
" set lines=50 columns=100

" Speed up syntax highlighting {
set nocursorcolumn
set nocursorline
syntax sync minlines=100
syntax sync maxlines=240
" Don't try to highlight lines longer than 300 characters,
" in order to speed up the viewport.
set synmaxcol=300
" }

" Color column
set colorcolumn=80

" Disable swap files from being created
set noswapfile

" Set new directory for backup files
set nobackup

" Remove widget stuff from window in gVim
set guioptions-=m
set guioptions-=T
set guioptions-=r
set guioptions-=L

" Use modelines
set modelines=1

" Automatic reloading of .vimrc
autocmd! bufwritepost init.vim source %

" Start scrolling 7 lines before edge
set so=5

" Horizontal split always to the bottom
set splitbelow

" Puts vertical windows to right, instead of left.
set splitright

" Tabs are 4 chars wide
set tabstop=4

" Indentation size is 4 characters
set shiftwidth=4

" Make backspace work like most other apps
set backspace=2

" Dont add additional space on S-j
set nojoinspaces

" Convert tabs to spaces
set expandtab
set smarttab

" Be single lined
set nowrap

" Necesary for lots of cool vim things
set nocompatible

" This shows what you are typing as a command
set showcmd

" Who doesn't like autoindent?
set autoindent

" Cool tab completion stuff
set wildmenu
set wildmode=list:longest,full

" Ignoring case is a fun trick
set ignorecase

" And so is Artificial Intellegence!
set smartcase

" Incremental searching is sexy
set incsearch

" Highlight things that we find with the search
set hlsearch

" Don't redraw when not needed
set lazyredraw

" Highlighting trail spaces
highlight ExtraWhitespace ctermfg=red guifg=red
" ctermfg=white guifg=white
match ExtraWhitespace /\s\+$/

" TODO test
highlight MyTodo ctermbg=white ctermfg=red guibg=white guifg=red
match MyTodo /TODO.*/

" Displaying spaces as dots and EOL as ¬
set listchars=tab:-\ ,trail:♥
set list!

" Autocompletion stuff...
set complete=.,w,b,u,U,t,i,d,k
set complete+=k
" set dictionary=./words/english,./words/russian

" Turn off bell
autocmd GUIEnter * set noerrorbells visualbell t_vb=

" }}}
" Vimwiki {{{
" =========
let g:vimwiki_ext2syntax = {'.md': 'markdown', '.mkd': 'markdown', '.wiki': 'media'}
let g:vimwiki_list = [{
    \ 'path': '~/vimwiki',
    \ 'path_html': '~/.vimwiki_html',
    \ 'auto_tags': 1
    \ }]
" }}}

" vim:foldmethod=marker:foldlevel=0

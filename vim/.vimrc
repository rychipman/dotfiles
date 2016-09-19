autocmd! bufwritepost .vimrc source %   " automatically reload .vimrc

set encoding=utf8           " Clean utf-8 encoding
set termencoding=utf-8

set history=1000            " Save a long command and undo history
set undolevels=1000
set undodir=~/.vim/undo     " Save undo history across sessions

set showcmd                 " show partially completed commands

set mouse=                 " allow interactions via mouse

set gcr=a:blinkon0          " disable cursor blink
set autoread                " load changes in files that happen outside vim
set splitright              " open vertical splits to right
set splitbelow              " opens horizontal split below
set shortmess=filnrxtToOI    " see :help shortmess

" save up to 100 marks, enable capital marks
set viminfo='100,f1,\"100,:20,n~/.vim/viminfo

set number                  " turn on line numbers
set hidden                  " for better buffer mangement

""" search stuff
set incsearch               " incremental search
set hlsearch                " highlight search terms
set smartcase
set ignorecase

""" turn off swap files
set noswapfile
set nobackup
set writebackup " same as default

""" allow backspace to work as expected
set backspace=2

""" indentation and display
set autoindent
set smartindent
set smarttab
set shiftwidth=4
set softtabstop=4
set tabstop=4
set expandtab
set wrap

""" shell
set shell=bash

""" Folds
set nofoldenable            " don't fold by default

""" Completion
set wildmode=longest,list,full
set wildmenu                    " enable c-n and c-p scrolling through matches
set wildignore=*.o,*.obj.pyc,*~ " ignore some things (compiled & backup files) when matching
set wildignore+=*vim/backups*

""" Scrolling
set scrolloff=10            " start scrolling 10 rows before margin
set sidescrolloff=15
set sidescroll=1

" add ruler to status bar
set ruler

noremap <silent> <C-u> :call smooth_scroll#up(&scroll, 11, 2)<CR>
noremap <silent> <C-d> :call smooth_scroll#down(&scroll, 11, 2)<CR>

" leaders

let mapleader="\<Space>"
let localleader=','

" sorting
vnoremap <leader>o :sort<CR>

" better indentation behavior (stays highlighted)
vnoremap < <gv
vnoremap > >gv

" remove highlights from last search
map <leader>h :nohl<CR>

" window splits
map <leader>v <C-w>v
map <leader>s <C-w>s

" navigation between tabs
map <leader>t <Esc>:tabnew<CR>
map <leader>n <Esc>:tabprevious<CR>
map <leader>m <Esc>:tabnext<CR>

" save a file that requires sudo escalation
map <leader>w :w ! sudo tee % >/dev/null <CR>

""" restore cursor location
function! RestoreCursorLocation()
    if line("'\"") <= line("$")
        normal! g`"
        return 1
    endif
endfunction

augroup restoreCursor
    autocmd!
    autocmd BufWinEnter * call RestoreCursorLocation()
augroup END

""" plugins
call plug#begin('~/.vim/autoload/vim-plug') " initialize vim-plug

""" declare plugins here """
Plug 'wombat256.vim'      " go-to color scheme
Plug 'tpope/vim-surround' " surrounding semantic units
Plug 'wellle/targets.vim' " text objects
Plug 'jayflo/vim-skip'   " move by fractional line-length units
Plug 'airblade/vim-gitgutter' " add git diff info in gutter

""" Language-specific plugins
Plug 'fatih/vim-go'
Plug 'mattn/emmet-vim'                " html expansion with emmet
Plug 'lervag/vimtex'
Plug 'wavded/vim-stylus'
Plug 'AndrewRadev/vim-eco'
Plug 'kchmck/vim-coffee-script'
Plug 'rust-lang/rust.vim'

""" Improvements to vim functionality
Plug 'terryma/vim-smooth-scroll'      " smooth scrolling for page jumps
Plug 'jiangmiao/auto-pairs'           " closing brackets, parens, etc.
Plug 'junegunn/vim-oblique' " search improvements
Plug 'junegunn/vim-pseudocl' " required for vim-oblique 
Plug 'richsoni/vim-ecliptic' " clean system clipboard integration
Plug 'rking/ag.vim'
Plug 'kien/ctrlp.vim'
Plug 'Shougo/neocomplete.vim'
Plug 'unicode.vim'
"Plug 'tpope/vim-sleuth' " autodetecting tab sizes

call plug#end()

"##########################################
"######## AUTOCOMPLETE ####################
"##########################################

" Disable AutoComplPop
let g:acp_enableAtStartup = 0
" Use neocomplete.
let g:neocomplete#enable_at_startup = 1
" Use smartcase.
let g:neocomplete#enable_smart_case = 1
" Set minimum syntax keyword length.
let g:neocomplete#sources#syntax#min_keyword_length = 3
let g:neocomplete#lock_buffer_name_pattern = '\*ku\*'

" Define dictionary.
let g:neocomplete#sources#dictionary#dictionaries = {
    \ 'default' : ''
    \ }

" Define keyword.
if !exists('g:neocomplete#keyword_patterns')
    let g:neocomplete#keyword_patterns = {}
endif

let g:neocomplete#keyword_patterns['default'] = '\h\w*'

" Plugin key-mappings.
inoremap <expr><C-g>     neocomplete#undo_completion()
inoremap <expr><C-l>     neocomplete#complete_common_string()

" Recommended key-mappings.
" <CR>: close popup and save indent.
inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
function! s:my_cr_function()
  return neocomplete#close_popup() . "\<CR>"
  " For no inserting <CR> key.
  "return pumvisible() ? neocomplete#close_popup() : "\<CR>"
endfunction

" <TAB>: completion.
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
" <C-h>, <BS>: close popup and delete backword char.
inoremap <expr><C-h> neocomplete#smart_close_popup()."\<C-h>"
inoremap <expr><BS> neocomplete#smart_close_popup()."\<C-h>"
inoremap <expr><C-y>  neocomplete#close_popup()
inoremap <expr><C-e>  neocomplete#cancel_popup()

" Enable omni completion.
autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags

"##########################################
"######## EMMET OPTS ######################
"##########################################

let g:user_emmet_leader_key='<C-e>'
let g:user_emmet_settings = {
\  'html' : {
\    'attr_quotes' : 'single',
\    'tag_nl_leaf' : 'true',
\    'indent'      : 'true',
\    'tag_nl'      : 'true',
\    'indentation' : '    '
\  },
\}

"##########################################
"######## AUTO_PAIRS OPTS #################
"##########################################

let g:AutoPairsFlyMode = 1

"##########################################
"######## VIM-SKIP OPTIONS ################
"##########################################

let g:vimskip_mode = "normal"
let g:vimskip_wraptocenter = 1
let g:vimskip_ignore_initial_ws = 1
let g:vimskip_ignore_trailing_ws = 1

"##########################################
"######## VIMTEX OPTIONS ##################
"##########################################

let g:vimtex_view_general_viewer = 'zathura'

"##########################################
"######## RUST.VIM OPTIONS ################
"##########################################

let g:rustfmt_autosave = 0

"##########################################
"######## EYE CANDY #######################
"##########################################

""" syntax highlighting
syntax on
syntax enable

""" highlight ejs as html
au BufNewFile,BufRead *.ejs set filetype=html

""" highlight bad whitespace
autocmd ColorScheme * highlight ExtraWhitespace ctermbg=red
au InsertLeave * match ExtraWhitespace /\s\+$/

""" colors
set t_Co=256                    " ensure terminal works with 256 colors
let base16colorspace=256

""" color scheme
colorscheme wombat256mod        " set color scheme. good w/terminal
highlight Normal ctermbg=233

nohl

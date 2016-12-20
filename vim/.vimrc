autocmd!

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

set list listchars=tab:»·

noremap <silent> <C-u> :call smooth_scroll#up(&scroll, 11, 2)<CR>
noremap <silent> <C-d> :call smooth_scroll#down(&scroll, 11, 2)<CR>

" leaders

let mapleader="\<Space>"
let localleader=','

" reload .vimrc
map <leader>r <Esc>:source ~/.vimrc<CR>

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

Plug 'wombat256.vim'      " go-to color scheme
Plug 'tpope/vim-surround' " surrounding semantic units
Plug 'wellle/targets.vim' " text objects
Plug 'airblade/vim-gitgutter' " add git diff info in gutter
    let g:gitgutter_map_keys = 0
Plug 'tpope/vim-fugitive'
    map <leader>g :Gstatus<CR>
Plug 'fatih/vim-go'
Plug 'wavded/vim-stylus'
Plug 'mattn/emmet-vim'                " html expansion with emmet
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
Plug 'terryma/vim-smooth-scroll'      " smooth scrolling for page jumps
Plug 'jiangmiao/auto-pairs'           " closing brackets, parens, etc.
    let g:AutoPairsFlyMode = 1
    let g:AutoPairsShortcutBackInsert = '<C-i>'
Plug 'junegunn/vim-oblique' " search improvements
Plug 'junegunn/vim-pseudocl' " required for vim-oblique
Plug 'richsoni/vim-ecliptic' " clean system clipboard integration

call plug#end()

""" syntax highlighting
syntax on
syntax enable

""" highlight ejs as html
au BufNewFile,BufRead *.ejs set filetype=html

""" colors
set t_Co=256                    " ensure terminal works with 256 colors
let base16colorspace=256

""" color scheme
colorscheme wombat256mod
highlight Normal ctermbg=233
highlight SpecialKey ctermbg=233 ctermfg=235
highlight ExtraWhitespace ctermbg=red

""" highlight trailing whitespace
autocmd InsertLeave * match ExtraWhitespace /\s\+$/

nohl

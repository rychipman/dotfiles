
autocmd! bufwritepost .config/nvim/init.vim source %   " automatically reload .vimrc

set showcmd

set guicursor=a:blinkon0

set mouse=a " same as default

set autoread " same as default

set splitright
set splitbelow

set shortmess=filmnrxtToOI    " see :help shortmess

set number
set norelativenumber " same as default

set nohidden " same as default" deactivated because I want the more explicit behavior

""" search options """
set incsearch " same as default
set hlsearch " same as default
set ignorecase
set smartcase

""" swap/backup options """
set noswapfile
set nobackup	" same as default
set writebackup " same as default

""" set indentation options """
set autoindent " same as default
set smartindent
set tabstop=4
set softtabstop=0 " same as default TODO: check if this is correct
set expandtab
set smarttab " same as default
set wrap " same as default TODO: look into more advanced wrapping

""" shell """
set shell=/bin/bash " same as default

""" folding """
set nofoldenable " same as default

""" scrolling """
set scrolloff=10
set sidescrolloff=0 " same as default
set sidescroll=1

call plug#begin('~/.config/nvim/autoload/vim-plug')

""" leader key mappings """
let mapleader="\<Space>"
let localleader=','

" sorting
vnoremap <leader>o :sort<CR>

" better indentation behavior (stays highlighted)
vnoremap < <gv
vnoremap > >gv

" quick quit, with or without saving
map <leader>q :wa<CR>:qa<CR>
map <leader>Q :qa!<CR>

" remove highlights from last search
map <leader>h :nohl<CR>

" bind CTRL + motion keys to navigate between windows
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-h> <C-w>h
map <C-l> <C-w>l

" window splits
map <leader>v <C-w>v
map <leader>s <C-w>s

" navigation between tabs
map <leader>t <Esc>:tabnew<CR>
map <leader>n <Esc>:tabprevious<CR>
map <leader>m <Esc>:tabnext<CR>

""" declare plugins here """
Plug 'wombat256.vim'      " go-to color scheme
Plug 'tpope/vim-surround' " surrounding semantic units
Plug 'wellle/targets.vim' " text objects
Plug 'jayflo/vim-skip'   " move by fractional line-length units

""" Language-specific plugins
Plug 'fatih/vim-go'
Plug 'mattn/emmet-vim'                " html expansion with emmet

""" Improvements to vim functionality
Plug 'terryma/vim-smooth-scroll'      " smooth scrolling for page jumps
Plug 'jiangmiao/auto-pairs'           " closing brackets, parens, etc.
Plug 'junegunn/vim-oblique' " search improvements
Plug 'junegunn/vim-pseudocl' " required for vim-oblique 
Plug 'richsoni/vim-ecliptic' " clean system clipboard integration
Plug 'rking/ag.vim'
Plug 'kien/ctrlp.vim'
"Plug 'tpope/vim-sleuth' " autodetecting tab sizes

call plug#end()

""" vim-smooth-scroll """
noremap <silent> <c-u> :call smooth_scroll#up(&scroll, 8, 2)<CR>
noremap <silent> <c-d> :call smooth_scroll#down(&scroll, 8, 2)<CR>

""" theming """
syntax enable
      
""" highlight bad whitespace
"autocmd ColorScheme * highlight ExtraWhitespace ctermbg=red
"au InsertLeave * match ExtraWhitespace /\s\+$/


colorscheme wombat256mod

nohl

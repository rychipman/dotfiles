
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

""" declare plugins here """
Plug 'wombat256.vim'

call plug#end()

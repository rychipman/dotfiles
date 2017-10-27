" clear any existing top-level autocmds
autocmd!

" default options ---------------------------------------------------------- {{{
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
set relativenumber          " use relative line numbers
set hidden                  " for better buffer mangement

set completeopt=            ""

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
" ---------------------------------------------------------- default options }}}

" status line -------------------------------------------------------------- {{{
" always have the status line on
set laststatus=2

" define the statusline
set statusline=%f\   " filename and a space
set statusline+=%m   " modified indicator
set statusline+=%r   " read-only indicator
set statusline+=%h   " help-buffer indicator
set statusline+=%w   " preview indicator
set statusline+=%=   " switch to right side
set statusline+=%c/
set statusline+=%{strwidth(getline('.'))}
set statusline+=\ \ \  " separation)
set statusline+=%y   " filetype indicator
set statusline+=\    " right side padding
" -------------------------------------------------------------- status line }}}

" filetype settings -------------------------------------------------------- {{{
augroup filetype_vim
    autocmd!
    autocmd FileType vim setlocal foldenable
    autocmd FileType vim setlocal foldmethod=marker
augroup END

augroup filetype_help
    autocmd!
    autocmd FileType help nnoremap <buffer> q :q<cr>
augroup END
" -------------------------------------------------------- filetype settings }}}

" mappings ----------------------------------------------------------------- {{{

" quicker jumps to line ends
noremap H ^
noremap L g_

" better indentation behavior (stays highlighted)
vnoremap < <gv
vnoremap > >gv

" leaders
let mapleader="\<space>"
let maplocalleader="\\"

" reload or edit .vimrc
nnoremap <leader>rr :source $MYVIMRC<CR>
nnoremap <leader>re :e $MYVIMRC<CR>

" ----------------------------------------------------------------- mappings }}}

" plugins ------------------------------------------------------------------ {{{
call plug#begin('~/.vim/autoload/vim-plug') " initialize vim-plug

Plug 'wombat256.vim'      " go-to color scheme
Plug 'tpope/vim-surround' " surrounding semantic units
Plug 'wellle/targets.vim' " text objects
Plug 'wavded/vim-stylus'
Plug 'junegunn/vim-pseudocl' " required for vim-oblique
Plug 'richsoni/vim-ecliptic' " clean system clipboard integration
Plug 'junegunn/vim-oblique' " search improvements

Plug 'airblade/vim-gitgutter' " add git diff info in gutter
    let g:gitgutter_map_keys = 1

Plug 'fatih/vim-go'
    let g:go_fmt_command = "goimports"

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
    noremap <silent> <C-u> :call smooth_scroll#up(&scroll, 11, 2)<CR>
    noremap <silent> <C-d> :call smooth_scroll#down(&scroll, 11, 2)<CR>

Plug 'jiangmiao/auto-pairs'           " closing brackets, parens, etc.
    let g:AutoPairsShortcutBackInsert = '<C-u>'
    let g:AutoPairsFlyMode = 1

Plug 'lifepillar/vim-mucomplete'
    set shortmess-=c " don't display completion-related messages
    set completeopt=menuone,noselect
    inoremap <expr> <c-e> mucomplete#popup_exit("\<c-e>")
    inoremap <expr> <c-y> mucomplete#popup_exit("\<c-y>")
    inoremap <expr>  <cr> mucomplete#popup_exit("\<cr>")
    let g:mucomplete#enable_auto_at_startup = 1
    let g:mucomplete#chains = {
    \   'default': ['path', 'omni', 'keyn', 'dict', 'uspl'],
    \   'vim': ['path', 'cmd', 'keyn'],
    \}

Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
    nnoremap <leader>e :Files<CR>
    nnoremap <leader>b :Buffers<CR>
    nnoremap <leader>c :Commits<CR>
    nnoremap <leader>f :Ag 
    nnoremap <leader>h :History:<CR>
    let g:fzf_colors = {
    \   'fg':      ['fg', 'Normal'],
    \   'bg':      ['bg', 'Normal'],
    \   'hl':      ['fg', 'Comment'],
    \   'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
    \   'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
    \   'hl+':     ['fg', 'Statement'],
    \   'info':    ['fg', 'PreProc'],
    \   'prompt':  ['fg', 'Conditional'],
    \   'pointer': ['fg', 'Exception'],
    \   'marker':  ['fg', 'Keyword'],
    \   'spinner': ['fg', 'Label'],
    \   'header':  ['fg', 'Comment']
    \}

Plug 'tpope/vim-fugitive'
    nnoremap <leader>g :Gstatus<CR>

Plug 'romainl/vim-qf'
    nmap ]q <Plug>QfCnext
    nmap [q <Plug>QfCprevious
    nmap ]l <Plug>QfLnext
    nmap [l <Plug>QfLprevious

call plug#end()
" ------------------------------------------------------------------ plugins }}}

" colors and theming ------------------------------------------------------- {{{

" syntax highlighting
syntax on
syntax enable

" colors
set t_Co=256                    " ensure terminal works with 256 colors
let base16colorspace=256

" color scheme
colorscheme wombat256mod
highlight Normal ctermbg=233
highlight SpecialKey ctermbg=233 ctermfg=235
highlight ExtraWhitespace ctermbg=red

highlight DiffAdd ctermbg=22
highlight DiffDelete ctermbg=52
highlight DiffChange ctermbg=234
highlight DiffText ctermbg=23

" highlight trailing whitespace
augroup trailingWhitespace
    autocmd!
    autocmd InsertLeave * match ExtraWhitespace /\s\+$/
augroup END

" ------------------------------------------------------- colors and theming }}}

" misc --------------------------------------------------------------------- {{{

" restores cursor to last location in current file
function! RestoreCursorLocation()
    if line("'\"") <= line("$")
        normal! g`"
        return 1
    endif
endfunction

" restore cursor location when opening buffer
augroup restoreCursor
    autocmd!
    autocmd BufWinEnter * call RestoreCursorLocation()
augroup END

" --------------------------------------------------------------------- misc }}}

" turn of highlights from a pre-existing search
nohl

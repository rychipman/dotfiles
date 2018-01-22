" be explicit about encoding
set encoding=utf8
scriptencoding utf8

" clear any existing top-level autocmds
autocmd!

" default options ---------------------------------------------------------- {{{
set termencoding=utf-8

set history=1000            " Save a long command and undo history
set undolevels=1000
set undodir=~/.vim/undo     " Save undo history across sessions

set showcmd                 " show partially completed commands

set mouse=                  " allow interactions via mouse

set guicursor=a:blinkon0    " disable cursor blink
set autoread                " load changes in files that happen outside vim
set splitright              " open vertical splits to right
set splitbelow              " opens horizontal split below
set shortmess=filnrxtToOI   " see :help shortmess

" save up to 100 marks, enable capital marks
if has('nvim')
    set viminfo='100,f1,\"100,:20,n~/.config/nvim/shada
else
    set viminfo='100,f1,\"100,:20,n~/.vim/viminfo
endif

if has('nvim')
    set inccommand=nosplit
endif

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
let g:mapleader="\<space>"
let g:maplocalleader="\\"

" reload or edit .vimrc
nnoremap <leader>rr :source $MYVIMRC<CR>
nnoremap <leader>re :e ~/.vimrc<CR>

" ----------------------------------------------------------------- mappings }}}

" terminal options --------------------------------------------------------- {{{
if has('nvim')
    tnoremap <C-a>s <C-\><C-n><C-w>s :terminal<CR> i
    tnoremap <C-a>v <C-\><C-n><C-w>v :terminal<CR> i
    nnoremap <C-a>s <C-w>s :terminal<CR> i
    nnoremap <C-a>v <C-\><C-n><C-w>v :terminal<CR> i

    nnoremap <leader>tt :terminal<CR> i
    nnoremap <leader>ts <C-w>s :terminal<CR> i
    nnoremap <leader>tv <C-w>v :terminal<CR> i

    augroup term_config
        autocmd!
        autocmd TermOpen * setlocal nonumber
        autocmd TermOpen * setlocal norelativenumber
        autocmd TermOpen * nnoremap <buffer> <C-c> i
        autocmd TermOpen * nnoremap <buffer> <CR> i
        autocmd TermOpen term://* tnoremap <ESC> <C-\><C-n>
    augroup END
endif
" -------------------------------------------------------- terminal settings }}}

" plugins ------------------------------------------------------------------ {{{
call plug#begin('~/.vim/autoload/vim-plug') " initialize vim-plug

Plug 'michalbachowski/vim-wombat256mod' " go-to color scheme
Plug 'tpope/vim-surround' " surrounding semantic units
Plug 'wellle/targets.vim' " text objects
Plug 'wavded/vim-stylus'
Plug 'richsoni/vim-ecliptic' " clean system clipboard integration
Plug 'junegunn/vim-slash' " search improvements

Plug 'vimwiki/vimwiki'

Plug 'w0rp/ale'
    let g:ale_echo_cursor = 1
    let g:ale_set_signs = 1
    let g:ale_set_highlights = 1

    let g:ale_sign_error = '●'
    let g:ale_sign_warning = '●'
    let g:ale_sign_info = '●'
    let g:ale_sign_style_error = '●'
    let g:ale_sign_style_warning = '●'

    let g:ale_completion_enabled = 0

    let g:ale_lint_on_enter = 1
    let g:ale_lint_on_filetype_changed = 1
    let g:ale_lint_on_save = 1
    let g:ale_lint_on_insert_leave = 1
    let g:ale_lint_on_text_changed = 'normal'

    let g:go_fmt_fail_silently = 1

Plug 'airblade/vim-gitgutter' " add git diff info in gutter
    let g:gitgutter_map_keys = 1

Plug 'rust-lang/rust.vim'
Plug 'racer-rust/vim-racer'

Plug 'fatih/vim-go'
    let g:go_fmt_command = 'goimports'

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

if has('nvim')
    Plug 'roxma/nvim-completion-manager'
        set completeopt=menuone,noselect
        inoremap <expr> <CR> (pumvisible() ? "\<c-y>\<cr>" : "\<CR>")
        inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
        inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
else
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
endif

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
let g:base16colorspace=256

" color scheme
colorscheme wombat256mod
highlight Normal ctermbg=233
highlight SpecialKey ctermbg=233 ctermfg=235
highlight ExtraWhitespace ctermbg=red

highlight mDiffInvisible ctermbg=234 ctermfg=234

highlight DiffAdd ctermbg=234 ctermfg=234
highlight DiffDelete ctermbg=234 ctermfg=234
highlight DiffChange ctermbg=234 ctermfg=234

"highlight DiffText ctermbg=23

highlight mDiffNormal ctermbg=234
highlight mDiffAdd ctermbg=green
highlight mDiffDelete ctermbg=red
highlight mDiffChangeDeleteLine ctermbg=red
highlight mDiffChangeAddLine ctermbg=green
highlight mDiffChangeDeleteText ctermbg=blue
highlight mDiffChangeAddText ctermbg=blue

nnoremap <leader>de :call SynStack()<CR>
function! SynStack()
    let l:state = synIDattr((diff_hlID('.',col('.'))), 'name')
    echo l:state
endfunction

nnoremap <leader>do :call RecolorDiffBuffer('old')<CR>
nnoremap <leader>dn :call RecolorDiffBuffer('new')<CR>
nnoremap <leader>dw :call RecolorDiffBuffer('working')<CR>
nnoremap <leader>di :call RecolorDiffBuffer('index')<CR>
function! RecolorDiffBuffer(strategy)

    let l:diffHighlights = {
\       'old': {
\           'DiffAdd': 'mDiffDelete',
\           'DiffDelete': 'mDiffInvisible',
\           'DiffChange': 'mDiffChangeDeleteLine',
\           'DiffText': 'mDiffChangeDeleteText',
\       },
\       'new': {
\           'DiffAdd': 'mDiffAdd',
\           'DiffDelete': 'mDiffInvisible',
\           'DiffChange': 'mDiffChangeAddLine',
\           'DiffText': 'mDiffChangeAddText',
\       },
\       'working': {
\           'DiffAdd': 'mDiffAdd',
\           'DiffDelete': 'mDiffDelete',
\       },
\       'index': {
\           'DiffAdd': 'mDiffNormal',
\           'DiffDelete': 'mDiffNormal',
\       },
\   }

    let l:num_lines = line('$')
    let l:line_num = 0
    while l:line_num < l:num_lines
        let l:line_state = CellState(l:line_num, 1)
        if l:line_state ==# 'DiffAdd' || l:line_state ==# 'DiffDelete'
            call matchaddpos(l:diffHighlights[a:strategy][l:line_state], [l:line_num], 100)
        elseif l:line_state ==# 'DiffChange' || l:line_state ==# 'DiffText'
            let l:num_cols = strlen(getline('.'))
            let l:col_num = 0
            while l:col_num < l:num_cols
                let l:char_state = CellState(l:line_num, l:col_num)
                call matchaddpos(l:diffHighlights[a:strategy][l:char_state], [l:line_num,l:col_num], 100)
                let l:col_num += 1
            endwhile
        endif
        let l:line_num += 1
    endwhile

endfunction

function! CellState(row, col)
    return synIDattr((diff_hlID(a:row,a:col)), 'name')
endfunction

" highlight trailing whitespace
augroup trailingWhitespace
    autocmd!
    autocmd InsertLeave * match ExtraWhitespace /\s\+$/
augroup END

" sign-column highlighting for ale
highlight clear SignColumn
highlight SignColumn ctermbg=232

highlight ALEErrorSign ctermbg=232 ctermfg=196
highlight ALEError ctermbg=196 ctermfg=230

highlight ALEWarningSign ctermbg=232 ctermfg=226
highlight ALEWarning ctermbg=226 ctermfg=0

highlight ALEInfoSign ctermbg=232 ctermfg=33
highlight ALEInfo ctermbg=33 ctermfg=230

" ------------------------------------------------------- colors and theming }}}

" misc --------------------------------------------------------------------- {{{

" restores cursor to last location in current file
function! RestoreCursorLocation()
    if line("'\"") <= line('$')
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

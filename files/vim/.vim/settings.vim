"source system defaults
"----------------------{{{
if filereadable(expand('$VIMRUNTIME/defaults.vim'))
    unlet! g:skip_defaults_vim
    source $VIMRUNTIME/defaults.vim
endif
"}}}

"general settings
"----------------{{{
"encoding/format
set encoding=utf-8
set fileformats=unix,dos,mac

"file/buffer
set autoread
set autowrite
set confirm

"display
set background=dark
set number
set wrap
set foldmethod=marker
set listchars=tab:→\ ,extends:›,precedes:‹,nbsp:·,space:·,trail:·,eol:¬
" set list

"drawing
set lazyredraw
set regexpengine=1
set redrawtime=10000

"tab/indent
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab
set autoindent

"status bar
set showcmd
set wildmenu
set ruler
set laststatus=2

"search
set incsearch
set hlsearch
set ignorecase
set smartcase

"cursor behaviour
set virtualedit=block
set backspace=indent,eol,start
set scrolloff=2
set sidescrolloff=4

"input/timeout
set esckeys
set timeoutlen=1000 "for mappings
set ttimeoutlen=20 "for keycodes

"window behaviour (ltr)
set splitbelow
set splitright
"}}}

"os/gui settings
"---------------{{{
if has('win32')
    let &runtimepath.=",$HOME/.vim"
endif

if has("gui_running")
    set guioptions -=m
    set guioptions -=T
    if has("gui_gtk2")
        set guifont=Inconsolata\ 12
    elseif has("gui_macvim")
        set guifont=Menlo\ Regular:h14
    elseif has("gui_win32")
        set guifont=Consolas:h10
        " set guifont=Cascadia\ Code:h10

        "start full-screened
        augroup WINGUI
            autocmd! GUIEnter * simalt ~x
        augroup END
    endif
endif
"}}}

"syntax/filetype/matchit
"-----------------------{{{
syntax on
filetype plugin indent on
runtime macros/matchit.vim
"}}}

"file cleanup
"------------{{{
"swap
let mySwapDir = expand("$HOME/.vim/.swap")
if !isdirectory(mySwapDir)
    silent! call mkdir(mySwapDir, "p")
endif
let &directory=mySwapDir

"undo
if has('persistent_undo')
    let myUndoDir = expand("$HOME/.vim/.undo")
    if !isdirectory(myUndoDir)
        silent! call mkdir(myUndoDir, "p")
    endif
    let &undodir=myUndoDir
    set undofile
endif

"backup
if has('writebackup')
    let myBackupDir = expand("$HOME/.vim/.backup")
    if !isdirectory(myBackupDir)
        silent! call mkdir(myBackupDir, "p")
    endif
    let &backupdir=myBackupDir
    set backup
endif
"}}}


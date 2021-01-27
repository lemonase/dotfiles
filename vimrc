"__   _(_)_ __ ___  _ __ ___
"\ \ / / | '_ ` _ \| '__/ __|
" \ V /| | | | | | | | | (__
"  \_/ |_|_| |_| |_|_|  \___|

"source system defaults
if filereadable(expand('$VIMRUNTIME/defaults.vim'))
    unlet! g:skip_defaults_vim
    source $VIMRUNTIME/defaults.vim
endif

"os settings
"-----------
if has('win32')
    let &runtimepath.=",$HOME/.vim"
endif

"gui settings
"------------
if has("gui_running")
    set guioptions -=m
    set guioptions -=T
    if has("gui_gtk2")
        set guifont=Inconsolata\ 12
    elseif has("gui_macvim")
        set guifont=Menlo\ Regular:h14
    elseif has("gui_win32")
        " set guifont=Consolas:h10
        set guifont=Cascadia\ Code:h10
    endif
endif

"general settings
"----------------
"encoding/format
set encoding=utf-8
set fileformats=unix,dos,mac

"file/buffer
set autoread
set autowrite
set confirm

"display
set background=light
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
set showmatch
set scrolloff=2
set sidescrolloff=4

"input/timeout
set esckeys
set timeoutlen=1000 "for mappings
set ttimeoutlen=20 "for keycodes

"window behaviour (ltr)
set splitbelow
set splitright

"cleanup files created by vim

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

"plugins (vim-plug)
"------------------
"vim-plug paths
let plugDir = expand("$HOME/.vim/autoload/plug.vim")
let pluginDir = expand("$HOME/.vim/plugged")
let plugRemote = "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"

"ensure vim-plug is installed on VimEnter
if empty(glob(plugDir))
    silent execute "!curl -fLo " . shellescape(expand(plugDir)) . " --create-dirs " . shellescape(plugRemote)
    autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin(pluginDir)
    Plug 'junegunn/vim-plug'

    "quality of life
    Plug 'tpope/vim-surround'
    Plug 'tpope/vim-commentary'
    Plug 'tpope/vim-unimpaired'
    Plug 'tpope/vim-rsi'
    Plug 'tpope/vim-eunuch'
    Plug 'tpope/vim-repeat'
    Plug 'editorconfig/editorconfig-vim'
    "browser/url opener
    Plug 'tyru/open-browser.vim'
    "git
    Plug 'tpope/vim-fugitive'
    "visual
    Plug 'flazz/vim-colorschemes'
    "linting
    Plug 'w0rp/ale'

    "javascript/css/html
    Plug 'pangloss/vim-javascript'
    Plug 'mattn/emmet-vim'
    "gdscript
    Plug 'calviken/vim-gdscript3'
    "go
    Plug 'fatih/vim-go'
    "md
    Plug 'godlygeek/tabular'
    Plug 'plasticboy/vim-markdown'
    "toml
    Plug 'cespare/vim-toml'
call plug#end()

"syntax/filetype settings
"---------------
" syntax on
" filetype plugin indent on
" runtime macros/matchit.vim

"plugin settings
"---------------
"netrw
let g:netrw_banner = 0
let g:netrw_winsize = 24
let g:netrw_liststyle = 3
let g:netrw_preview = 1
let g:netrw_alto = 0
let g:netrw_usetab = 1
let g:netrw_browsex_viewer = "xdg-open"
let g:NetrwIsOpen = 0 "for toggle function

"replace netrw gx command
let g:netrw_nogx = 1
nmap gx <Plug>(openbrowser-smart-search)
vmap gx <Plug>(openbrowser-smart-search)

"ale
let g:ale_linters = {
\   'javascript': ['prettier', 'eslint'],
\   'markdown': ['mdl', 'write-good']
\}

let g:ale_fixers = {
\   '*': ['remove_trailing_lines', 'trim_whitespace'],
\   'javascript': ['eslint'],
\   'markdown': ['prettier']
\}

let g:ale_sign_error = '->'
let g:ale_sign_warning = '--'
let g:ale_lint_on_save = 1
let g:ale_fix_on_save = 1

"vim-markdown
let g:vim_markdown_folding_disabled = 1
let g:vim_markdown_no_default_key_mappings = 1
let g:vim_markdown_toc_autofit = 1

" colorschemes
" {{{
" colorscheme 1989
" colorscheme PaperColor
" colorscheme gruvbox
" colorscheme jellybeans
" colorscheme molokai
" colorscheme Benokai
" colorscheme Monokai
" colorscheme badwolf
" colorscheme wombat256
" colorscheme yuejiu
" colorscheme wargrey
" colorscheme Tomorrow-Night-Bright
" colorscheme monokai-phoenix
" }}}

"leader key
let mapleader=' '

"normal/visual mode keybinds
"---------------------------
"paste and search
nnoremap <silent><leader><space> :let @/ = ""<CR>
nnoremap <leader>p :set invpaste<CR>

"toggles
nnoremap <silent><leader>lb :call ToggleLineBreak()<CR>
nnoremap <silent><leader>wr :call ToggleWrap()<CR>
nnoremap <silent><leader>cc :call ToggleColorColumn()<CR>

"dates
nnoremap <leader>ts "=strftime("%F %T%z")<CR>
nnoremap <leader>dt :r !date<CR>

"rc files
nnoremap <leader>rc :vsplit $MYVIMRC<CR>
nnoremap <leader>so :source $MYVIMRC<CR>

"formatting tools
nnoremap =j :%!python -m json.tool<CR>

"plugin keybinds
nnoremap <silent><leader>e :call ToggleNetrw()<CR>
nnoremap <leader>at :ALEToggle<CR>


"insert mode keybinds/abbreviations
"--------------------
inoreabbrev <expr> #!! "#!/usr/bin/env"

"custom commands
"---------------
"common typos and abbreviations
cnoremap w!! w !sudo tee % > /dev/null
command! W w !sudo tee % > /dev/null
command! Trim %s/\s\+$//
command! Q q
command! WQ wq
command! Q1 q!

"general functions
"-----------------
"toggle functions
function! ToggleColorColumn()
    if &cc == ''
        set cc=80
    else
        set cc=
    endif
endfunction

function! ToggleLineBreak()
    if &lbr == ''
        set fo+=t "Autowraps text with textwidth
        set fo-=l "Wraps long lines in --insert-- mode
        set lbr
    else
        set fo-=t
        set fo+=l
        set lbr!
    endif
endfunction

function! ToggleWrap()
    if &wrap == ''
        set wrap
    else
        set nowrap
    endif
endfunction

function! ToggleNetrw() "make netrw toggleable <https://vi.stackexchange.com/questions/10988/toggle-explorer-window>
    if g:NetrwIsOpen
        let i = bufnr("$")
        while (i >= 1)
            if (getbufvar(i, "&filetype") == "netrw")
                silent exe "bwipeout " . i
            endif
            let i-=1
        endwhile
        let g:NetrwIsOpen=0
    else
        let g:NetrwIsOpen=1
        silent Lexplore
    endif
endfunction

"autogroup filetype functions
"------------------
function BashSettings()
    set syntax=sh
endfunction

function PythonSettings()
    xnoremap <leader>r <esc>:'<,'>:w !python3<CR>
endfunction

function GoSettings()
    set noexpandtab
    let g:go_auto_type_info = 1
    let g:go_imports_autosave = 1
endfunction

function HtmlSettings()
    syntax sync fromstart
endfunction

"autogroup group functions
"-------------------------

function SpellSettings()
    setlocal spell
endfunction

function HalftabSettings()
    setlocal ts=2 sts=2 sw=2 expandtab
endfunction

"auto commands
"-------------
if has("autocmd")
    "global
    augroup global
        autocmd!
        "keep equal proportions when windows resized
        autocmd VimResized * wincmd =
        "save cursor position in a file
        autocmd BufReadPost * if line("'\"") > 1 && line("'\"")
                    \ <= line("$") | exe "normal! g'\"" | endif
    augroup END

    augroup filetype_bash
        autocmd! BufNewFile,BufRead *.bash call BashSettings()
    augroup END

    augroup filetype_python
        autocmd! FileType python call PythonSettings()
    augroup END

    augroup filetype_go
        autocmd! FileType go call GoSettings()
    augroup END

    augroup filetype_html
        autocmd! FileType html call HtmlSettings()
    augroup END

    augroup filetype_spell
        autocmd! FileType markdown call SpellSettings()
    augroup END

    augroup filetype_halftab
        autocmd! FileType html,javascript,css,json,yaml,sh call HalftabSettings()
    augroup END
endif

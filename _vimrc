"__   _(_)_ __ ___  _ __ ___
"\ \ / / | '_ ` _ \| '__/ __|
" \ V /| | | | | | | | | (__
"  \_/ |_|_| |_| |_|_|  \___|

"source system defaults
if filereadable(expand('$VIMRUNTIME/defaults.vim'))
    unlet! g:skip_defaults_vim
    source $VIMRUNTIME/defaults.vim
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
    autocmd GUIEnter * simalt ~x
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
let mySwapDir = expand("$HOME/vimfiles/.swap")
if !isdirectory(mySwapDir)
    silent! call mkdir(mySwapDir, "p")
endif
let &directory=mySwapDir

"undo
if has('persistent_undo')
    let myUndoDir = expand("$HOME/vimfiles/.undo")
    if !isdirectory(myUndoDir)
        silent! call mkdir(myUndoDir, "p")
    endif
    let &undodir=myUndoDir
    set undofile
endif

"backup
if has('writebackup')
    let myBackupDir = expand("$HOME/vimfiles/.backup")
    if !isdirectory(myBackupDir)
        silent! call mkdir(myBackupDir, "p")
    endif
    let &backupdir=myBackupDir
    set backup
endif

"plugins (vim-plug)
"------------------
"vim-plug paths
let plugDir = expand("$HOME/vimfiles/autoload/plug.vim")
let pluginDir = expand("$HOME/vimfiles/plugged")
let plugRemote = "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"

"ensure vim-plug is installed on VimEnter
if empty(glob(plugDir))
    silent execute "!curl -fLo " . shellescape(expand(plugDir)) . " --create-dirs " . shellescape(plugRemote)
    autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin(pluginDir)
    Plug 'junegunn/vim-plug'

    " == quality of life ==
    "normal mode keybinds
    Plug 'tpope/vim-commentary'
    Plug 'tpope/vim-surround'
    Plug 'tpope/vim-unimpaired'
    "command mode keybinds
    Plug 'tpope/vim-rsi'
    Plug 'tpope/vim-eunuch'
    Plug 'tpope/vim-repeat'

    " == external tools ==
    "git
    Plug 'tpope/vim-fugitive'
    Plug 'tpope/vim-rhubarb'
    Plug 'tpope/vim-dispatch'
    Plug 'airblade/vim-gitgutter'
    "fzf
    Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
    Plug 'junegunn/fzf.vim'

    " == language support ==
    "go
    Plug 'fatih/vim-go'
    "ruby
    Plug 'vim-ruby/vim-ruby'
    "html
    Plug 'mattn/emmet-vim'
    "markdown
    Plug 'plasticboy/vim-markdown'
    Plug 'godlygeek/tabular'
    "toml
    Plug 'cespare/vim-toml'
    "linting and lsp
    Plug 'w0rp/ale'

    " == misc ==
    Plug 'flazz/vim-colorschemes'
    Plug 'editorconfig/editorconfig-vim'
    Plug 'tyru/open-browser.vim'
call plug#end()

"post plugin settings
"--------------------
"netrw
let g:netrw_banner = 0
let g:netrw_winsize = 24
let g:netrw_liststyle = 3
let g:netrw_preview = 1
let g:netrw_alto = 0
let g:netrw_usetab = 1
let g:netrw_browsex_viewer = "xdg-open"
let g:NetrwIsOpen = 0 "for toggle function
let g:netrw_nogx = 1

"ale
let g:ale_linters = {
\   'python': ['flake8', 'pylint'],
\   'javascript': ['eslint'],
\   'ruby': ['rubocop'],
\   'markdown': ['mdl', 'write-good']
\}

let g:ale_fixers = {
\   '*': ['remove_trailing_lines', 'trim_whitespace'],
\   'python': ['yapf', 'black'],
\   'javascript': ['prettier', 'eslint'],
\   'ruby': ['rubocop'],
\   'css': ['prettier'],
\   'scss': ['prettier'],
\   'html': ['prettier'],
\   'markdown': ['prettier']
\}

let g:ale_sign_error = '->'
let g:ale_sign_warning = '--'
let g:ale_lint_on_save = 1
let g:ale_fix_on_save_ignore = 1
highlight clear SignColumn

"vim-markdown
let g:vim_markdown_folding_disabled = 1
let g:vim_markdown_toc_autofit = 1
let g:vim_markdown_conceal = 0
let g:vim_markdown_frontmatter = 1

"vim-emmet
let g:user_emmet_install_global = 0

" colorschemes
" colorscheme adventurous
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
colorscheme solarized8_dark

" keybinds
"----------
"leader key
let mapleader=' '

"normal mode keybinds
"---------------
"paste and search
nnoremap <silent><leader><space> :noh <BAR> :let @/ = ""<CR>
nnoremap <leader>i :set invpaste<CR>

"toggles
nnoremap <silent><leader>lb :call ToggleLineBreak()<CR>
nnoremap <silent><leader>wr :call ToggleWrap()<CR>
nnoremap <silent><leader>cc :call ToggleColorColumn()<CR>

"dates
nnoremap <leader>ts "=strftime("%F %T%z")<CR>
nnoremap <leader>dt :r !date<CR>

"rc files
nnoremap <leader>rc :vsplit $MYVIMRC<CR>
nnoremap <leader>rcs :vsplit ~/.vim/settings.vim<CR>
nnoremap <leader>rcf :vsplit ~/.vim/functions.vim<CR>
nnoremap <leader>rcc :vsplit ~/.vim/commands.vim<CR>
nnoremap <leader>rck :vsplit ~/.vim/keybinds.vim<CR>
nnoremap <leader>rcp :vsplit ~/.vim/plugins.vim<CR>
nnoremap <leader>rca :vsplit ~/.vim/autocmds.vim<CR>
nnoremap <leader>rcft :vsplit $HOME/.vim/after/ftplugin/<CR>
nnoremap <leader>rcl :vsplit ~/.config/vimrc<CR>
nnoremap <leader>so :source $MYVIMRC<CR>

"windows
nnoremap <leader>cl :close<CR>

"formatting tools
nnoremap =j :%!python -m json.tool<CR>

"insert mode keybinds
"--------------------
"abbrevations
inoreabbrev <expr> #!! "#!/usr/bin/env"

"plugin keybinds
"---------------
"netrw
nnoremap <silent><leader>e :call ToggleNetrw()<CR>

"vim-plug
nnoremap <leader>pi :source $MYVIMRC <BAR> :PlugInstall<CR>
nnoremap <leader>pu :source $MYVIMRC <BAR> :PlugUpdate<CR>
nnoremap <leader>pc :source $MYVIMRC <BAR> :PlugClean<CR>

"openbrowser
nnoremap <leader>ob :OpenBrowser
nnoremap <leader>obs :OpenBrowserSearch
nmap gx <Plug>(openbrowser-smart-search)
vmap gx <Plug>(openbrowser-smart-search)

"ale
nnoremap <leader>at :ALEToggle<CR>
nmap <leader>af <Plug>(ale_fix)
nmap <leader>aK <Plug>(ale_hover)
nmap <leader>agd <Plug>(ale_go_to_definition)
nmap <leader>agd <Plug>(ale_go_to_definition)

"vim-emmet
let g:user_emmet_leader_key = ','

"git-gutter
nnoremap <leader>ggt :GitGutterToggle<CR>

"vim-fugitive
nnoremap <leader>gw :Gwrite<CR>
nnoremap <leader>gl :Glog<CR>
nnoremap <leader>gs :Gstatus<CR>
nnoremap <leader>gd :Gdiffsplit<CR>
nnoremap <leader>gp :Gpush<CR>
nnoremap <leader>ga :Git add %<CR>
nnoremap <leader>ga. :Git add .<CR>
nnoremap <leader>gcm :Gcommit<CR>

"fzf
nnoremap <leader>gf :GitFiles<CR>
nnoremap <leader>f :Files<CR>
nnoremap <leader>b :Buffers<CR>
nnoremap <leader>h :Helptags<CR>
nnoremap <leader>m :Maps<CR>
nnoremap <leader>rg :Rg<CR>

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

if has("autocmd")
    "filetype functions
    "------------------
    function FT_halftab()
        setlocal expandtab
        setlocal tabstop=2
        setlocal softtabstop=2
        setlocal shiftwidth=2
    endfunction

    function FT_python()
        setlocal autoindent
        setlocal formatprg=yapf
        iabbr false False
        iabbr true True

        xnoremap <leader>r <esc>:'<,'>:w !python3<CR>
    endfunction

    function FT_go()
        set noexpandtab
        let g:go_auto_type_info = 1
        let g:go_imports_autosave = 1
    endfunction

    function FT_html()
        syntax sync fromstart
    endfunction

    function FT_markdown()
        setlocal spell
    endfunction

    "global autocmds
    "-----------------
    augroup global
        autocmd!
        "keep equal proportions when windows resized
        autocmd VimResized * wincmd =
        "save cursor position in a file
        autocmd BufReadPost * if line("'\"") > 1 && line("'\"")
                    \ <= line("$") | exe "normal! g'\"" | endif
    augroup END

    "general filetype autocmds
    "-------------------------
    augroup emmet_group
        autocmd!
        autocmd FileType html,css EmmetInstall
    augroup END

    augroup prettier_format_group
        autocmd!
        autocmd FileType javascript setlocal formatprg=prettier
        autocmd FileType typescript setlocal formatprg=prettier\ --parser\ typescript
        autocmd FileType vue setlocal formatprg=prettier\ --parser\ vue
        autocmd FileType html setlocal formatprg=prettier\ --parser\ html
        autocmd FileType css setlocal formatprg=prettier\ --parser\ css
        autocmd FileType scss setlocal formatprg=prettier\ --parser\ scss
        autocmd FileType markdown setlocal formatprg=prettier\ --parser\ markdown
        autocmd FileType json setlocal formatprg=prettier\ --parser\ json
        autocmd FileType yaml setlocal formatprg=prettier\ --parser\ yaml
    augroup END

    augroup halftab_indent_group
        autocmd!
        autocmd FileType sh,bash,html,css,scss,javascript,json,toml,yaml call FT_halftab()
    augroup END

    "language specific autocmds
    "--------------------------
    augroup language_group
        autocmd FileType python call FT_python()
        autocmd FileType go call FT_go()
        autocmd FileType html call FT_html()
        autocmd FileType markdown call FT_markdown()
    augroup END
endif

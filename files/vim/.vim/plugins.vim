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

"pre plugin settings

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
    "html
    Plug 'mattn/emmet-vim'
    "markdown
    Plug 'plasticboy/vim-markdown'
    Plug 'godlygeek/tabular'
    "toml
    Plug 'cespare/vim-toml'
    "linting and lsp
    Plug 'w0rp/ale'

    " == extra ==
    Plug 'flazz/vim-colorschemes'
    Plug 'editorconfig/editorconfig-vim'
    Plug 'tyru/open-browser.vim'
call plug#end()

"post plugin settings
"---------------{{{
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
\   'markdown': ['mdl', 'write-good']
\}

let g:ale_fixers = {
\   '*': ['remove_trailing_lines', 'trim_whitespace'],
\   'python': ['yapf', 'black'],
\   'javascript': ['prettier', 'eslint'],
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

"}}}

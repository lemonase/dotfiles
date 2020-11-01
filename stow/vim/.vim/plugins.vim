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
    "   quality of life
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

    "   syntax and colorscheme packs
    Plug 'flazz/vim-colorschemes'
    Plug 'sheerun/vim-polyglot'

    "   linting and lsp
    Plug 'w0rp/ale'
    " Plug 'prabirshrestha/vim-lsp'
    " Plug 'mattn/vim-lsp-settings'

    "   extra language plugins
    "go
    Plug 'fatih/vim-go'
    "html
    Plug 'mattn/emmet-vim'

    "   other useful things
    "editorconfig
    Plug 'editorconfig/editorconfig-vim'
    "browser/url opener
    Plug 'tyru/open-browser.vim'
    "tables
    Plug 'godlygeek/tabular'
call plug#end()

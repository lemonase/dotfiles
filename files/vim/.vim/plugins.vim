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
    "async task runner
    Plug 'tpope/vim-dispatch'
    "git
    Plug 'tpope/vim-fugitive'
    Plug 'tpope/vim-rhubarb'
    Plug 'airblade/vim-gitgutter'
    "fzf
    Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
    Plug 'junegunn/fzf.vim'
    "snippet engine
    Plug 'SirVer/ultisnips'
    Plug 'honza/vim-snippets'

    "   syntax and colorscheme packs
    Plug 'flazz/vim-colorschemes'
    Plug 'sheerun/vim-polyglot'

    "   linting and lsp
    Plug 'w0rp/ale'

    "   extra language plugins
    "go
    Plug 'fatih/vim-go'
    "html
    Plug 'mattn/emmet-vim'
    "markdown
    Plug 'plasticboy/vim-markdown'

    "   other useful things
    "editorconfig
    Plug 'editorconfig/editorconfig-vim'
    "browser/url opener
    Plug 'tyru/open-browser.vim'
    "tables
    Plug 'godlygeek/tabular'
call plug#end()

"plugin settings
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

"UltiSnips
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<c-n>"
let g:UltiSnipsJumpBackwardTrigger="<c-p>"

"}}}

"plugin keybinds
"{{{
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

"UltiSnips
inoremap <C-S> <ESC> :Snippets<CR>
nnoremap <leader>sn :Snippets<CR>
nnoremap <leader>se <ESC>:UltiSnipsEdit<CR>

"}}}

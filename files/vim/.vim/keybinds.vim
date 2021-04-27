"========
"Keybinds
"========

"leader key
let mapleader=' '

"----------------
"regular keybinds
"----------------

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
nnoremap <leader>so :source $MYVIMRC<CR>
nnoremap <leader>rcl :vsplit ~/.config/vimrc<CR>
nnoremap <leader>rcft :vsplit $HOME/.vim/after/ftplugin/<CR>

"windows
nnoremap <leader>cl :close<CR>

"formatting tools
nnoremap =j :%!python -m json.tool<CR>

"abbrevations
inoreabbrev <expr> #!! "#!/usr/bin/env"

"---------------
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
let g:user_emmet_leader_key = '<C-E>'

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
nnoremap <leader>gcm :Git commit<CR>

"fzf
nnoremap <leader>gf :GitFiles<CR>
nnoremap <leader>f :Files<CR>
nnoremap <leader>b :Buffers<CR>
nnoremap <leader>h :Helptags<CR>
nnoremap <leader>m :Maps<CR>
nnoremap <leader>rg :Rg<CR>

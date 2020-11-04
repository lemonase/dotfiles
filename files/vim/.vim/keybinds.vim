"leader key
let mapleader=' '

"normal keybinds
"-------------{{{
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
"}}}

"insert mode
"-----------{{{
"abbrevations
inoreabbrev <expr> #!! "#!/usr/bin/env"
"}}}

"plugin keybinds
"{{{
"netrw
nnoremap <silent><leader>e :call ToggleNetrw()<CR>

"openbrowser
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

"lsp
function! s:on_lsp_buffer_enabled() abort
    setlocal omnifunc=lsp#complete
    setlocal signcolumn=yes
    if exists('+tagfunc') | setlocal tagfunc=lsp#tagfunc | endif
    nmap <buffer> gd <plug>(lsp-definition)
    nmap <buffer> gr <plug>(lsp-references)
    nmap <buffer> gi <plug>(lsp-implementation)
    nmap <buffer> gt <plug>(lsp-type-definition)
    nmap <buffer> rn <plug>(lsp-rename)
    nmap <buffer> [g <Plug>(lsp-previous-diagnostic)
    nmap <buffer> ]g <Plug>(lsp-next-diagnostic)
    nmap <buffer> K <plug>(lsp-hover)
    nmap <buffer> <leader>lf <plug>(lsp-document-format)
    nmap <buffer> <leader>lst <plug>(lsp-status)
endfunction
"}}}

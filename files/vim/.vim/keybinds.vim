"leader key
let mapleader=' '

"normal keybinds
"-------------{{{
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
nnoremap <leader>rck :vsplit ~/.vim/keybinds.vim<CR>
nnoremap <leader>rcb :vsplit ~/.vim/keybinds.vim<CR>
nnoremap <leader>rcp :vsplit ~/.vim/plugins.vim<CR>
nnoremap <leader>rcf :vsplit $HOME/.vim/after/ftplugin/<CR>
nnoremap <leader>so :source $MYVIMRC<CR>

"windows
nnoremap <leader>cl :close<CR>

"formatting tools
nnoremap =j :%!python -m json.tool<CR>
"}}}

"insert mode
"-----------{{{
"abbrevations
inoreabbrev <expr> #!! "#!/usr/bin/env"
"}}}

"autocmds
"--------{{{
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

    augroup emmet_group
        autocmd!
        autocmd FileType html,css EmmetInstall
    augroup END

    augroup half_tab_group
        autocmd FileType html,css,javascript,json,toml,yaml
                    \ setlocal ts=2 sts=2 sw=2 expandtab
    augroup END

endif
"}}}

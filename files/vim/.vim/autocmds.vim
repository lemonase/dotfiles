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
        autocmd!
        autocmd FileType html,css,javascript,json,toml,yaml
                    \ setlocal ts=2 sts=2 sw=2 expandtab
    augroup END

    augroup format_group
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
        autocmd FileType python setlocal formatprg=yapf
    augroup END

endif
"}}}

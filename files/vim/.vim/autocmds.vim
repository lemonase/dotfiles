if has("autocmd")
    "filetype functions
    "------------------
    function FT_python()
        setlocal autoindent
        setlocal formatprg=yapf
        iabbr false False
        iabbr true True

        xnoremap <leader>r <esc>:'<,'>:w !python3<CR>
    endfunction

    function FT_ruby()
        setlocal tabstop=2 softtabstop=2 shiftwidth=2 expandtab
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

    function FT_halftab()
        setlocal tabstop=2 softtabstop=2 shiftwidth=2 expandtab
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
        autocmd FileType ruby,eruby call FT_ruby()
        autocmd FileType go call FT_go()
        autocmd FileType html call FT_html()
        autocmd FileType markdown call FT_markdown()
    augroup END
endif

"============
"Autocommands
"============

if has("autocmd")
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
        autocmd BufNewFile,BufRead *html*,*css*,*js* EmmetInstall
    augroup END

    "language specific filetype functions (run by autocmds)
    "------------------
    function FT_python()
        setlocal autoindent
        setlocal formatprg=yapf
        iabbr false False
        iabbr true True

        xnoremap <leader>r <esc>:'<,'>:w !python3<CR>
    endfunction

    function FT_ruby()
        setlocal autoindent
        setlocal formatprg=rubocop
    endfunction

    function FT_go()
        set noexpandtab
        setlocal formatprg=gofmt
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

    "language specific autocmds
    "--------------------------
    augroup language_group
        autocmd FileType python call FT_python()
        autocmd FileType ruby,eruby call FT_ruby()
        autocmd FileType go call FT_go()
        autocmd FileType html call FT_html()
        autocmd FileType markdown call FT_markdown()
        autocmd FileType sh,ruby,bash,html,css,scss,javascript,json,toml,yaml call FT_halftab()
    augroup END
endif

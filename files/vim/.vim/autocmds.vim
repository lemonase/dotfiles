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

	" augroup lsp_install
	" 	autocmd!
	" 	autocmd User lsp_buffer_enabled call s:on_lsp_buffer_enabled()
	" augroup END
endif
"}}}

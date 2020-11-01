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

    augroup filetype_bash
        autocmd! BufNewFile,BufRead *.bash call BashSettings()
    augroup END

    augroup filetype_python
        autocmd! FileType python call PythonSettings()
    augroup END

    augroup filetype_go
        autocmd! FileType go call GoSettings()
    augroup END

    augroup filetype_html
        autocmd! FileType html call HtmlSettings()
    augroup END

    augroup filetype_spell
        autocmd! FileType markdown call SpellSettings()
    augroup END

    augroup filetype_halftab
        autocmd! FileType html,javascript,css,json,yaml,sh call HalftabSettings()
    augroup END

	augroup lsp_install
		autocmd!
		autocmd User lsp_buffer_enabled call s:on_lsp_buffer_enabled()
	augroup END
endif
"}}}

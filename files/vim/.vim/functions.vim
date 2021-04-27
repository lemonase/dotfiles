"=========
"Functions
"=========

"toggle functions
"----------------
function! ToggleColorColumn()
    if &cc == ''
        set cc=80
    else
        set cc=
    endif
endfunction

function! ToggleLineBreak()
    if &lbr == ''
        set fo+=t "Autowraps text with textwidth
        set fo-=l "Wraps long lines in --insert-- mode
        set lbr
    else
        set fo-=t
        set fo+=l
        set lbr!
    endif
endfunction

function! ToggleWrap()
    if &wrap == ''
        set wrap
    else
        set nowrap
    endif
endfunction

function! ToggleNetrw() "make netrw toggleable <https://vi.stackexchange.com/questions/10988/toggle-explorer-window>
    if g:NetrwIsOpen
        let i = bufnr("$")
        while (i >= 1)
            if (getbufvar(i, "&filetype") == "netrw")
                silent exe "bwipeout " . i
            endif
            let i-=1
        endwhile
        let g:NetrwIsOpen=0
    else
        let g:NetrwIsOpen=1
        silent Lexplore
    endif
endfunction

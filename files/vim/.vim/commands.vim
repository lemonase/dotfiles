"========
"Commands
"========

"common typos and abbreviations
cnoremap w!! w !sudo tee % > /dev/null
command! W w !sudo tee % > /dev/null
command! Q q
command! WQ wq
command! Q1 q!

"useful shortcut commands
command! Trim %s/\s\+$//

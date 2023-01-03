-- init.lua --

-- general vim settings
require('settings')

-- vim keymaps
require('keymaps')

-- package manager (packer.nvim)
require('plugins')

-- plugin configurations
require('plugin-config')

-- platform/os specific stuff
if vim.fn.has('win32') then
  vim.cmd([[
    " WSL yank support
    let s:clip = '/mnt/c/Windows/System32/clip.exe'  " change this path according to your mount point
    if executable(s:clip)
        augroup WSLYank
            autocmd!
            autocmd TextYankPost * if v:event.operator ==# 'y' | call system(s:clip, @0) | endif
        augroup END
    endif
  ]])
end

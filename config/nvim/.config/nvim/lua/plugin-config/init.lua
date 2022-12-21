-- plugin-config/init.lua

-- lsp
require('plugin-config.lsp')

-- completion
require('plugin-config.cmp')

-- treesitter (syntax highlighting)
require('plugin-config.nvim-treesitter')

-- nvim-tree
require('plugin-config.nvim-tree')

-- plugin keybinds
require('plugin-config.keybinds.vim-fugitive')
require('plugin-config.keybinds.fzf')
require('plugin-config.keybinds.nvim-tree')

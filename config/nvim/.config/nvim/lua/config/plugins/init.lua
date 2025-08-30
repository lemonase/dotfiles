-- config/plugins/init.lua

-- * LSP, Completion and TreeSitter * --
-- lsp
require('config.plugins.lsp')
-- completion
require('config.plugins.cmp')
-- treesitter (tree based syntax highlighting)
require('config.plugins.nvim-treesitter')

-- * File explorer and Fuzzy Finder * --
-- nvim-tree
require('config.plugins.oil')
-- fzf (fuzzy finder)
require('config.plugins.fzf')

-- * Git Integration * --
-- gitsigns
require('config.plugins.gitsigns')
-- vim-fugitive (git plugins)
require('config.plugins.vim-fugitive')

-- * Discord Presence * --
-- require('config.plugins-config.discord-presence')

-- colorscheme
require('config.plugins.colorscheme')

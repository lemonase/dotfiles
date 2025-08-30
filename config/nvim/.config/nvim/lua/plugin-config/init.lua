-- plugin-config/init.lua

-- * LSP, Completion and TreeSitter * --
-- lsp
require('plugin-config.lsp')
-- completion
require('plugin-config.cmp')
-- treesitter (tree based syntax highlighting)
require('plugin-config.nvim-treesitter')

-- * File explorer and Fuzzy Finder * --
-- nvim-tree
require('plugin-config.oil')
-- fzf (fuzzy finder)
require('plugin-config.fzf')

-- * Git Integration * --
-- gitsigns
require('plugin-config.gitsigns')
-- vim-fugitive (git plugin)
require('plugin-config.vim-fugitive')

-- * Discord Presence * --
-- require('plugin-config.discord-presence')

-- colorscheme
require('plugin-config.colorscheme')

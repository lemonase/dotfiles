-- keymaps.lua

vim.g.mapleader = ' '

-- common vim commands in normal mode
vim.keymap.set('n', '<leader>w', ':w<CR>')

-- paste and search
vim.keymap.set('n', '<silent><leader><space>', ':noh <BAR> :let @/ = ""<CR>')
vim.keymap.set('n', '<leader>i', ':set invpaste<CR>')

-- dates
vim.keymap.set('n', '<leader>ts', '"=strftime("%F %T%z")<CR>')
vim.keymap.set('n', '<leader>dt', ':r !date<CR>')

-- rc files
vim.keymap.set('n', '<leader>rc', ':vsplit $MYVIMRC<CR>')
vim.keymap.set('n', '<leader>so', ':source $MYVIMRC<CR>')
vim.keymap.set('n', '<leader>rcc', ':vsplit ~/.config/nvim/lua/config/<CR>')
vim.keymap.set('n', '<leader>rck', ':tabnew ~/.config/nvim/lua/config/keymaps.lua<CR>')

vim.keymap.set('n', '<leader>rcp', function()
  vim.cmd(':tabnew ~/.config/nvim/lua/config/lazy.lua')
  vim.cmd(':vsplit ~/.config/nvim/lua/plugin-config/init.lua')
  vim.cmd(':split ~/.config/nvim/lua/plugin-config/')
end)

vim.keymap.set('n', '<leader>rcl', function()
  vim.cmd(':tabnew ~/.config/nvim/lua/plugin-config/lsp.lua')
  vim.cmd(':vsplit ~/.config/nvim/lua/plugin-config/cmp.lua')
  vim.cmd(':vsplit ~/.config/nvim/lua/plugin-config/nvim-treesitter.lua')
end)

-- windows
vim.keymap.set('n', '<leader>cl', ':close<CR>')

-- formatting tools
vim.keymap.set('n', '=j', ':%!python -m json.tool<CR>')

-- plugins
vim.keymap.set('n', '<leader>pu', ':Lazy update<CR>')
vim.keymap.set('n', '<leader>ps', ':Lazy sync<CR>')
vim.keymap.set('n', '<leader>pi', ':Lazy install<CR>')

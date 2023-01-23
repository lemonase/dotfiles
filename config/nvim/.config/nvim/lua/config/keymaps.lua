--
-- keymaps
--

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
vim.keymap.set('n', '<leader>rcl', ':vsplit ~/.config/nvim/lua/<CR>')

-- windows
vim.keymap.set('n', '<leader>cl', ':close<CR>')

-- formatting tools
vim.keymap.set('n', '=j', ':%!python -m json.tool<CR>')

-- vim-plug
vim.keymap.set('n', '<leader>ps', ':source $MYVIMRC <BAR> :PackerSync<CR>')
vim.keymap.set('n', '<leader>pi', ':source $MYVIMRC <BAR> :PackerInstall<CR>')
vim.keymap.set('n', '<leader>pu', ':source $MYVIMRC <BAR> :PackerUpdate<CR>')
vim.keymap.set('n', '<leader>pc', ':source $MYVIMRC <BAR> :PackerClean<CR>')

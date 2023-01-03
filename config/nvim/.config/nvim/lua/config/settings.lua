--
-- general settings
--

-- encoding/format
vim.opt.encoding = 'utf-8'
vim.opt.fileformats = 'unix,dos,mac'

-- status bar
vim.opt.wildmenu = true
vim.opt.showcmd = true
vim.opt.showmatch = true
vim.opt.laststatus = 2

-- search
vim.opt.hlsearch = true
vim.opt.incsearch = true
vim.opt.ignorecase = true
vim.opt.smartcase = true

-- indent
vim.opt.autoindent = true
vim.opt.expandtab = true
vim.opt.shiftwidth = 4
vim.opt.tabstop = 4
vim.opt.softtabstop = 0

-- buffer behaviour
vim.opt.autoread = true
vim.opt.autowrite = true
vim.opt.confirm = true

-- cursor
vim.opt.virtualedit = 'block'
vim.opt.backspace = 'indent,eol,start'
vim.opt.scrolloff = 2
vim.opt.sidescrolloff = 4

-- window behaviour (ltr)
vim.opt.splitbelow = true
vim.opt.splitright = true

-- display
vim.opt.background = "dark"
vim.opt.termguicolors = true
vim.opt.number = true
vim.opt.wrap = true
vim.opt.listchars = {eol = '↲', tab = '▸ ', trail = '·'}

-- vim cmds
vim.cmd('syntax enable')
vim.cmd('filetype plugin indent on')

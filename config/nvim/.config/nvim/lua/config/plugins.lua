--
-- plugins
--

local install_path = vim.fn.stdpath 'data' .. '/site/pack/packer/start/packer.nvim'
if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
  vim.fn.execute('!git clone https://github.com/wbthomason/packer.nvim ' .. install_path)

  vim.cmd('packadd packer.nvim')
end

require('packer').startup(function(use)
  use 'wbthomason/packer.nvim'

  -- * nvim specific plugins * --
  use { -- LSP Configuration & Plugins
    'neovim/nvim-lspconfig',
    requires = {
      'williamboman/mason.nvim',
      'williamboman/mason-lspconfig.nvim',
      'j-hui/fidget.nvim',
    },
  }

  use { -- Autocompletion (cmp)
    'hrsh7th/nvim-cmp',
    requires = {
      'hrsh7th/cmp-nvim-lsp',
      'L3MON4D3/LuaSnip',
      'saadparwaiz1/cmp_luasnip'
    },
  }

  use { -- Highlighting/nav/editing (TreeSitter)
    'nvim-treesitter/nvim-treesitter',
    run = function()
      pcall(require('nvim-treesitter.install').update { with_sync = true })
    end,
  }

  use { -- File Explorer
    'nvim-tree/nvim-tree.lua',
    requires = {
      'nvim-tree/nvim-web-devicons', -- optional, for file icons
    },
    tag = 'nightly'
  }

  -- * vanilla vim plugins * --

  -- normal mode keybinds
  use 'tpope/vim-commentary'
  use 'tpope/vim-surround'
  use 'tpope/vim-unimpaired'

  -- command mode keybinds
  use 'tpope/vim-rsi'
  use 'tpope/vim-eunuch'
  use 'tpope/vim-repeat'

  -- git
  use 'tpope/vim-fugitive'
  use 'tpope/vim-rhubarb'
  -- use 'airblade/vim-gitgutter'
  use { 'lewis6991/gitsigns.nvim' }

  -- fzf
  use { 'junegunn/fzf', run = ":call fzf#install()" }
  use { 'junegunn/fzf.vim' }

  -- colorschemes
  -- use 'rose-pine/neovim'
  use 'w0ng/vim-hybrid'
  use 'folke/tokyonight.nvim'
  use 'bluz71/vim-nightfly-colors'
end)

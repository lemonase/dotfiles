--
-- plugins
--

local install_path = vim.fn.stdpath 'data' .. '/site/pack/packer/start/packer.nvim'
if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
  vim.fn.execute('!git clone https://github.com/wbthomason/packer.nvim ' .. install_path)
  vim.cmd [[packadd packer.nvim]]
end

require('packer').startup(function(use)
  use 'wbthomason/packer.nvim'
  use { -- LSP Configuration & Plugins
    'neovim/nvim-lspconfig',
    requires = {
      'williamboman/mason.nvim',
      'williamboman/mason-lspconfig.nvim',
      'j-hui/fidget.nvim',
    },
  }
  use { -- Autocompletion
    'hrsh7th/nvim-cmp',
    requires = { 'hrsh7th/cmp-nvim-lsp', 'L3MON4D3/LuaSnip', 'saadparwaiz1/cmp_luasnip' },
  }
  use { -- Highlight, edit, and navigate code
    'nvim-treesitter/nvim-treesitter',
    run = function()
      pcall(require('nvim-treesitter.install').update { with_sync = true })
    end,
  }

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
  use 'airblade/vim-gitgutter'

  use { 'junegunn/fzf', run = ":call fzf#install()" }
  use { 'junegunn/fzf.vim' }

  -- colorschemes
  use 'nanotech/jellybeans.vim'
  use({
    'rose-pine/neovim',
    as = 'rose-pine',
    config = function()
        vim.cmd('colorscheme rose-pine')
    end
  })
end)

-- vim.cmd [[
--   let autoload_plug_path = stdpath('data') . '/site/autoload/plug.vim'
--   if !filereadable(autoload_plug_path)
--     silent execute '!curl -fLo ' . autoload_plug_path . '  --create-dirs 
--         \ "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"'
--     autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
--   endif
--   unlet autoload_plug_path
-- ]]
-- local Plug = vim.fn['plug#']
-- vim.call('plug#begin', '~/.config/nvim/plugged')
--     -- plug
--     Plug 'junegunn/vim-plug'

--     -- normal mode keybinds
--     Plug 'tpope/vim-commentary'
--     Plug 'tpope/vim-surround'
--     Plug 'tpope/vim-unimpaired'
--     -- command mode keybinds
--     Plug 'tpope/vim-rsi'
--     Plug 'tpope/vim-eunuch'
--     Plug 'tpope/vim-repeat'

--     -- external tools
--     -- git
--     Plug 'tpope/vim-fugitive'
--     Plug 'tpope/vim-rhubarb'
--     Plug 'tpope/vim-dispatch'
--     Plug 'airblade/vim-gitgutter'

--     -- fzf
--     Plug 'junegunn/fzf'
--     Plug 'junegunn/fzf.vim'

--     -- colors
--     Plug 'flazz/vim-colorschemes'
-- vim.call('plug#end')


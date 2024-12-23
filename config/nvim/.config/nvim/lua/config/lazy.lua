-- Bootstrap lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
  local lazyrepo = "https://github.com/folke/lazy.nvim.git"
  local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
  if vim.v.shell_error ~= 0 then
    vim.api.nvim_echo({
      { "Failed to clone lazy.nvim:\n", "ErrorMsg" },
      { out, "WarningMsg" },
      { "\nPress any key to exit..." },
    }, true, {})
    vim.fn.getchar()
    os.exit(1)
  end
end
vim.opt.rtp:prepend(lazypath)

-- Setup lazy.nvim
require("lazy").setup({
  spec = {
    -- import your plugins
    {
      -- * vanilla vim plugins * --
      -- normal mode keybinds
      'tpope/vim-commentary',
      'tpope/vim-surround',
      'tpope/vim-unimpaired',

      -- command mode keybinds
      'tpope/vim-rsi',
      'tpope/vim-eunuch',
      'tpope/vim-repeat',
      'tpope/vim-unimpaired',

      -- git
      'tpope/vim-fugitive',
      'tpope/vim-rhubarb',
      'lewis6991/gitsigns.nvim',

      -- fzf
      'junegunn/fzf', run = ":call fzf#install()",
      'junegunn/fzf.vim',

      -- filetree
      'stevearc/oil.nvim',

      -- lsp
      "williamboman/mason.nvim",
      "williamboman/mason-lspconfig.nvim",
      "neovim/nvim-lspconfig",
      'hrsh7th/cmp-nvim-lsp',
      'hrsh7th/cmp-buffer',
      'hrsh7th/cmp-path',
      'hrsh7th/cmp-cmdline',
      "L3MON4D3/LuaSnip",
      'hrsh7th/nvim-cmp',
      'nvim-treesitter/nvim-treesitter',
      "j-hui/fidget.nvim",

      -- colorschemes
      'rose-pine/neovim',
      'w0ng/vim-hybrid',
      'folke/tokyonight.nvim',
      'bluz71/vim-nightfly-colors',
      'Shatur/neovim-ayu'
    },
  },
  checker = { enabled = true },
})

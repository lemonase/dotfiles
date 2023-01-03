local ok, _ = pcall(vim.cmd, 'colorscheme tokyonight')
-- local ok, _ = pcall(vim.cmd, 'colorscheme tokyonight-storm')
-- local ok, _ = pcall(vim.cmd, 'colorscheme tokyonight-night')
-- local ok, _ = pcall(vim.cmd, 'colorscheme tokyonight-moon')
if not ok then
  vim.cmd 'colorscheme default' -- if the above fails, then use default
end

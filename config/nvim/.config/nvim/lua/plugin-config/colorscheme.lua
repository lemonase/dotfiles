local ok, _ = pcall(vim.cmd, 'colorscheme hybrid')
if not ok then
  vim.cmd 'colorscheme delek' -- if the above fails, then use default
end

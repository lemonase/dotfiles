-- source local rc file
local local_rc = vim.fn.expand("~/.local/.nvimrc")
if (vim.fn.filereadable(local_rc)) then
  vim.cmd.source(local_rc)
end


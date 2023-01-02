vim.o.laststatus = 0
vim.api.nvim_create_autocmd('User', {
  pattern = 'ActuallyEditing',
  once = true,
  callback = function()
    vim.o.laststatus = 3
    statusline = require 'statusline'
    vim.o.statusline = '%!v:lua.statusline.status()'
  end,
})

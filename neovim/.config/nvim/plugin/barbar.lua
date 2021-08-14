local map = require('config.utils').map

vim.g.bufferline = {
  auto_hide = true,
}

local opts = { silent = true, nowait = true }
map('n', '<c-q>', '<cmd>BufferPick<cr>', opts)
map('n', '<c-p>', '<cmd>BufferPin<cr>', opts)
map('n', '<leader>d', '<cmd>BufferClose<cr>', opts)

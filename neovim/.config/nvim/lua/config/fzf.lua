local map = require('config.utils').map

local silent = {silent = true}
-- Navigate buffers and repos
map('n', '<c-b>', [[<cmd>History<cr>]], silent)
map('n', '<c-a>', [[<cmd>GFiles<cr>]], silent)
map('n', '<c-p>', [[<cmd>Files<cr>]], silent)

-- FZF LSP
require('fzf_lsp').setup()

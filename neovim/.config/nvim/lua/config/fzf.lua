local map = require('config.utils').map

-- Navigate buffers and repos
map('n', '<c-b>', [[<cmd>History<cr>]], silent)
map('n', '<c-a>', [[<cmd>GFiles<cr>]], silent)

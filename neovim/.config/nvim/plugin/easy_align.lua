local map = vim.api.nvim_set_keymap

-- Keybindings
local options = { silent = true, noremap = false }
map('x', 'ga', '<Plug>(EasyAlign)', options)
map('n', 'ga', '<Plug>(EasyAlign)', options)

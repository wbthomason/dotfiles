local map = vim.api.nvim_set_keymap
local g = vim.g

-- 2-character Sneak (default)
local opts = {noremap = false}
map('n', 'z', '<Plug>(leap-forward-x)', opts)
map('n', 'Z', '<Plug>(leap-backward-x)', opts)

-- visual-mode
map('x', 'z', '<Plug>(leap-forward-x)', opts)
map('x', 'Z', '<Plug>(leap-backward-x)', opts)

-- operator-pending-mode
map('o', 'z', '<Plug>(leap-forward-x)', opts)
map('o', 'Z', '<Plug>(leap-backward-x)', opts)

local map = require('config.utils').map
local g = vim.g

-- 2-character Sneak (default)
local opts = {noremap = false}
map('n', 'z', '<Plug>Sneak_s', opts)
map('n', 'Z', '<Plug>Sneak_S', opts)

-- visual-mode
map('x', 'z', '<Plug>Sneak_s', opts)
map('x', 'Z', '<Plug>Sneak_S', opts)

-- operator-pending-mode
map('o', 'z', '<Plug>Sneak_s', opts)
map('o', 'Z', '<Plug>Sneak_S', opts)

-- repeat motion
map('', ';', '<Plug>Sneak_;', opts)
map('', [[\]], '<Plug>Sneak_,', opts)

-- 1-character enhanced 'f'
map('n', 'f', '<Plug>Sneak_f', opts)
map('n', 'F', '<Plug>Sneak_F', opts)

-- visual-mode
map('x', 'f', '<Plug>Sneak_f', opts)
map('x', 'F', '<Plug>Sneak_F', opts)

-- operator-pending-mode
map('o', 'f', '<Plug>Sneak_f', opts)
map('o', 'F', '<Plug>Sneak_F', opts)

-- 1-character enhanced 't'
map('n', 't', '<Plug>Sneak_t', opts)
map('n', 'T', '<Plug>Sneak_T', opts)

-- visual-mode
map('x', 't', '<Plug>Sneak_t', opts)
map('x', 'T', '<Plug>Sneak_T', opts)

-- operator-pending-mode
map('o', 't', '<Plug>Sneak_t', opts)
map('o', 'T', '<Plug>Sneak_T', opts)

g['sneak#s_next'] = 1
g['sneak#label'] = 1
g['sneak#use_ic_scs'] = 1

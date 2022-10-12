local neogen = require 'neogen'
local map = vim.api.nvim_set_keymap

neogen.setup { enabled = true, jump_map = '<tab>' }
map('n', '<localleader>d', '', {
  callback = function()
    require('neogen').generate()
  end,
  noremap = true,
})
map('n', '<localleader>df', '', {
  callback = function()
    require('neogen').generate { type = 'func' }
  end,
  noremap = true,
})
map('n', '<localleader>dc', '', {
  callback = function()
    require('neogen').generate { type = 'class' }
  end,
  noremap = true,
})

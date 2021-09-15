local neogen = require 'neogen'
local map = require('config.utils').map

neogen.setup { enabled = true, jump_map = '<tab>' }
map('n', '<localleader>d', '<cmd>lua require("neogen").generate()<cr>')
map('n', '<localleader>df', '<cmd>lua require("neogen").generate({ type = "func" })<cr>')
map('n', '<localleader>dc', '<cmd>lua require("neogen").generate({ type = "class" })<cr>')

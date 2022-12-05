require 'impatient'.enable_profile()
-- vim.go.packpath = '~/.local/share/nvim/site'
vim.notify = function(_, m, l, o)
  local notify = require 'notify'
  vim.notify = notify
  notify(_, m, l, o)
end
require 'config.disable_builtins'
require 'config.options'
require 'config.autocmds'
require 'config.commands'
require 'config.keymaps'
require 'config.colorscheme'

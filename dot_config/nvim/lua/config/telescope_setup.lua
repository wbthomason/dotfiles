local map = vim.api.nvim_set_keymap

local function files_fallback()
  vim.fn.system 'git rev-parse --is-inside-work-tree'
  local dropdown = require('telescope.themes').get_dropdown()
  if vim.v.shell_error == 0 then
    require('telescope.builtin').git_files(dropdown)
  else
    require('telescope.builtin').find_files(dropdown)
  end
end

local silent = { silent = true, noremap = true }
map('n', '<c-a>', [[<cmd>Telescope buffers show_all_buffers=true theme=get_dropdown<cr>]], silent)
map('n', '<c-d>', '', { silent = true, noremap = true, callback = files_fallback })
map('n', '<c-g>', [[<cmd>Telescope live_grep theme=get_dropdown<cr>]], silent)
map('n', '<c-p>', [[<cmd>Telescope commands theme=get_dropdown<cr>]], silent)
map('n', '<c-s>', [[<cmd>Telescope aerial theme=get_dropdown<cr>]], silent)

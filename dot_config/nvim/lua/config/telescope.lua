local telescope = require 'telescope'
telescope.setup {
  defaults = {
    color_devicons = false,
    layout_strategy = 'flex',
    layout_config = { anchor = 'N' },
    scroll_strategy = 'cycle',
    theme = require('telescope.themes').get_dropdown { layout_config = { prompt_position = 'top' } },
  },
  extensions = {
    fzf = {
      fuzzy = true,
      override_generic_sorter = true,
      override_file_sorter = true,
      case_mode = 'smart_case',
    },
    ['ui-select'] = {
      require('telescope.themes').get_dropdown { layout_config = { prompt_position = 'top' } },
    },
  },
  pickers = {
    buffers = {
      ignore_current_buffer = true,
      sort_mru = true,
      -- sort_lastused = true,
      previewer = false,
    },
  },
}

-- Extensions
telescope.load_extension 'aerial'
telescope.load_extension 'fzf'
telescope.load_extension 'noice'
telescope.load_extension 'ui-select'
telescope.load_extension 'cmake_tools'
telescope.load_extension 'undo'

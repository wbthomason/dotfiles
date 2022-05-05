local telescope = require 'telescope'
telescope.setup {
  defaults = {
    layout_strategy = 'flex',
    scroll_strategy = 'cycle',
  },
  extensions = {
    frecency = { workspaces = { exo = '/home/wil/projects/research/exoplanet' } },
    fzf = {
      fuzzy = true,
      override_generic_sorter = true,
      override_file_sorter = true,
      case_mode = 'smart_case',
    },
    ['ui-select'] = {
      require('telescope.themes').get_dropdown {},
    },
  },
  pickers = {
    lsp_references = { theme = 'dropdown' },
    lsp_code_actions = { theme = 'dropdown' },
    lsp_definitions = { theme = 'dropdown' },
    lsp_implementations = { theme = 'dropdown' },
    buffers = {
      ignore_current_buffer = true,
      sort_mru = true,
      previewer = false,
    },
  },
}

-- Extensions
telescope.load_extension 'frecency'
telescope.load_extension 'fzf'
telescope.load_extension 'ui-select'

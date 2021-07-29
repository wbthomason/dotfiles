local telescope = require 'telescope'
telescope.setup {
  defaults = { layout_strategy = 'flex', scroll_strategy = 'cycle' },
  extensions = { frecency = { workspaces = { exo = '/home/wil/projects/research/exoplanet' } } },
}

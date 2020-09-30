local ts_configs = require('nvim-treesitter.configs')

ts_configs.setup {
  ensure_installed = {
    'c',
    'cpp',
    'lua',
    'css',
    'fennel',
    'html',
    'javascript',
    'json',
    'ocaml',
    'ocaml_interface',
    'python',
    'rust',
    'toml',
    'typescript'
  },
  highlight = {
    enable = true,
    disable = { 'cpp' }
  },
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = 'gnn',
      node_incremental = 'grn',
      scope_incremental = 'grc',
      node_decremental = 'grm',
    },
  },
  refactor = {
    smart_rename = {
      enable = true,
      keymaps = {
        smart_rename = "grr",
      },
    },
    highlight_definitions = { enable = true }
  },
}

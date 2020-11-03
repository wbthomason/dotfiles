local ts_configs = require('nvim-treesitter.configs')

ts_configs.setup {
  ensure_installed = {
    'bash',
    'c',
    'cpp',
    'lua',
    'css',
    'fennel',
    'html',
    'javascript',
    'json',
    'julia',
    'ocaml',
    'ocaml_interface',
    'python',
    'rust',
    'toml',
    'typescript'
  },
  highlight = {
    enable = true,
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
    highlight_definitions = { enable = true },
    highlight_current_scope = { enable = true }
  },
  textobjects = { -- syntax-aware textobjects
  enable = true,
  disable = {},
  keymaps = {
    ["iL"] = { -- you can define your own textobjects directly here
    python = "(function_definition) @function",
    cpp = "(function_definition) @function",
    c = "(function_definition) @function",
    java = "(method_declaration) @function"
  },
  -- or you use the queries from supported languages with textobjects.scm
  ["af"] = "@function.outer",
  ["if"] = "@function.inner",
  ["aC"] = "@class.outer",
  ["iC"] = "@class.inner",
  ["ac"] = "@conditional.outer",
  ["ic"] = "@conditional.inner",
  ["ae"] = "@block.outer",
  ["ie"] = "@block.inner",
  ["al"] = "@loop.outer",
  ["il"] = "@loop.inner",
  ["is"] = "@statement.inner",
  ["as"] = "@statement.outer",
  ["ad"] = "@comment.outer",
  ["am"] = "@call.outer",
  ["im"] = "@call.inner"
}
      },
    }
    vim.api.nvim_set_option('foldmethod', 'expr')
    vim.api.nvim_set_option('foldexpr', 'nvim_treesitter#foldexpr()')

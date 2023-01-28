local function in_vscode()
  return vim.g.vscode ~= nil
end

return {
  'lewis6991/impatient.nvim',
  {
    'ojroques/nvim-bufdel',
    cmd = 'BufDel',
    config = function()
      require('bufdel').setup {}
    end,
  },
  {
    'folke/which-key.nvim',
    config = function()
      require('which-key').setup {}
    end,
    event = 'BufReadPost',
  },
  { 'chaoren/vim-wordmotion', event = 'User ActuallyEditing' },
  {
    'ggandor/leap.nvim',
    event = 'User ActuallyEditing',
    dependencies = 'tpope/vim-repeat',
    config = function()
      local map = vim.api.nvim_set_keymap
      -- 2-character Sneak (default)
      local opts = { noremap = false }
      map('n', 'z', '<Plug>(leap-forward-x)', opts)
      map('n', 'Z', '<Plug>(leap-backward-x)', opts)

      -- visual-mode
      map('x', 'z', '<Plug>(leap-forward-x)', opts)
      map('x', 'Z', '<Plug>(leap-backward-x)', opts)

      -- operator-pending-mode
      map('o', 'z', '<Plug>(leap-forward-x)', opts)
      map('o', 'Z', '<Plug>(leap-backward-x)', opts)
    end,
  },
  {
    'ggandor/flit.nvim',
    config = function()
      require('flit').setup { labeled_modes = 'nv' }
    end,
    event = 'User ActuallyEditing',
  },
  { 'Olical/vim-enmasse', cmd = 'EnMasse' },
  { 'kevinhwang91/nvim-bqf', event = 'QuickFixCmdPre' },
  {
    url = 'https://gitlab.com/yorickpeterse/nvim-pqf',
    config = function()
      require('pqf').setup()
    end,
    event = 'QuickFixCmdPre',
  },
  {
    'lukas-reineke/indent-blankline.nvim',
    event = 'User ActuallyEditing',
    dependencies = 'nvim-treesitter',
    config = function()
      require('indent_blankline').setup {
        char = '│',
        space_char_blankline = ' ',
        use_treesitter = true,
        show_first_indent_level = false,
        show_trailing_blankline_indent = false,
        show_current_context = true,
        use_treesitter_scope = false,
        context_patterns = {
          '^for',
          '^func',
          '^if',
          '^object',
          '^table',
          '^while',
          'argument_list',
          'arguments',
          'block',
          'catch_clause',
          'class',
          'dictionary',
          'do_block',
          'element',
          'else_clause',
          'except',
          'for',
          'function',
          'if_statement',
          'import_statement',
          'method',
          'object',
          'operation_type',
          'return',
          'table',
          'try',
          'try_statement',
          'tuple',
          'while',
          'with',
        },
      }
    end,
  },
  {
    'numToStr/Comment.nvim',
    event = 'User ActuallyEditing',
    config = function()
      require('Comment').setup {}
    end,
  },
  { 'machakann/vim-sandwich', event = 'User ActuallyEditing' },
  {
    'andymass/vim-matchup',
    init = function()
      require 'config.matchup'
    end,
    event = 'User ActuallyEditing',
  },
  { 'romainl/vim-cool', event = 'User ActuallyEditing' },
  -- { 'junegunn/vim-easy-align', disable = true }
  { 'wellle/targets.vim', event = 'User ActuallyEditing' },
  {
    'nvim-telescope/telescope.nvim',
    dependencies = {
      'nvim-lua/popup.nvim',
      'nvim-lua/plenary.nvim',
      'telescope-frecency.nvim',
      'telescope-fzf-native.nvim',
      'nvim-telescope/telescope-ui-select.nvim',
    },
    init = function()
      require 'config.telescope_setup'
    end,
    config = function()
      require 'config.telescope'
    end,
    cmd = 'Telescope',
  },
  {
    'nvim-telescope/telescope-frecency.nvim',
    dependencies = 'tami5/sqlite.lua',
  },
  {
    'nvim-telescope/telescope-fzf-native.nvim',
    build = 'make',
  },
  'crispgm/telescope-heading.nvim',
  'nvim-telescope/telescope-file-browser.nvim',
  {
    'mbbill/undotree',
    cmd = 'UndotreeToggle',
    init = function()
      vim.g.undotree_SetFocusWhenToggle = 1
    end,
  },
  {
    'lewis6991/gitsigns.nvim',
    dependencies = 'nvim-lua/plenary.nvim',
    config = function()
      require 'config.gitsigns'
    end,
    event = 'User ActuallyEditing',
  },
  {
    'sindrets/diffview.nvim',
    dependencies = 'nvim-lua/plenary.nvim',
  },
  {
    'TimUntersberger/neogit',
    cmd = 'Neogit',
    config = function()
      require 'config.neogit'
    end,
  },
  {
    'akinsho/git-conflict.nvim',
    version = '*',
    config = function()
      require('git-conflict').setup()
    end,
    event = 'BufReadPost',
  },
  {
    'lewis6991/hover.nvim',
    event = 'BufReadPost',
    config = function()
      require('hover').setup {
        init = function()
          require 'hover.providers.lsp'
        end,
      }

      vim.keymap.set('n', 'K', require('hover').hover, { desc = 'hover.nvim' })
      vim.keymap.set('n', 'gK', require('hover').hover_select, { desc = 'hover.nvim (select)' })
    end,
  },
  {
    'DNLHC/glance.nvim',
    cmd = 'Glance',
    config = function()
      require('glance').setup {
        border = { enable = true, top_char = '─', bottom_char = '─' },
        theme = { mode = 'brighten' },
        indent_lines = { icon = '│' },
      }
    end,
  },
  'kyazdani42/nvim-web-devicons',
  'neovim/nvim-lspconfig',
  {
    'smjonas/inc-rename.nvim',
    config = function()
      require('inc_rename').setup()
    end,
    event = 'BufReadPost',
  },
  {
    'folke/trouble.nvim',
    cmd = 'Trouble',
    config = function()
      require('trouble').setup {}
    end,
  },
  'ray-x/lsp_signature.nvim',
  'kosayoda/nvim-lightbulb',
  'p00f/clangd_extensions.nvim',
  {
    'simrat39/rust-tools.nvim',
    ft = 'rust',
    config = function()
      require('rust-tools').setup {}
    end,
  },
  {
    'nvim-treesitter/nvim-treesitter',
    dependencies = {
      'nvim-treesitter/nvim-treesitter-refactor',
      'RRethy/nvim-treesitter-textsubjects',
      'RRethy/nvim-treesitter-endwise',
    },
    build = ':TSUpdate',
    event = 'User ActuallyEditing',
    config = function()
      require 'config.treesitter'
    end,
  },
  {
    'danymat/neogen',
    dependencies = 'nvim-treesitter',
    config = function()
      require 'config.neogen'
    end,
    keys = { '<localleader>d', '<localleader>df', '<localleader>dc' },
  },
  { 'gpanders/nvim-parinfer', ft = { 'lisp', 'fennel', 'clojure', 'racket' } },
  {
    'L3MON4D3/LuaSnip',
  },
  'rafamadriz/friendly-snippets',
  {
    'hrsh7th/nvim-cmp',
    dependencies = {
      'hrsh7th/cmp-buffer',
      'hrsh7th/cmp-nvim-lsp',
      'onsails/lspkind.nvim',
      'hrsh7th/cmp-nvim-lsp-signature-help',
      'hrsh7th/cmp-path',
      'hrsh7th/cmp-nvim-lua',
      'saadparwaiz1/cmp_luasnip',
      'lukas-reineke/cmp-under-comparator',
      'hrsh7th/cmp-cmdline',
      'hrsh7th/cmp-nvim-lsp-document-symbol',
    },
    config = function()
      require 'config.cmp'
    end,
    event = 'InsertEnter',
  },
  {
    'mfussenegger/nvim-dap',
    init = function()
      require 'config.dap_setup'
    end,
    config = function()
      require 'config.dap'
    end,
    dependencies = 'jbyuki/one-small-step-for-vimkind',
    cmd = { 'BreakpointToggle', 'Debug', 'DapREPL' },
  },
  {
    'rcarriga/nvim-dap-ui',
    dependencies = 'nvim-dap',
    config = function()
      require('dapui').setup()
    end,
  },
  {
    'nvim-neo-tree/neo-tree.nvim',
    branch = 'v2.x',
    init = function()
      vim.g.neo_tree_remove_legacy_commands = true
    end,
    cmd = 'Neotree',
    dependencies = {
      'nvim-lua/plenary.nvim',
      'kyazdani42/nvim-web-devicons', -- not strictly required, but recommended
      'MunifTanjim/nui.nvim',
    },
  },
  {
    'lervag/vimtex',
    init = function()
      local g = vim.g
      g.vimtex_complete_recursive_bib = 1
      g.vimtex_complete_enabled = 1
      g.vimtex_quickfix_method = 'pplatex'
      g.tex_conceal = ''
      g.vimtex_quickfix_mode = 0
      g.vimtex_view_forward_search_on_start = 0
      g.vimtex_view_method = 'sioyek'
      g.vimtex_view_general_options = [[--unique file:@pdf\#src:@line@tex]]
      g.vimtex_compiler_latexrun = { options = { '-verbose-cmds', '--latex-args="-synctex=1"', '--bibtex-cmd=biber' } }
      -- This must be a dictionary, and {} gets converted to a list
      g.vimtex_syntax_conceal_disable = 1
    end,
    ft = 'tex',
  },
  'barreiroleo/ltex_extra.nvim',
  'igankevich/mesonic',
  {
    'Shatur/neovim-cmake',
    dependencies = { 'nvim-lua/plenary.nvim', 'mfussenegger/nvim-dap' },
    cmd = 'CMake',
  },
  'PontusPersson/pddl.vim',
  {
    'thibthib18/ros-nvim',
    dependencies = 'nvim-telescope/telescope.nvim',
    config = function()
      require('ros-nvim').setup {
        catkin_ws_path = '~/projects/research/riposte_ws',
        catkin_program = 'catkin build',
      }
    end,
    enabled = false,
  },
  {
    'dstein64/vim-startuptime',
    cmd = 'StartupTime',
    init = function()
      vim.g.startuptime_tries = 15
    end,
  },
  -- {'ThePrimeagen/refactoring.nvim',
  'folke/neodev.nvim',
  {
    'NvChad/nvim-colorizer.lua',
    ft = { 'css', 'javascript', 'vim', 'html', 'lua' },
    config = function()
      require('colorizer').setup {}
    end,
  },
  {
    dir = '~/projects/personal/vim-nazgul',
    config = function()
      require 'config.colorscheme'
    end,
    lazy = false,
  },
  -- 'hardselius/warlock',
  -- 'arzg/vim-substrata',
  -- 'ellisonleao/gruvbox.nvim',
  -- 'RRethy/nvim-base16',

  -- Notes
  -- {
  --   '~/projects/personal/pdf-scribe.nvim',
  --   config = [[require('config.pdf_scribe')]],
  --   disable = true,
  -- }

  -- Buffer management
  -- {
  --   'akinsho/bufferline.nvim',
  --   dependencies = 'kyazdani42/nvim-web-devicons',
  --   config = [[require('config.bufferline')]],
  --   event = 'User ActuallyEditing',
  --   disable = true,
  -- }

  {
    'b0o/incline.nvim',
    config = function()
      require 'config.incline'
    end,
    event = 'User ActuallyEditing',
  },

  {
    'filipdutescu/renamer.nvim',
    branch = 'master',
    dependencies = { { 'nvim-lua/plenary.nvim' } },
  },
  'teal-language/vim-teal',
  { 'jose-elias-alvarez/null-ls.nvim', dependencies = { 'nvim-lua/plenary.nvim', 'neovim/nvim-lspconfig' } },
  {
    'simrat39/symbols-outline.nvim',
    event = 'BufReadPost',
    config = function()
      require('symbols-outline').setup()
    end,
  },
  'stevearc/dressing.nvim',
  'rcarriga/nvim-notify',
  'vigoux/notifier.nvim',
  {
    'folke/todo-comments.nvim',
    dependencies = 'nvim-lua/plenary.nvim',
    event = 'BufReadPost',
    config = function()
      require('todo-comments').setup {}
    end,
  },
  {
    'j-hui/fidget.nvim',
    event = 'User ActuallyEditing',
    config = function()
      require('fidget').setup {
        sources = {
          ['null-ls'] = { ignore = true },
        },
      }
    end,
    enabled = false,
  },
  {
    'ethanholz/nvim-lastplace',
    config = function()
      require('nvim-lastplace').setup {}
      vim.api.nvim_exec_autocmds('BufWinEnter', { group = 'NvimLastplace' })
    end,
    event = 'User ActuallyEditing',
  },
  {
    'voldikss/vim-floaterm',
    init = function()
      vim.keymap.set('n', '<f7>', '<cmd>FloatermToggle<cr>')
      vim.keymap.set('t', '<f7>', '<c-\\><c-n>:FloatermToggle<cr>')
      vim.keymap.set('t', 'jj', '<c-\\><c-n>')
    end,
    enabled = false,
    cmd = {
      'FloatermNew',
      'FloatermToggle',
    },
  },
  {
    'akinsho/toggleterm.nvim',
    version = '*',
    config = function()
      require('toggleterm').setup { open_mapping = [[<c-\>]] }
    end,
    keys = [[<c-\>/]],
  },
  {
    'beauwilliams/focus.nvim',
    config = function()
      require('focus').setup { excluded_filetypes = { 'toggleterm', 'TelescopePrompt' }, signcolumn = false }
    end,
    event = 'User ActuallyEditing',
  },
  {
    'hkupty/iron.nvim',
    cmd = { 'IronRepl', 'IronFocus' },
    init = function()
      vim.keymap.set('n', '<leader>rs', '<cmd>IronRepl<cr>')
      vim.keymap.set('n', '<leader>rr', '<cmd>IronRestart<cr>')
      vim.keymap.set('n', '<leader>rf', '<cmd>IronFocus<cr>')
      vim.keymap.set('n', '<leader>rh', '<cmd>IronHide<cr>')
    end,
    dependencies = 'which-key.nvim',
    config = function()
      require('iron.core').setup {
        config = {
          repl_open_cmd = require('iron.view').right '40%',
          repl_definition = {
            python = require('iron.fts.python').ptipython,
            ocaml = require('iron.fts.ocaml').utop,
            lua = { command = 'croissant' },
          },
          highlight = { italic = true },
        },
        keymaps = {
          send_motion = '<c-c>',
          visual_send = '<c-cr>',
          send_file = '<leader>rsf',
          send_line = '<c-cr>',
          send_mark = '<leader>rsm',
          mark_motion = '<leader>rmc',
          mark_visual = '<leader>rmc',
          remove_mark = '<leader>rmd',
          cr = '<leader>r<cr>',
          interrupt = '<leader>ri<leader>',
          exit = '<leader>rq',
          clear = '<leader>rc',
        },
      }
    end,
  },
  {
    'folke/noice.nvim',
    config = function()
      require('noice').setup {
        routes = {
          {
            filter = {
              event = 'msg_show',
              kind = '',
              find = 'written',
            },
            opts = { skip = true },
          },
          {
            filter = {
              event = 'msg_show',
              find = '%d+L, %d+B',
            },
            view = 'mini',
          },
        },
        lsp = {
          override = {
            ['vim.lsp.util.convert_input_to_markdown_lines'] = true,
            ['vim.lsp.util.stylize_markdown'] = true,
            ['cmp.entry.get_documentation'] = true,
          },
        },
        presets = {
          bottom_search = true,
          command_palette = true,
          long_message_to_split = true,
          inc_rename = true,
          lsp_doc_border = true,
        },
      }
    end,
    dependencies = { 'MunifTanjim/nui.nvim', 'rcarriga/nvim-notify' },
    event = 'VeryLazy',
  },
}

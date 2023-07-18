return {
  {
    'ojroques/nvim-bufdel',
    cmd = 'BufDel',
    opts = {},
  },
  {
    'folke/which-key.nvim',
    opts = {},
    event = 'BufReadPost',
  },
  { 'chaoren/vim-wordmotion', event = 'VeryLazy' },
  {
    'ggandor/leap.nvim',
    event = 'VeryLazy',
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
    opts = { labeled_modes = 'nv' },
    event = 'VeryLazy',
  },
  { 'Olical/vim-enmasse', cmd = 'EnMasse' },
  {
    'kevinhwang91/nvim-bqf',
    ft = 'qf',
  },
  {
    'echasnovski/mini.nvim',
    version = false,
    event = 'User ActuallyEditing',
    config = function()
      require('mini.surround').setup { search_method = 'cover_or_nearest' }
      require('mini.align').setup { mappings = { start = '', start_with_preview = 'g=' } }
      require('mini.ai').setup { search_method = 'cover_or_nearest' }
      require('mini.bracketed').setup {}
      require('mini.comment').setup { options = { ignore_blank_line = true } }
      require('mini.indentscope').setup {
        symbol = '│',
        options = { try_as_border = true },
        draw = { animation = require('mini.indentscope').gen_animation.none() },
      }
      require('mini.move').setup {}
      require('mini.splitjoin').setup { mappings = { toggle = 'gJ' } }
    end,
  },
  {
    'windwp/nvim-autopairs',
    opts = {
      enable_check_bracket_line = false,
      ignored_next_char = '[%w%.]',
      fast_wrap = {},
    },
    event = 'BufReadPost',
  },
  {
    'andymass/vim-matchup',
    init = function()
      require 'config.matchup'
    end,
    event = 'User ActuallyEditing',
  },
  { 'romainl/vim-cool', event = 'VeryLazy' },
  {
    'nvim-telescope/telescope.nvim',
    dependencies = {
      'nvim-lua/popup.nvim',
      'nvim-lua/plenary.nvim',
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
    event = 'BufReadPost',
  },
  {
    'sindrets/diffview.nvim',
    dependencies = 'nvim-lua/plenary.nvim',
  },
  {
    'NeogitOrg/neogit',
    cmd = 'Neogit',
    config = function()
      require 'config.neogit'
    end,
  },
  {
    'akinsho/git-conflict.nvim',
    version = '*',
    opts = {},
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
        detached = true,
        border = { enable = true, top_char = '─', bottom_char = '─' },
        theme = { mode = 'brighten' },
        indent_lines = { icon = '│' },
        winbar = { enable = true },
      }
    end,
  },
  'nvim-tree/nvim-web-devicons',
  'neovim/nvim-lspconfig',
  {
    'smjonas/inc-rename.nvim',
    opts = {},
    event = 'BufReadPost',
  },
  {
    'folke/trouble.nvim',
    cmd = 'Trouble',
    opts = {},
  },
  'p00f/clangd_extensions.nvim',
  {
    'simrat39/rust-tools.nvim',
    ft = 'rust',
    opts = { tools = { inlay_hints = { auto = false } } },
  },
  {
    'nvim-treesitter/nvim-treesitter',
    dependencies = {
      'nvim-treesitter/nvim-treesitter-refactor',
      'RRethy/nvim-treesitter-textsubjects',
      'RRethy/nvim-treesitter-endwise',
      'windwp/nvim-ts-autotag',
    },
    build = ':TSUpdate',
    event = 'BufReadPost',
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
  { 'gpanders/nvim-parinfer', ft = { 'lisp', 'fennel', 'clojure', 'racket', 'pddl' } },
  'L3MON4D3/LuaSnip',
  { 'rafamadriz/friendly-snippets', lazy = false },
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
      'doxnit/cmp-luasnip-choice',
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
    opts = {},
  },
  {
    'nvim-neo-tree/neo-tree.nvim',
    branch = 'v3.x',
    init = function()
      vim.g.neo_tree_remove_legacy_commands = true
    end,
    cmd = 'Neotree',
    event = 'User EditingDirectory',
    config = function()
      require 'config.neotree'
    end,
    dependencies = {
      'nvim-lua/plenary.nvim',
      'nvim-tree/nvim-web-devicons',
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
      -- g.vimtex_view_general_options = [[--unique file:@pdf\#src:@line@tex]]
      -- g.vimtex_compiler_latexrun = { options = { '-verbose-cmds', '--latex-args="-synctex=1"', '--bibtex-cmd=biber' } }
      -- This must be a dictionary, and {} gets converted to a list
      g.vimtex_syntax_conceal_disable = 1
    end,
    -- ft = 'tex',
    lazy = false,
  },
  'barreiroleo/ltex_extra.nvim',
  'igankevich/mesonic',
  {
    'Shatur/neovim-cmake',
    dependencies = { 'nvim-lua/plenary.nvim', 'mfussenegger/nvim-dap' },
    cmd = 'CMake',
  },
  { 'PontusPersson/pddl.vim', lazy = false },
  {
    'thibthib18/ros-nvim',
    dependencies = 'nvim-telescope/telescope.nvim',
    opts = {
      catkin_ws_path = '~/projects/research/riposte_ws',
      catkin_program = 'catkin build',
    },
    enabled = false,
  },
  -- {'ThePrimeagen/refactoring.nvim',
  'folke/neodev.nvim',
  {
    'NvChad/nvim-colorizer.lua',
    ft = { 'css', 'javascript', 'vim', 'html', 'lua' },
    opts = {},
  },
  {
    dir = '~/projects/personal/vim-nazgul',
  },
  -- 'hardselius/warlock',
  'arzg/vim-substrata',
  {
    'mcchrish/zenbones.nvim',
    dependencies = 'rktjmp/lush.nvim',
  },
  -- 'ellisonleao/gruvbox.nvim',
  -- 'RRethy/nvim-base16',
  {
    'utilyre/barbecue.nvim',
    event = 'User ActuallyEditing',
    name = 'barbecue',
    version = '*',
    dependencies = {
      'SmiteshP/nvim-navic',
      'nvim-tree/nvim-web-devicons',
    },
    opts = {
      create_autocmd = false,
      attach_navic = false,
      show_modified = true,
      exclude_filetypes = { 'netrw', 'toggleterm', 'NeogitCommitMessage' },
      custom_section = function()
        -- Copied from @akinsho's config
        local error_icon = '' -- '✗'
        local warning_icon = ''
        local info_icon = '' --  
        local hint_icon = '⚑' --  ⚑
        local errors = #vim.diagnostic.get(0, { severity = vim.diagnostic.severity.ERROR })
        local warnings = #vim.diagnostic.get(0, { severity = vim.diagnostic.severity.WARN })
        local hints = #vim.diagnostic.get(0, { severity = vim.diagnostic.severity.HINT })
        local info = #vim.diagnostic.get(0, { severity = vim.diagnostic.severity.INFO })
        local components = {}
        if errors > 0 then
          components[#components + 1] = { error_icon .. ' ' .. errors, 'DiagnosticError' }
        end

        if warnings > 0 then
          components[#components + 1] =
            { (#components > 0 and ' ' or '') .. warning_icon .. ' ' .. warnings, 'DiagnosticWarning' }
        end

        if hints > 0 then
          components[#components + 1] =
            { (#components > 0 and ' ' or '') .. hint_icon .. ' ' .. hints, 'DiagnosticHint' }
        end

        if info > 0 then
          components[#components + 1] =
            { (#components > 0 and ' ' or '') .. info_icon .. ' ' .. info, 'DiagnosticInfo' }
        end

        return components
      end,
    },
  },
  'teal-language/vim-teal',
  'jose-elias-alvarez/null-ls.nvim',
  {
    'stevearc/aerial.nvim',
    opts = {
      backends = { 'lsp', 'treesitter', 'markdown', 'man' },
      on_attach = function(bufnr)
        vim.keymap.set('n', '{', '<cmd>AerialPrev<CR>', { buffer = bufnr })
        vim.keymap.set('n', '}', '<cmd>AerialNext<CR>', { buffer = bufnr })
      end,
    },
    cmd = { 'AerialOpen', 'AerialToggle' },
  },
  {
    'folke/todo-comments.nvim',
    dependencies = 'nvim-lua/plenary.nvim',
    event = 'BufReadPost',
    opts = { colors = { info = { 'WhiteHover' } } },
  },
  {
    'ethanholz/nvim-lastplace',
    opts = {
      lastplace_ignore_buftype = { 'quickfix', 'nofile', 'help' },
      lastplace_ignore_filetype = {
        'gitcommit',
        'gitrebase',
        'svn',
        'hgcommit',
      },
    },
    event = 'BufReadPre',
    priority = 1001,
  },
  {
    'akinsho/toggleterm.nvim',
    version = '*',
    opts = { open_mapping = [[<c-\>]], direction = 'float' },
    keys = [[<c-\>]],
  },
  {
    'willothy/flatten.nvim',
    opts = {
      window = { open = 'alternate' },
      post_open = function(_bufnr, winnr, _ft, is_blocking)
        if is_blocking then
          require('toggleterm').toggle(0)
        else
          vim.api.nvim_set_current_win(winnr)
        end
      end,
    },
    event = 'TermOpen',
  },
  {
    'beauwilliams/focus.nvim',
    opts = { excluded_filetypes = { 'toggleterm', 'TelescopePrompt' }, signcolumn = false },
    event = 'VeryLazy',
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
    opts = {
      views = { mini = { timeout = 10000, lang = 'markdown' } },
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
            event = 'lsp',
            kind = 'progress',
            find = 'null-l',
          },
          opts = { skip = true, stop = true },
        },
        {
          view = 'notify',
          filter = { event = 'msg_showmode' },
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
    },
    dependencies = { 'MunifTanjim/nui.nvim' },
    event = 'VeryLazy',
  },
  {
    'SmiteshP/nvim-navic',
    dependencies = 'neovim/nvim-lspconfig',
    opts = { lazy_update_context = true },
  },
  {
    'stevearc/overseer.nvim',
    config = true,
    cmd = { 'OverseerRun', 'OverseerToggle' },
  },
  {
    'chrisgrieser/nvim-various-textobjs',
    opts = { useDefaultKeymaps = true },
  },
  {
    'folke/persistence.nvim',
    event = 'BufReadPre',
    module = 'persistence',
    config = function()
      require('persistence').setup {
        dir = vim.fn.expand(vim.fn.stdpath 'config' .. '/session/'),
        options = { 'buffers', 'curdir', 'tabpages', 'winsize' },
      }
    end,
  },
  {
    'nanozuki/tabby.nvim',
    event = 'User ActuallyEditing',
    config = function()
      local theme = {
        fill = { fg = '#222222', bg = '#222222' },
        current_tab = { fg = '#e9e9e9', bg = '#222222' },
        tab = { fg = '#666666', bg = '#222222' },
      }
      local tabby_api = require 'tabby.module.api'
      require('tabby.tabline').set(function(line)
        local num_tabs = #tabby_api.get_tabs()
        local tabs = line.tabs().foreach(function(tab)
          local hl = tab.is_current() and theme.current_tab or theme.tab
          local tab_separator = (tab.number() < num_tabs)
              and line.sep(
                '／ ',
                { fg = '#222222', bg = '#e9e9e9', style = 'bold' },
                { fg = '#e9e9e9', bg = '#222222', style = 'bold' }
              )
            or line.sep(
              '',
              { fg = '#222222', bg = '#e9e9e9', style = 'bold' },
              { fg = '#e9e9e9', bg = '#222222', style = 'bold' }
            )
          local num_wins = #tabby_api.get_tab_wins(tab.id)
          local win_idx = 1
          return {
            tab.wins().foreach(function(win)
              local window_separator = (win_idx < num_wins) and '  ' or ''
              win_idx = win_idx + 1
              return {
                win.file_icon(),
                win.buf_name(),
                win.buf().is_changed() and '●' or '',
                window_separator,
                margin = ' ',
                hl = hl,
              }
            end),
            tab_separator,
            hl = hl,
          }
        end)
        return {
          line.spacer(),
          tabs,
          line.spacer(),
          buf_name = { mode = 'unique' },
          hl = theme.fill,
        }
      end)
    end,
  },
}

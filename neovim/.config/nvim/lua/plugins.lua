local packer = nil
local function init()
  if packer == nil then
    packer = require 'packer'
    packer.init { disable_commands = true, display = {
      open_fn = require('packer.util').float,
    } }
  end

  local use = packer.use
  packer.reset()

  -- Packer
  use '~/projects/personal/packer.nvim'

  use 'lewis6991/impatient.nvim'

  -- Async building & commands
  -- use { 'tpope/vim-dispatch', cmd = { 'Dispatch', 'Make', 'Focus', 'Start' } }
  use 'mhinz/vim-sayonara'

  -- Marks
  -- use { 'kshenoy/vim-signature', config = [[require('config.signature')]] }

  use { 'tversteeg/registers.nvim', keys = { { 'n', '"' }, { 'i', '<c-r>' } } }

  -- Movement
  use 'chaoren/vim-wordmotion'
  use {
    {
      'ggandor/leap.nvim',
      requires = 'tpope/vim-repeat',
    },
    { 'ggandor/flit.nvim', config = [[require'flit'.setup{}]] },
  }

  -- Quickfix
  use { 'Olical/vim-enmasse', cmd = 'EnMasse' }
  use 'kevinhwang91/nvim-bqf'
  use {
    'https://gitlab.com/yorickpeterse/nvim-pqf',
    config = function()
      require('pqf').setup()
    end,
  }

  -- Indentation tracking
  use 'lukas-reineke/indent-blankline.nvim'

  -- Commenting
  -- use 'tomtom/tcomment_vim'
  use {
    'numToStr/Comment.nvim',
    event = 'User ActuallyEditing',
    config = function()
      require('Comment').setup {}
    end,
  }

  -- Wrapping/delimiters
  use {
    { 'machakann/vim-sandwich', event = 'User ActuallyEditing' },
    { 'andymass/vim-matchup', setup = [[require('config.matchup')]], event = 'User ActuallyEditing' },
  }

  -- Search
  use 'romainl/vim-cool'

  -- Prettification
  use 'junegunn/vim-easy-align'

  -- Text objects
  use 'wellle/targets.vim'

  -- Search
  use {
    {
      'nvim-telescope/telescope.nvim',
      requires = {
        'nvim-lua/popup.nvim',
        'nvim-lua/plenary.nvim',
        'telescope-frecency.nvim',
        'telescope-fzf-native.nvim',
        'nvim-telescope/telescope-ui-select.nvim',
      },
      wants = {
        'popup.nvim',
        'plenary.nvim',
        'telescope-frecency.nvim',
        'telescope-fzf-native.nvim',
      },
      setup = [[require('config.telescope_setup')]],
      config = [[require('config.telescope')]],
      cmd = 'Telescope',
      module = 'telescope',
    },
    {
      'nvim-telescope/telescope-frecency.nvim',
      after = 'telescope.nvim',
      requires = 'tami5/sqlite.lua',
    },
    {
      'nvim-telescope/telescope-fzf-native.nvim',
      run = 'make',
    },
    'crispgm/telescope-heading.nvim',
    'nvim-telescope/telescope-file-browser.nvim',
  }

  -- Project Management/Sessions
  use {
    'dhruvasagar/vim-prosession',
    after = 'vim-obsession',
    requires = { 'tpope/vim-obsession', cmd = 'Prosession' },
    config = [[require('config.prosession')]],
  }

  -- Undo tree
  use {
    'mbbill/undotree',
    cmd = 'UndotreeToggle',
    config = [[vim.g.undotree_SetFocusWhenToggle = 1]],
  }

  -- Git
  use {
    {
      'lewis6991/gitsigns.nvim',
      requires = 'nvim-lua/plenary.nvim',
      config = [[require('config.gitsigns')]],
      event = 'User ActuallyEditing',
    },
    { 'TimUntersberger/neogit', cmd = 'Neogit', config = [[require('config.neogit')]] },
    {
      'akinsho/git-conflict.nvim',
      tag = '*',
      config = function()
        require('git-conflict').setup()
      end,
    },
  }

  -- Pretty symbols
  use 'kyazdani42/nvim-web-devicons'

  -- REPLs
  use {
    'hkupty/iron.nvim',
    setup = [[vim.g.iron_map_defaults = 0]],
    config = [[require('config.iron')]],
    cmd = { 'IronRepl', 'IronSend', 'IronReplHere' },
  }

  -- Completion and linting
  use {
    'neovim/nvim-lspconfig',
    { '~/projects/personal/lsp-status.nvim', disable = true },
    'folke/trouble.nvim',
    'ray-x/lsp_signature.nvim',
    {
      'kosayoda/nvim-lightbulb',
      requires = 'antoinemadec/FixCursorHold.nvim',
    },
  }

  -- C++
  use 'p00f/clangd_extensions.nvim'

  -- Rust
  use {
    'simrat39/rust-tools.nvim',
    config = function()
      require('rust-tools').setup {}
    end,
  }

  -- Highlights
  use {
    'nvim-treesitter/nvim-treesitter',
    requires = {
      'nvim-treesitter/nvim-treesitter-refactor',
      'RRethy/nvim-treesitter-textsubjects',
    },
    run = ':TSUpdate',
  }

  -- Documentation
  use {
    'danymat/neogen',
    requires = 'nvim-treesitter',
    config = [[require('config.neogen')]],
    keys = { '<localleader>d', '<localleader>df', '<localleader>dc' },
  }

  -- Lisps
  use 'gpanders/nvim-parinfer'

  -- Snippets
  use {
    {
      'L3MON4D3/LuaSnip',
      event = 'InsertEnter',
      config = function()
        require('luasnip.loaders.from_vscode').lazy_load()
      end,
    },
    'rafamadriz/friendly-snippets',
  }

  -- Completion
  use {
    'hrsh7th/nvim-cmp',
    requires = {
      { 'hrsh7th/cmp-buffer', after = 'nvim-cmp' },
      'hrsh7th/cmp-nvim-lsp',
      { 'hrsh7th/cmp-nvim-lsp-signature-help', after = 'nvim-cmp' },
      { 'hrsh7th/cmp-path', after = 'nvim-cmp' },
      { 'hrsh7th/cmp-nvim-lua', after = 'nvim-cmp' },
      { 'saadparwaiz1/cmp_luasnip', after = 'nvim-cmp' },
      'lukas-reineke/cmp-under-comparator',
      'hrsh7th/cmp-cmdline',
      { 'hrsh7th/cmp-nvim-lsp-document-symbol', after = 'nvim-cmp' },
    },
    config = [[require('config.cmp')]],
    event = 'InsertEnter',
    after = 'LuaSnip',
  }

  -- Endwise
  use 'RRethy/nvim-treesitter-endwise'

  -- Debugger
  use {
    {
      'mfussenegger/nvim-dap',
      setup = [[require('config.dap_setup')]],
      config = [[require('config.dap')]],
      requires = 'jbyuki/one-small-step-for-vimkind',
      wants = 'one-small-step-for-vimkind',
      cmd = { 'BreakpointToggle', 'Debug', 'DapREPL' },
    },
    {
      'rcarriga/nvim-dap-ui',
      requires = 'nvim-dap',
      wants = 'nvim-dap',
      after = 'nvim-dap',
      config = function()
        require('dapui').setup()
      end,
    },
  }

  -- Path navigation
  use 'justinmk/vim-dirvish'
  use {
    'nvim-neo-tree/neo-tree.nvim',
    branch = 'v2.x',
    config = [[vim.g.neo_tree_remove_legacy_commands = true]],
    requires = {
      'nvim-lua/plenary.nvim',
      'kyazdani42/nvim-web-devicons', -- not strictly required, but recommended
      'MunifTanjim/nui.nvim',
    },
  }

  -- LaTeX
  use 'lervag/vimtex'
  use 'barreiroleo/ltex_extra.nvim'

  -- Meson
  use 'igankevich/mesonic'

  -- CMake
  use {
    'Shatur/neovim-cmake',
    requires = { 'nvim-lua/plenary.nvim', 'mfussenegger/nvim-dap' },
    wants = 'plenary.nvim',
    cmd = 'CMake',
  }

  -- PDDL
  use 'PontusPersson/pddl.vim'

  -- ROS
  use {
    'thibthib18/ros-nvim',
    requires = 'nvim-telescope/telescope.nvim',
    wants = 'telescope.nvim',
    config = function()
      require('ros-nvim').setup {
        catkin_ws_path = '~/projects/research/riposte_ws',
        catkin_program = 'catkin build',
      }
    end,
    module = 'ros-nvim',
  }

  -- Zig
  use 'ziglang/zig.vim'

  -- Julia
  use { 'JuliaEditorSupport/julia-vim', setup = [[vim.g.latex_to_unicode_tab = 'off']], opt = true }

  -- Profiling
  use { 'dstein64/vim-startuptime', cmd = 'StartupTime', config = [[vim.g.startuptime_tries = 10]] }

  -- Refactoring
  use 'ThePrimeagen/refactoring.nvim'

  -- Plugin development
  use 'folke/lua-dev.nvim'

  -- Highlight colors
  use {
    'norcalli/nvim-colorizer.lua',
    ft = { 'css', 'javascript', 'vim', 'html' },
    config = [[require('colorizer').setup {'css', 'javascript', 'vim', 'html'}]],
  }

  -- Color scheme
  use '~/projects/personal/vim-nazgul'
  use 'hardselius/warlock'
  use 'arzg/vim-substrata'
  use 'sainnhe/gruvbox-material'
  use 'RRethy/nvim-base16'

  -- Notes
  use {
    '~/projects/personal/pdf-scribe.nvim',
    config = [[require('config.pdf_scribe')]],
    disable = true,
  }

  -- Buffer management
  use {
    'akinsho/bufferline.nvim',
    requires = 'kyazdani42/nvim-web-devicons',
    config = [[require('config.bufferline')]],
    event = 'User ActuallyEditing',
  }

  use 'b0o/incline.nvim'
  use 'teal-language/vim-teal'
  use { 'jose-elias-alvarez/null-ls.nvim', requires = { 'nvim-lua/plenary.nvim', 'neovim/nvim-lspconfig' } }

  -- Pretty UI
  use 'stevearc/dressing.nvim'
  use 'rcarriga/nvim-notify'
  use {
    'B4mbus/todo-comments.nvim',
    requires = 'nvim-lua/plenary.nvim',
    config = function()
      require('todo-comments').setup {}
    end,
  }

  use {
    'j-hui/fidget.nvim',
    config = function()
      require('fidget').setup {
        sources = {
          ['null-ls'] = { ignore = true },
        },
      }
    end,
  }

  use {
    'ethanholz/nvim-lastplace',
    config = function()
      require('nvim-lastplace').setup {}
    end,
  }
end

local plugins = setmetatable({}, {
  __index = function(_, key)
    init()
    return packer[key]
  end,
})

return plugins

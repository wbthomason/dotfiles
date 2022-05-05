local packer = nil
local function init()
  if packer == nil then
    packer = require 'packer'
    packer.init { disable_commands = true }
  end

  local use = packer.use
  packer.reset()

  -- Packer
  use '~/projects/personal/packer.nvim'

  use 'lewis6991/impatient.nvim'

  -- Async building & commands
  use { 'tpope/vim-dispatch', cmd = { 'Dispatch', 'Make', 'Focus', 'Start' } }

  -- Registers
  -- use 'junegunn/vim-peekaboo'

  use 'mhinz/vim-sayonara'

  -- Marks
  use { 'kshenoy/vim-signature', config = [[require('config.signature')]], disable = true }

  use { 'tversteeg/registers.nvim', keys = { { 'n', '"' }, { 'i', '<c-r>' } } }

  -- Movement
  use { 'chaoren/vim-wordmotion', 'justinmk/vim-sneak' }

  -- Quickfix
  use { 'Olical/vim-enmasse', cmd = 'EnMasse' }
  use 'kevinhwang91/nvim-bqf'

  -- Indentation tracking
  use 'lukas-reineke/indent-blankline.nvim'

  -- Commenting
  -- use 'tomtom/tcomment_vim'
  use {
    'numToStr/Comment.nvim',
    config = function()
      require('Comment').setup()
    end,
  }

  -- Wrapping/delimiters
  use {
    'machakann/vim-sandwich',
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
  }

  -- Project Management/Sessions
  use {
    'dhruvasagar/vim-prosession',
    after = 'vim-obsession',
    requires = { { 'tpope/vim-obsession', cmd = 'Prosession' } },
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
    { 'tpope/vim-fugitive', cmd = { 'Git', 'Gstatus', 'Gblame', 'Gpush', 'Gpull' }, disable = true },
    {
      'lewis6991/gitsigns.nvim',
      requires = { 'nvim-lua/plenary.nvim' },
      config = [[require('config.gitsigns')]],
    },
    { 'TimUntersberger/neogit', cmd = 'Neogit', config = [[require('config.neogit')]] },
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
    '~/projects/personal/lsp-status.nvim',
    'folke/trouble.nvim',
    'ray-x/lsp_signature.nvim',
    'kosayoda/nvim-lightbulb',
  }

  -- C++
  use 'p00f/clangd_extensions.nvim'

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

  -- Completion
  use {
    'hrsh7th/nvim-cmp',
    requires = {
      'L3MON4D3/LuaSnip',
      { 'hrsh7th/cmp-buffer', after = 'nvim-cmp' },
      'hrsh7th/cmp-nvim-lsp',
      'hrsh7th/cmp-nvim-lsp-signature-help',
      { 'hrsh7th/cmp-path', after = 'nvim-cmp' },
      { 'hrsh7th/cmp-nvim-lua', after = 'nvim-cmp' },
      { 'saadparwaiz1/cmp_luasnip', after = 'nvim-cmp' },
      'lukas-reineke/cmp-under-comparator',
      { 'hrsh7th/cmp-nvim-lsp-document-symbol', after = 'nvim-cmp' },
    },
    config = [[require('config.cmp')]],
    event = 'InsertEnter *',
  }

  -- Endwise
  -- use 'tpope/vim-endwise'
  use 'RRethy/nvim-treesitter-endwise'

  -- Debugger
  use {
    {
      'mfussenegger/nvim-dap',
      setup = [[require('config.dap_setup')]],
      config = [[require('config.dap')]],
      requires = 'jbyuki/one-small-step-for-vimkind',
      wants = 'one-small-step-for-vimkind',
      module = 'dap',
    },
    {
      'rcarriga/nvim-dap-ui',
      requires = 'nvim-dap',
      after = 'nvim-dap',
      config = function()
        require('dapui').setup()
      end,
    },
  }

  use {
    'puremourning/vimspector',
    setup = [[vim.g.vimspector_enable_mappings = 'HUMAN']],
    disable = true,
  }

  -- Path navigation
  use 'justinmk/vim-dirvish'

  -- LaTeX
  use 'lervag/vimtex'

  -- Meson
  use 'igankevich/mesonic'

  -- PDDL
  use 'PontusPersson/pddl.vim'

  -- Zig
  use 'ziglang/zig.vim'

  -- Julia
  use { 'JuliaEditorSupport/julia-vim', setup = [[vim.g.latex_to_unicode_tab = 'off']], opt = true }

  -- Profiling
  use { 'dstein64/vim-startuptime', cmd = 'StartupTime', config = [[vim.g.startuptime_tries = 10]] }

  -- Refactoring
  use { 'ThePrimeagen/refactoring.nvim', opt = true }
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

  -- Notes
  use {
    '~/projects/personal/pdf-scribe.nvim',
    config = [[require('config.pdf_scribe')]],
    disable = true,
  }

  use { { 'nvim-orgmode/orgmode.nvim', config = [[require('config.orgmode')]] }, 'akinsho/org-bullets.nvim' }

  -- Buffer management
  use {
    'akinsho/bufferline.nvim',
    requires = 'kyazdani42/nvim-web-devicons',
    config = [[require('config.bufferline')]],
    event = 'User ActuallyEditing',
  }

  use {
    'lukas-reineke/headlines.nvim',
    config = function()
      require('headlines').setup()
    end,
  }

  -- use 'https://github.com/numToStr/Comment.nvim'

  use 'teal-language/vim-teal'
  use { 'jose-elias-alvarez/null-ls.nvim', requires = { 'nvim-lua/plenary.nvim', 'neovim/nvim-lspconfig' } }
  -- use {
  --   'AckslD/nvim-neoclip.lua',
  --   event = 'TextYankPost',
  --   config = function()
  --     local count = 0
  --     _G.test_handler = function()
  --       count = count + 1
  --       print('called', count, 'times')
  --     end
  --     vim.cmd [[autocmd TextYankPost * lua _G.test_handler()]]
  --   end,
  -- }
end

local plugins = setmetatable({}, {
  __index = function(_, key)
    init()
    return packer[key]
  end,
})

return plugins

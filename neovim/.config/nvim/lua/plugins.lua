local packer = nil
local function init()
  if packer == nil then
    packer = require('packer')
    packer.init({disable_commands = true})
  end

  local use = packer.use
  local use_rocks = packer.use_rocks
  packer.reset()

  -- Packer
  use {'~/projects/personal/packer.nvim', opt = true}

  -- Async building & commands
  use {'tpope/vim-dispatch', cmd = {'Dispatch', 'Make', 'Focus', 'Start'}}

  -- Auto-pairs
  -- use 'cohama/lexima.vim'

  -- Registers
  use 'junegunn/vim-peekaboo'

  -- Marks
  use 'kshenoy/vim-signature'

  -- Buffer management
  use {'mhinz/vim-sayonara', cmd = 'Sayonara'}

  -- Movement
  use {'chaoren/vim-wordmotion', 'justinmk/vim-sneak'}
  -- use 'unblevable/quick-scope'

  -- Quickfix
  use {'Olical/vim-enmasse', cmd = 'EnMasse'}

  -- Grepping
  use {'mhinz/vim-grepper', cmd = 'Grepper'}

  -- Indentation tracking
  use 'yggdroot/indentLine'

  -- Commenting
  use 'tomtom/tcomment_vim'
  -- use 'tyru/caw.vim'

  -- Wrapping/delimiters
  use {'machakann/vim-sandwich', {'andymass/vim-matchup', event = 'VimEnter *'}}

  -- Search
  use 'romainl/vim-cool'

  -- Pattern preview
  -- use { 'markonm/traces.vim' }

  -- Prettification
  use 'junegunn/vim-easy-align'
  use {'mhartington/formatter.nvim', config = require('format')}

  -- Text objects
  use 'wellle/targets.vim'

  -- Search
  use {'nvim-lua/telescope.nvim', requires = {'nvim-lua/popup.nvim', 'nvim-lua/plenary.nvim'}}

  -- Project Management/Sessions
  use {
    'dhruvasagar/vim-prosession',
    after = 'vim-obsession',
    requires = {{'tpope/vim-obsession', cmd = 'Prosession'}}
  }

  -- REPL
  -- use {'hkupty/iron.nvim', cmd = {'IronRepl', 'IronWatchCurrentFile', 'IronSend'}}

  -- Undo tree
  use {'mbbill/undotree', cmd = 'UndotreeToggle'}

  -- Git
  use {'mhinz/vim-signify', {'tpope/vim-fugitive', cmd = {'Gblame', 'Gpull', 'Gpush', 'Gstatus'}}}

  -- Terminal
  use 'voldikss/vim-floaterm'

  -- Completion and linting
  use {
    'onsails/lspkind-nvim', 'neovim/nvim-lspconfig', '~/projects/personal/lsp-status.nvim', {
      'nvim-treesitter/nvim-treesitter',
      requires = {
        {'nvim-treesitter/nvim-treesitter-refactor', after = 'nvim-treesitter'},
        {'nvim-treesitter/nvim-treesitter-textobjects', after = 'nvim-treesitter'}
      },
      config = require('treesitter'),
      event = 'VimEnter *'
    }
  }

  use {'hrsh7th/nvim-compe', config = function() require('config.compe') end}
  use 'hrsh7th/vim-vsnip'

  -- use '~/projects/personal/hover.nvim'

  -- Debugger
  use {'mfussenegger/nvim-dap', opt = true}
  use {
    'puremourning/vimspector',
    setup = function() vim.g.vimspector_enable_mappings = 'HUMAN' end,
    opt = true
  }

  -- Better Lua highlighting
  -- use 'euclidianAce/BetterLua.vim'

  -- Path navigation
  use 'justinmk/vim-dirvish'

  -- LaTeX
  use 'lervag/vimtex'

  -- Meson
  use 'igankevich/mesonic'

  -- TOML
  use 'cespare/vim-toml'

  -- Julia
  use 'JuliaEditorSupport/julia-vim'

  -- PDDL
  use 'PontusPersson/pddl.vim'

  -- Profiling
  -- use {'dstein64/vim-startuptime', cmd = 'StartupTime'}
  use {'tweekmonster/startuptime.vim', cmd = 'StartupTime'}

  -- Highlight colors
  use 'norcalli/nvim-colorizer.lua'

  -- Color scheme
  use '~/projects/personal/vim-nazgul'
  -- use 'hardselius/warlock'
  -- use 'arzg/vim-substrata'

  -- Notes
  use '~/projects/personal/pdf-scribe.nvim'
  -- use {
  --   'oberblastmeister/neuron.nvim',
  --   event = 'VimEnter *',
  --   config = function() require('neuron').setup {neuron_dir = '~/gdrive/notes/neuron'} end
  -- }

end

local plugins = setmetatable({}, {
  __index = function(_, key)
    init()
    return packer[key]
  end
})

return plugins

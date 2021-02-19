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

  -- Registers
  use 'junegunn/vim-peekaboo'

  -- Marks
  use {'kshenoy/vim-signature', config = [[require('config.signature')]], disable = true}

  -- Buffer management
  use {'mhinz/vim-sayonara', cmd = 'Sayonara'}

  -- Movement
  use {'chaoren/vim-wordmotion', {'justinmk/vim-sneak', config = [[require('config.sneak')]]}}

  -- Quickfix
  use {'Olical/vim-enmasse', cmd = 'EnMasse'}

  -- Indentation tracking
  use {'yggdroot/indentLine', setup = [[require('config.indentline')]]}

  -- Commenting
  use 'tomtom/tcomment_vim'

  -- Wrapping/delimiters
  use {'machakann/vim-sandwich', {'andymass/vim-matchup', setup = [[require('config.matchup')]]}}

  -- Search
  use 'romainl/vim-cool'

  -- Prettification
  use {'junegunn/vim-easy-align', config = [[require('config.easy_align')]]}
  use {'mhartington/formatter.nvim', config = [[require('config.format')]]}

  -- Text objects
  use 'wellle/targets.vim'

  -- Search
  use {'junegunn/fzf.vim', config = [[require('config.fzf')]]}
  use 'gfanto/fzf-lsp.nvim'

  -- Project Management/Sessions
  use {
    'dhruvasagar/vim-prosession',
    after = 'vim-obsession',
    requires = {{'tpope/vim-obsession', cmd = 'Prosession'}},
    config = [[require('config.prosession')]]
  }

  -- Undo tree
  use {
    'mbbill/undotree',
    cmd = 'UndotreeToggle',
    config = [[vim.g.undotree_SetFocusWhenToggle = 1]]
  }

  -- Git
  use {
    {'tpope/vim-fugitive', cmd = {'Gstatus', 'Gblame', 'Gpush', 'Gpull'}}, {
      'lewis6991/gitsigns.nvim',
      requires = {'nvim-lua/plenary.nvim'},
      config = [[require('config.gitsigns')]]
    }
  }

  -- Pretty symbols
  use 'kyazdani42/nvim-web-devicons'

  -- Terminal
  use 'voldikss/vim-floaterm'

  -- Completion and linting
  use {
    {'kosayoda/nvim-lightbulb', config = [[require('config.lightbulb')]]}, 'onsails/lspkind-nvim',
    'neovim/nvim-lspconfig', '~/projects/personal/lsp-status.nvim', {
      'nvim-treesitter/nvim-treesitter',
      requires = {
        'nvim-treesitter/nvim-treesitter-refactor', 'nvim-treesitter/nvim-treesitter-textobjects'
      },
      config = [[require('config.treesitter')]]
    }
  }

  -- Just for tracking progess until this is ready for use
  use {'mfussenegger/nvim-lint', opt = true}

  use {'hrsh7th/nvim-compe', config = [[require('config.compe')]], event = 'InsertEnter *'}
  use {'hrsh7th/vim-vsnip', config = [[require('config.vsnip')]], event = 'InsertEnter *'}

  -- Debugger
  use {'mfussenegger/nvim-dap', opt = true}
  use {
    'puremourning/vimspector',
    setup = [[vim.g.vimspector_enable_mappings = 'HUMAN']],
    disable = true
  }

  -- Path navigation
  use 'justinmk/vim-dirvish'

  -- LaTeX
  use {'lervag/vimtex', config = [[require('config.vimtex')]]}

  -- Meson
  use 'igankevich/mesonic'

  -- PDDL
  use 'PontusPersson/pddl.vim'

  -- Profiling
  use {'dstein64/vim-startuptime', cmd = 'StartupTime', config = [[vim.g.startuptime_tries = 10]]}

  -- Highlight colors
  use {
    'norcalli/nvim-colorizer.lua',
    ft = {'css', 'javascript', 'vim', 'html'},
    config = [[require('colorizer').setup {'css', 'javascript', 'vim', 'html'}]]
  }

  -- Color scheme
  use '~/projects/personal/vim-nazgul'
  -- use 'hardselius/warlock'
  -- use 'arzg/vim-substrata'

  -- Notes
  use {
    '~/projects/personal/pdf-scribe.nvim',
    config = [[require('config.pdf_scribe')]],
    disable = true
  }
end

local plugins = setmetatable({}, {
  __index = function(_, key)
    init()
    return packer[key]
  end
})

return plugins

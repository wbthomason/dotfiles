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
  use {'yggdroot/indentLine', config = [[require('config.indentline')]]}

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
  use {
    'nvim-lua/telescope.nvim',
    requires = {'nvim-lua/popup.nvim', 'nvim-lua/plenary.nvim'},
    disable = true
  }
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
    {'mhinz/vim-signify', config = [[require('config.signify')]]},
    {'tpope/vim-fugitive', cmd = {'Gblame', 'Gpull', 'Gpush', 'Gstatus'}}
  }

  -- Terminal
  use 'voldikss/vim-floaterm'

  -- Completion and linting
  use {
    'onsails/lspkind-nvim', 'neovim/nvim-lspconfig', '~/projects/personal/lsp-status.nvim', {
      'nvim-treesitter/nvim-treesitter',
      requires = {
        'nvim-treesitter/nvim-treesitter-refactor', 'nvim-treesitter/nvim-treesitter-textobjects'
      },
      config = [[require('config.treesitter')]]
    }
  }

  use {'hrsh7th/nvim-compe', config = [[require('config.compe')]]}
  use {'hrsh7th/vim-vsnip', config = [[require('config.vsnip')]]}

  -- Debugger
  use {'mfussenegger/nvim-dap', opt = true}
  use {
    'puremourning/vimspector',
    setup = [[vim.g.vimspector_enable_mappings = 'HUMAN']],
    opt = true
  }

  -- Path navigation
  use 'justinmk/vim-dirvish'

  -- LaTeX
  use {'lervag/vimtex', config = [[require('config.vimtex')]]}

  -- Meson
  use 'igankevich/mesonic'

  -- TOML
  use 'cespare/vim-toml'

  -- Julia
  use 'JuliaEditorSupport/julia-vim'

  -- PDDL
  use 'PontusPersson/pddl.vim'

  -- Profiling
  use {'dstein64/vim-startuptime', cmd = 'StartupTime', config = [[vim.g.startuptime_tries = 5]]}

  -- Highlight colors
  use {
    'norcalli/nvim-colorizer.lua',
    event = 'InsertEnter *',
    config = [[require('colorizer').setup {'css', 'javascript', 'vim', 'html'}]]
  }

  -- Color scheme
  use '~/projects/personal/vim-nazgul'
  -- use 'hardselius/warlock'
  -- use 'arzg/vim-substrata'

  -- Notes
  use {'~/projects/personal/pdf-scribe.nvim', config = [[require('config.pdf_scribe')]]}
end

local plugins = setmetatable({}, {
  __index = function(_, key)
    init()
    return packer[key]
  end
})

return plugins

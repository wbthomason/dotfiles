local packer = nil
local function init()
  if packer == nil then
    packer = require('packer')
    packer.init({})
  end

  local use = packer.use
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
  use { 'unblevable/quick-scope'}

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
  -- TODO: Only load this for filetypes where LSP doesn't provide formatting
  -- use {'sbdchd/neoformat', cmd = 'Neoformat'}

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
  use {'hkupty/iron.nvim', cmd = {'IronRepl', 'IronWatchCurrentFile', 'IronSend'}}

  -- Undo tree
  use {'mbbill/undotree', cmd = 'UndotreeToggle'}

  -- Git
  use 'lewis6991/gitsigns.nvim'
  use {'mhinz/vim-signify', {'tpope/vim-fugitive', cmd = {'Gpull', 'Gpush', 'Gstatus'}}}

  -- Terminal
  -- use 'voldikss/vim-floaterm'

  -- Completion and linting
  use {
    'neovim/nvim-lspconfig', '~/projects/personal/lsp-status.nvim', {
      'nvim-lua/completion-nvim',
      event = 'InsertEnter *',
      config = function()
        local completion = require('completion')
        completion.addCompletionSource('vimtex', require('vimtex').complete_item)

        vim.cmd [[ augroup lsp_aucmds ]]
        vim.cmd [[ au BufEnter * lua require('completion').on_attach() ]]
        vim.cmd [[ augroup END ]]

        completion.on_attach()
        vim.cmd [[ doautoall FileType ]]
      end,
      requires = {
        'norcalli/snippets.nvim',
        event = 'InsertEnter *',
        config = function() require('snippets').use_suggested_mappings() end
      }
    }, {'nvim-treesitter/completion-treesitter', opt = true},
    {
      'nvim-treesitter/nvim-treesitter',
      requires = {
        { 'nvim-treesitter/nvim-treesitter-refactor', after = 'nvim-treesitter'},
        { 'nvim-treesitter/nvim-treesitter-textobjects', after = 'nvim-treesitter' }},
        config = 'require("treesitter")', event =
    'VimEnter *'}
  }

  use {'liuchengxu/vista.vim', cmd = 'Vista'}
  use '~/projects/personal/hover.nvim'

  -- Debugger
  use {'mfussenegger/nvim-dap', opt = true}
  use {
    'puremourning/vimspector',
    setup = function() vim.g.vimspector_enable_mappings = 'HUMAN' end,
    opt = true
  }

  -- Linting
  use 'w0rp/ale'

  -- Language multipack
  -- use 'sheerun/vim-polyglot'

  -- Better Lua highlighting
  use 'euclidianAce/BetterLua.vim'

  -- Better Lua development for Neovim
  -- use 'tjdevries/nlua.nvim'

  -- Path navigation
  use 'justinmk/vim-dirvish'
  use 'kristijanhusak/vim-dirvish-git'

  -- -- Clojure/Lisps/Scheme
  -- local sexp_filetypes = {'clojure', 'lisp', 'scheme', 'racket', 'jbuild', 'fennel', 'pddl'}
  -- use {
  --   'guns/vim-sexp',
  --   ft = sexp_filetypes,
  --   config = function() vim.cmd [[ doautoall sexp_filetypes Filetype ]] end
  -- }
  --
  -- use {
  --   'tpope/vim-sexp-mappings-for-regular-people',
  --   ft = sexp_filetypes,
  --   config = function() vim.cmd [[ doautoall sexp_mappings_for_regular_people Filetype ]] end
  -- }
  --
  -- use {'vlime/vlime', rtp = 'vim/', ft = {'lisp'}}
  -- use {'Olical/conjure', ft = {'clojure', 'fennel'}}
  --
  -- use {'eraserhd/parinfer-rust', ft = sexp_filetypes, run = 'cargo build --release'}
  --
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
  use {'dstein64/vim-startuptime', cmd = 'StartupTime'}

  -- Highlight colors
  use 'norcalli/nvim-colorizer.lua'

  -- Color scheme
  use '~/projects/personal/vim-nazgul'
  -- use 'chriskempson/base16-vim'
  -- use 'hardselius/warlock'
  -- use 'arzg/vim-substrata'
  -- use {'tjdevries/colorbuddy.vim', opt = true}

  -- Notes
  use '~/projects/personal/pdf-scribe.nvim'

  -- Tasks
  -- use '~/projects/personal/import-todoist.nvim'
end

local plugins = setmetatable({}, {
  __index = function(_, key)
    init()
    return packer[key]
  end
})

return plugins

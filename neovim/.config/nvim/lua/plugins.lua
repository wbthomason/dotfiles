local packer = nil
local function init()
  if packer == nil then
    packer = require('packer')
    packer.init()
  end

  local use = packer.use
  packer.reset()

  -- Packer
  use { '~/projects/personal/packer.nvim', opt = true }

  -- Async building & commands
  use {
    'tpope/vim-dispatch',
    opt = true,
    cmd = { 'Dispatch', 'Make', 'Focus', 'Start' }
  }

  -- Registers
  use 'junegunn/vim-peekaboo'

  -- Marks
  use 'kshenoy/vim-signature'
  use { 'Yilin-Yang/vim-markbar', opt = true }

  -- Buffer management
  use 'mhinz/vim-sayonara'

  -- Startup screen
  use 'mhinz/vim-startify'

  -- Movement
  use 'chaoren/vim-wordmotion'
  use 'tpope/vim-repeat'
  use 'justinmk/vim-sneak'
  -- use { 'unblevable/quick-scope', opt = true }

  -- Quickfix
  -- Pack 'romainl/vim-qf'
  use { 'Olical/vim-enmasse', cmd = 'EnMasse' }

  -- Grepping
  use { 'mhinz/vim-grepper', cmd = 'Grepper' }

  -- Indentation tracking
  use 'yggdroot/indentLine'

  -- Commenting
  use 'tomtom/tcomment_vim'

  -- Wrapping/delimiters
  use 'machakann/vim-sandwich'
  use { 'andymass/vim-matchup', event = 'VimEnter *' }
  use '9mm/vim-closer'
  use 'tpope/vim-endwise'

  -- Search
  use 'romainl/vim-cool'

  -- Pattern preview
  -- use { 'markonm/traces.vim' }

  -- Prettification
  use 'junegunn/vim-easy-align'
  use 'sbdchd/neoformat'

  -- Text objects
  use 'wellle/targets.vim'
  use 'PeterRincker/vim-argumentative'

  -- Search
  use { 'liuchengxu/vim-clap', run = ':Clap install-binary' }

  -- Special symbols
  -- use { 'chrisbra/unicode.vim', opt = true }

  -- Project Management/Sessions
  use { 'tpope/vim-obsession', cmd = 'Prosession' }
  use { 'dhruvasagar/vim-prosession', after = 'vim-obsession' }

  -- REPL
  use {
    'Vigemus/iron.nvim',
    cmd = { 'IronRepl', 'IronWatchCurrentFile', 'IronSend' }
  }

  -- Undo tree
  use {
    'simnalamburt/vim-mundo',
    cmd = { 'MundoToggle', 'MundoShow' }
  }

  -- Git
  use 'mhinz/vim-signify'
  use {
    'tpope/vim-fugitive',
    cmd = { 'Gpull', 'Gpush', 'Gstatus' }
  }

  use 'tpope/vim-rhubarb'
  use 'itchyny/vim-gitbranch'

  -- Terminal
  -- use { 'voldikss/vim-floaterm' }

  -- Completion and linting
  use { 'neovim/nvim-lsp', opt = true }
  use  '~/projects/personal/lsp-status.nvim'
  use { 'haorenW1025/completion-nvim', opt = true }
  use { 'nvim-treesitter/completion-treesitter', opt = true }
  use { 'nvim-treesitter/nvim-treesitter', opt = true }

  use '~/projects/personal/hover.nvim'
  use { 'haorenW1025/diagnostic-nvim', opt = true }
  use { 'hrsh7th/vim-vsnip', opt = true }
  use { 'hrsh7th/vim-vsnip-integ', opt = true }


  use { 'ncm2/ncm2', opt = true }
  use { 'roxma/nvim-yarp', opt = true }
  use { 'ncm2/ncm2-path', opt = true }
  use { 'ncm2/ncm2-syntax', opt = true }
  use { 'Shougo/neco-syntax', opt = true }
  use { 'ncm2/float-preview.nvim', opt = true }

  -- Linting
  use {
    'w0rp/ale',
    ft = {
      'sh',
      'zsh',
      'bash',
      'c',
      'cpp',
      'cmake',
      'html',
      'markdown',
      'racket',
      'vim',
      'tex'
    },
    config = 'vim.api.nvim_command("ALEEnable")'
  }

  -- Language multipack
  use 'sheerun/vim-polyglot'

  -- Path changing
  use 'tpope/vim-apathy'

  -- Path navigation
  use 'justinmk/vim-dirvish'
  use 'kristijanhusak/vim-dirvish-git'

  -- C/C++ semantic highlighting
  -- use  'jackguo380/vim-lsp-cxx-highlight'

  -- Clojure/Lisps/Scheme
  local sexp_filetypes = { 'clojure', 'lisp', 'scheme', 'racket', 'jbuild', 'fennel', 'pddl' }
  use {
    'guns/vim-sexp',
    ft = sexp_filetypes,
    config = 'vim.api.nvim_command("doautoall sexp_filetypes Filetype")'
  }

  use {
    'tpope/vim-sexp-mappings-for-regular-people',
    ft = sexp_filetypes,
    config = 'vim.api.nvim_command("doautoall sexp_mappings_for_regular_people Filetype")'
  }

  use 'phmarek/vlime'
  use {
    'Olical/conjure',
    ft = { 'clojure', 'fennel' }
  }

  use {
    'eraserhd/parinfer-rust',
    ft = sexp_filetypes,
    run = 'cargo build --release'
  }

  -- LaTeX
  use 'lervag/vimtex'

  -- Meson
  -- use 'igankevich/mesonic'

  -- PDDL
  use 'PontusPersson/pddl.vim'

  -- Profiling
  use { 'dstein64/vim-startuptime', cmd = 'StartupTime' }

  -- Highlight colors
  use 'norcalli/nvim-colorizer.lua'

  -- Color scheme
  use '~/projects/personal/vim-nazgul'
  -- use 'chriskempson/base16-vim'
  -- use 'hardselius/warlock'
  -- use 'arzg/vim-substrata'
  use { 'tjdevries/colorbuddy.vim', opt = true }

  -- Markdown
  use { 'iamcco/markdown-preview.nvim', run = 'cd app && yarn install' }

  -- Tags
  -- use { 'ludovicchabant/vim-gutentags', opt = true }

  -- Wiki
  use {
    'lervag/wiki.vim',
    event = { 'BufNewFile ~/gdrive/notes/**/*.md', 'BufReadPre ~/gdrive/notes/**/*.md' },
    cmd = { 'WikiJournal', 'WikiOpen' },
    config = 'vim.api.nvim_command("WikiEnable")'
  }

  -- Notes
  use '~/projects/personal/pdf-scribe.nvim'

  -- Tasks
  use '~/projects/personal/import-todoist.nvim'
end

local plugins = setmetatable({}, {
  __index = function(_, key)
    init()
    return packer[key]
  end
})

return plugins

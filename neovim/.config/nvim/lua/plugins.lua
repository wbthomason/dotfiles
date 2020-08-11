local packer = nil
local function init()
  if packer == nil then
    packer = require('packer')
    packer.init()
  end

  local use = packer.use
  packer.reset()

  -- Packer
  use {'~/projects/personal/packer.nvim', opt = true}

  -- Async building & commands
  use {'tpope/vim-dispatch', opt = true, cmd = {'Dispatch', 'Make', 'Focus', 'Start'}}

  -- Registers
  use 'junegunn/vim-peekaboo'

  -- Marks
  use 'kshenoy/vim-signature'

  -- Buffer management
  use {'mhinz/vim-sayonara', cmd = 'Sayonara'}

  -- Startup screen
  use {'mhinz/vim-startify', opt = false}

  -- Movement
  use 'chaoren/vim-wordmotion'
  use 'tpope/vim-repeat'
  use 'justinmk/vim-sneak'
  -- use { 'unblevable/quick-scope', opt = true }

  -- Quickfix
  -- Pack 'romainl/vim-qf'
  use {'Olical/vim-enmasse', cmd = 'EnMasse'}

  -- Grepping
  use {'mhinz/vim-grepper', cmd = 'Grepper'}

  -- Indentation tracking
  use 'yggdroot/indentLine'

  -- Commenting
  use 'tomtom/tcomment_vim'
  -- use 'tyru/caw.vim'

  -- Wrapping/delimiters
  use 'machakann/vim-sandwich'
  use {'andymass/vim-matchup', event = 'VimEnter *'}
  use '9mm/vim-closer'
  use 'tpope/vim-endwise'

  -- Search
  use 'romainl/vim-cool'

  -- Pattern preview
  -- use { 'markonm/traces.vim' }

  -- Prettification
  use 'junegunn/vim-easy-align'
  -- TODO: Only load this for filetypes where LSP doesn't provide formatting
  use {'sbdchd/neoformat', cmd = 'Neoformat'}

  -- Text objects
  use 'wellle/targets.vim'
  -- use 'PeterRincker/vim-argumentative'

  -- Search
  use {'yuki-ycino/fzf-preview.vim', run = 'pwd && npm install', opt = true}
  use 'junegunn/fzf.vim'

  -- Project Management/Sessions
  use {
    'dhruvasagar/vim-prosession',
    after = 'vim-obsession',
    requires = {{'tpope/vim-obsession', cmd = 'Prosession'}}
  }

  -- REPL
  use {'hkupty/iron.nvim', cmd = {'IronRepl', 'IronWatchCurrentFile', 'IronSend'}}

  -- Undo tree
  use {'simnalamburt/vim-mundo', cmd = {'MundoToggle', 'MundoShow'}}

  -- Git
  use 'mhinz/vim-signify'
  use {'tpope/vim-fugitive', cmd = {'Gpull', 'Gpush', 'Gstatus'}}

  -- Terminal
  use 'voldikss/vim-floaterm'

  -- Completion and linting
  use {'neovim/nvim-lsp', opt = true}
  use '~/projects/personal/lsp-status.nvim'
  use {
    'haorenW1025/completion-nvim',
    opt = true,
    requires = {
      {'hrsh7th/vim-vsnip', opt = true}, {'hrsh7th/vim-vsnip-integ', opt = true},
      {'https://gitlab.com/HiPhish/completion-nvim-vlime', after = {'completion-nvim', 'vlime'}}
    }
  }

  use {'nvim-treesitter/completion-treesitter', opt = true}
  use {'nvim-treesitter/nvim-treesitter', opt = true}

  use {'liuchengxu/vista.vim', cmd = 'Vista'}

  use '~/projects/personal/hover.nvim'
  use {'haorenW1025/diagnostic-nvim', opt = true}

  -- Debugger
  use 'mfussenegger/nvim-dap'
  use {
    'puremourning/vimspector',
    setup = function() vim.g.vimspector_enable_mappings = 'HUMAN' end,
    config = function() vim.g.vimspector_enable_mappings = 'HUMAN' end
  }

  -- Linting
  use {
    'w0rp/ale',
    ft = {'sh', 'zsh', 'bash', 'c', 'cpp', 'cmake', 'html', 'markdown', 'racket', 'vim', 'tex'},
    cmd = 'ALEEnable',
    config = function() vim.api.nvim_command('ALEEnable') end
  }

  -- Language multipack
  use 'sheerun/vim-polyglot'

  -- Path navigation
  use 'justinmk/vim-dirvish'
  use 'kristijanhusak/vim-dirvish-git'

  -- C/C++ semantic highlighting
  -- use  'jackguo380/vim-lsp-cxx-highlight'

  -- Clojure/Lisps/Scheme
  local sexp_filetypes = {'clojure', 'lisp', 'scheme', 'racket', 'jbuild', 'fennel', 'pddl'}
  use {
    'guns/vim-sexp',
    ft = sexp_filetypes,
    config = function() vim.api.nvim_command('doautoall sexp_filetypes Filetype') end
  }

  use {
    'tpope/vim-sexp-mappings-for-regular-people',
    ft = sexp_filetypes,
    config = function()
      vim.api.nvim_command('doautoall sexp_mappings_for_regular_people Filetype')
    end
  }

  use {'vlime/vlime', rtp = 'vim/', ft = {'lisp'}}
  use {'Olical/conjure', ft = {'clojure', 'fennel'}}

  use {'eraserhd/parinfer-rust', ft = sexp_filetypes, run = 'cargo build --release'}

  -- LaTeX
  use 'lervag/vimtex'

  -- Meson
  use 'igankevich/mesonic'

  -- PDDL
  use 'PontusPersson/pddl.vim'

  -- Profiling
  use {'dstein64/vim-startuptime', cmd = 'StartupTime'}

  -- Highlight colors
  use 'norcalli/nvim-colorizer.lua'

  -- Lua development
  use {'rafcamlet/nvim-luapad', ft = 'lua'}

  -- Color scheme
  use '~/projects/personal/vim-nazgul'
  -- use 'chriskempson/base16-vim'
  -- use 'hardselius/warlock'
  -- use 'arzg/vim-substrata'
  -- use {'tjdevries/colorbuddy.vim', opt = true}

  -- Markdown
  use {
    'iamcco/markdown-preview.nvim',
    config = 'vim.api.nvim_command("doautocmd BufEnter")',
    run = 'cd app && yarn install',
    cmd = 'MarkdownPreview'
  }

  -- Tags
  -- use { 'ludovicchabant/vim-gutentags', opt = true }

  -- Wiki
  use {
    'lervag/wiki.vim',
    event = {'BufNewFile ~/gdrive/notes/*.md', 'BufRead ~/gdrive/notes/*.md'},
    cmd = {'WikiJournal', 'WikiOpen'}
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

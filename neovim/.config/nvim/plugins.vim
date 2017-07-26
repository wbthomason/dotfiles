" Plugin installation

call plug#begin('~/.local/share/nvim/plugged')

  " Utilities
  
  " Paren matching
  Plug 'itchyny/vim-parenmatch'

  " Registers
  Plug 'junegunn/vim-peekaboo' 

  " Quickfix
  Plug 'romainl/vim-qf'
  Plug 'Olical/vim-enmasse', {'on': 'EnMasse'}

  " Buffer management
  Plug 'mhinz/vim-sayonara', {'on': 'Sayonara'}

  " Startup screen
  Plug 'mhinz/vim-startify'

  " Snippets
  Plug 'SirVer/ultisnips'
  Plug 'honza/vim-snippets'

  " Status line
  Plug 'vim-airline/vim-airline'
  Plug 'vim-airline/vim-airline-themes'

  " Movement
  Plug 'easymotion/vim-easymotion', {'on': '<Plug>(easymotion' }
  Plug 'rhysd/clever-f.vim'
  Plug 'chaoren/vim-wordmotion'

  " Tmux and vim split navigation
  Plug 'christoomey/vim-tmux-navigator'

  " Color visualization
  Plug 'chrisbra/Colorizer', {'on': ['ColorHighlight', 'ColorClear', 'RGB2Term', 'HSL2RGB', 'Term2RGB', 'ColorContrast', 'ColorSwapFgBg', 'ColorToggle']}

  " Indentation tracking
  Plug 'yggdroot/indentLine'

  " Comments
  "Plug 'scrooloose/nerdcommenter'
  Plug 'tpope/vim-commentary'

  " Wrapping/delimiters
  Plug 'tpope/vim-surround'
  Plug 'luochen1990/rainbow'
  Plug 'ozelentok/vim-closer'
  Plug 'tpope/vim-endwise'

  " Undo/redo
  Plug 'mbbill/undotree', {'on': 'UndotreeToggle'}

  " Searching
  Plug 'haya14busa/incsearch.vim'
  Plug 'pgdouyon/vim-evanesco'

  " Yank highlighting
  Plug 'machakann/vim-highlightedyank'

  " Prettification
  Plug 'junegunn/vim-easy-align'
  Plug 'sbdchd/neoformat'

  " Text objects
  " Plug 'wellle/targets.vim'

  " Tags
  Plug 'ludovicchabant/vim-gutentags'
  Plug 'majutsushi/tagbar'

  " File opening/fuzzy finding
  Plug 'junegunn/fzf'
  Plug 'junegunn/fzf.vim'
  Plug 'fszymanski/fzf-gitignore', {'do': ':UpdateRemotePlugins'}

  " Writing
  Plug 'junegunn/goyo.vim', {'on': 'Goyo'}
  Plug 'junegunn/limelight.vim', {'on': 'Limelight'}
  Plug 'rhysd/vim-grammarous', {'on': 'GrammarousCheck'}
  Plug 'dbmrq/vim-ditto', {'on': ['DittoOn', 'ToggleDitto']}
  Plug 'reedes/vim-wordy', {'on': 'Wordy'}

  " Special symbols
  Plug 'chrisbra/unicode.vim'

  " Project Management
  Plug 'airblade/vim-rooter'
  Plug 'tpope/vim-projectionist'

  " REPL
  Plug 'hkupty/iron.nvim'

  " Color schemes
  Plug 'rafi/awesome-vim-colorschemes'

  " Journaling
  Plug 'vimwiki/vimwiki'

  " Denite and Unite
  Plug 'Shougo/denite.nvim', {'on': 'Denite'}
  Plug 'Shougo/unite.vim'
  Plug 'Shougo/vimfiler.vim'

  " Git
  " Plug 'tpope/vim-fugitive'
  Plug 'chemzqm/vim-easygit', {'on': ['Gadd', 'Gcd', 'Glcd', 'Gcommit', 'Gblame', 'Gstatus', 'Gdiff', 'Gbrowse']}
  Plug 'junegunn/gv.vim'
  Plug 'airblade/vim-gitgutter'
  Plug 'jreybert/vimagit', {'on': 'Magit'}
  Plug 'rhysd/committia.vim', {'for': 'gitcommit'}
  Plug 'tpope/vim-git', {'for': ['git', 'gitcommit', 'gitconfig', 'gitrebase', 'gitsendemail']}

  " Completion
  Plug 'Shougo/deoplete.nvim'
  Plug 'Shougo/neco-syntax'
  Plug 'Shougo/neoinclude.vim'
  Plug 'Shougo/context_filetype.vim'
  Plug 'ervandew/supertab'
  Plug 'Shougo/echodoc.vim'

  " Checkers
  Plug 'vim-syntastic/syntastic', {'for': ['clojure', 'idris']}
  Plug 'w0rp/ale'

  " Async building & commands
  Plug 'wbthomason/buildit.nvim'
  Plug 'tpope/vim-dispatch'
  Plug 'radenling/vim-dispatch-neovim'

  " Refactoring
  Plug 'AndrewRadev/splitjoin.vim'
  Plug 'machakann/vim-swap'
  Plug 'zirrostig/vim-schlepp'

  " Languages
  
  " Python
  Plug 'fisadev/vim-isort', {'for': 'python'}
  Plug 'zchee/deoplete-jedi', {'for': 'python'}

  " Rust
  Plug 'rust-lang/rust.vim', {'for': 'rust'}
  Plug 'racer-rust/vim-racer', {'for': 'rust'}

  " C/C++
  Plug 'tweekmonster/deoplete-clang2', {'for': ['c', 'cpp']}

  " Go
  Plug 'zchee/deoplete-go', {'for': 'go'}
  Plug 'fatih/vim-go', {'for': 'go'}

  " C#
  Plug 'dimixar/deoplete-omnisharp', {'for': 'cs'}
  Plug 'OmniSharp/omnisharp-vim', {'for': 'cs'}

  " Java
  Plug 'artur-shaik/vim-javacomplete2', {'for': 'java'}

  " Clojure
  Plug 'venantius/vim-eastwood', {'for': 'clojure'}
  Plug 'venantius/vim-cljfmt', {'for': 'clojure'}
  Plug 'guns/vim-clojure-static', {'for': 'clojure'}
  Plug 'guns/vim-sexp', {'for': ['clojure', 'lisp', 'scheme', 'racket']}
  Plug 'tpope/vim-sexp-mappings-for-regular-people', {'for': ['clojure', 'lisp', 'scheme', 'racket']}
  Plug 'guns/vim-clojure-highlight', {'for': 'clojure'}
  Plug 'clojure-vim/acid.nvim', {'for': 'clojure'}
  Plug 'SevereOverfl0w/async-clj-omni', {'for': 'clojure'}

  " Pandoc/Markdown
  Plug 'vim-pandoc/vim-pandoc', {'for': ['markdown', 'pandoc', 'markdown.pandoc']}
  Plug 'vim-pandoc/vim-pandoc-syntax', {'for': ['markdown', 'pandoc', 'markdown.pandoc']}
  Plug 'vim-pandoc/vim-pandoc-after', {'for': ['markdown', 'pandoc', 'markdown.pandoc']}

  " TOML
  Plug 'cespare/vim-toml', {'for': 'toml'}

  " Yaml
  Plug 'stephpy/vim-yaml', {'for': 'yaml'}

  " JS
  Plug 'marijnh/tern_for_vim', {'for': 'javascript'}
  Plug 'pangloss/vim-javascript', {'for': 'javascript'}
  Plug 'wookiehangover/jshint.vim', {'for': 'javascript'}
  Plug 'carlitux/deoplete-ternjs', {'for': 'javascript'}

  " Idris
  Plug 'idris-hackers/idris-vim', {'for': 'idris'}

  " Haskell
  Plug 'neovimhaskell/haskell-vim', {'for': 'haskell'}
  " neco-ghc and ghcmod-vim require vimproc
  Plug 'Shougo/vimproc.vim', {'do' : 'make'}
  Plug 'eagletmt/neco-ghc', {'for': 'haskell'}
  Plug 'eagletmt/ghcmod-vim', {'for': 'haskell'}
  Plug 'parsonsmatt/intero-neovim', {'for': 'haskell'}

  " Vimscript
  Plug 'Shougo/neco-vim', {'for': 'vim'}

  " Coffeescript
  Plug 'kchmck/vim-coffee-script', {'for': 'coffeescript'}

  " Jade
  Plug 'digitaltoad/vim-jade', {'for': 'jade'}

  " Less
  Plug 'groenewege/vim-less', {'for': 'less'}

  " Elm
  Plug 'lambdatoast/elm.vim', {'for': 'elm'}

  " OCaml
  Plug 'rgrinberg/vim-ocaml', {'for': 'ocaml'}

  " LaTeX
  Plug 'lervag/vimtex', {'for': 'tex'}
  "Plug 'poppyschmo/deoplete-latex', {'for': 'tex'}

  " Torch
  Plug 'jakezhaojb/vim-torch-snipmate', {'for': 'lua'}

  " Elixir
  Plug 'elixir-lang/vim-elixir', {'for': ['elixir', 'eelixir']}
  Plug 'slashmili/alchemist.vim', { 'for': ['elixir', 'eelixir']}

  " Scala
  Plug 'derekwyatt/vim-scala', {'for': 'scala'}
  Plug 'ensime/ensime-vim', {'for': 'scala'}

  " TypeScript
  Plug 'HerringtonDarkholme/yats.vim', {'for': 'typescript'}
  Plug 'leafgarland/typescript-vim', {'for': 'typescript'}

  " Racket
  Plug 'wlangstroth/vim-racket', {'for': 'racket'}

  " Fish
  Plug 'wilriker/vim-fish', {'for': 'fish'}

  " Coq
  Plug 'let-def/vimbufsync', {'for': 'coq'}
  Plug 'the-lambda-church/coquille', {'for': 'coq'}

  " Pretty pretty symbols
  Plug 'ryanoasis/vim-devicons'

  " Gui
  Plug 'equalsraf/neovim-gui-shim'

  " Profiling
  Plug 'tweekmonster/startuptime.vim'

call plug#end()

" Required:
filetype plugin indent on
syntax enable

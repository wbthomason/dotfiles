" Plugin installation

call plug#begin('~/.local/share/nvim/plugged')

  " Utilities

  " Startup screen
  Plug 'mhinz/vim-startify'

  " Snippets
  Plug 'SirVer/ultisnips'
  Plug 'honza/vim-snippets'

  " Status line
  Plug 'vim-airline/vim-airline'
  Plug 'vim-airline/vim-airline-themes'

  " Movement
  Plug 'easymotion/vim-easymotion'

  " Tmux and vim split navigation
  Plug 'christoomey/vim-tmux-navigator'

  " Color visualization
  Plug 'chrisbra/Colorizer', {'on': ['ColorHighlight', 'ColorClear', 'RGB2Term', 'HSL2RGB', 'Term2RGB', 'ColorContrast', 'ColorSwapFgBg', 'ColorToggle']}

  " Indentation tracking
  Plug 'yggdroot/indentLine'

  " Comments
  Plug 'scrooloose/nerdcommenter'

  " Wrapping/delimiters
  Plug 'tpope/vim-surround'
  Plug 'jiangmiao/auto-pairs'
  Plug 'luochen1990/rainbow'

  " Undo/redo
  Plug 'mbbill/undotree'

  " Searching
  Plug 'haya14busa/incsearch.vim'

  " Prettification
  Plug 'junegunn/vim-easy-align'
  Plug 'sbdchd/neoformat'

  " Text objects
  Plug 'wellle/targets.vim'

  " Tags
  Plug 'ludovicchabant/vim-gutentags'
  Plug 'majutsushi/tagbar'

  " File opening/fuzzy finding
  Plug 'junegunn/fzf', {'dir': '~/.fzf', 'do': './install --all'}
  Plug 'junegunn/fzf.vim'

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

  " REPL
  Plug 'hkupty/iron.nvim'

  " Color schemes
  Plug 'chriskempson/base16-vim'

  " Journaling
  Plug 'vimwiki/vimwiki'

  " Denite and Unite
  Plug 'Shougo/denite.nvim'
  Plug 'Shougo/unite.vim'
  Plug 'Shougo/vimproc.vim', {'do': 'make'}
  Plug 'Shougo/vimfiler.vim'
  Plug 'msprev/unite-bibtex', {'for': ['pandoc', 'tex']}

  " Git
  Plug 'tpope/vim-fugitive'
  Plug 'junegunn/gv.vim'
  Plug 'airblade/vim-gitgutter'
  Plug 'jreybert/vimagit'
  Plug 'rhysd/committia.vim'

  " Completion
  Plug 'Valloric/YouCompleteMe'
  Plug 'rdnetto/YCM-Generator', {'branch': 'stable'}

  " Checkers
  Plug 'vim-syntastic/syntastic', {'for': 'clojure'}
  Plug 'w0rp/ale'

  " Async building
  Plug 'wbthomason/buildit.nvim'

  " Languages
  
  " Python
  Plug 'fisadev/vim-isort', {'for': 'python'}

  " Rust
  Plug 'rust-lang/rust.vim', {'for': 'rust'}

  " Clojure
  Plug 'venantius/vim-eastwood', {'for': 'clojure'}
  Plug 'venantius/vim-cljfmt', {'for': 'clojure'}
  Plug 'guns/vim-clojure-static', {'for': 'clojure'}
  Plug 'guns/vim-sexp', {'for': 'clojure'}
  Plug 'tpope/vim-sexp-mappings-for-regular-people', {'for': 'clojure'}
  Plug 'guns/vim-clojure-highlight', {'for': 'clojure'}
  Plug 'clojure-vim/acid.nvim', {'for': 'clojure'}

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

  " Idris
  Plug 'idris-hackers/idris-vim', {'for': 'idris'}

  " Haskell
  Plug 'neovimhaskell/haskell-vim', {'for': 'haskell'}
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

  " Torch
  Plug 'jakezhaojb/vim-torch-snipmate', {'for': 'lua'}

  " Elixir
  Plug 'elixir-lang/vim-elixir', {'for': 'elixir'}
  Plug 'slashmili/alchemist.vim', { 'for': 'elixir'}

  " Scala
  Plug 'derekwyatt/vim-scala', {'for': 'scala'}

call plug#end()

" Required:
filetype plugin indent on
syntax enable

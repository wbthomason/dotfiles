" Plugin installation


" Required for Dein:
set runtimepath+=/home/wil/.config/nvim/dein/repos/github.com/Shougo/dein.vim

" Required for Dein:
if dein#load_state('/home/wil/.config/nvim/dein')
  call dein#begin('/home/wil/.config/nvim/dein')

  " Let dein manage dein
  " Required:
  call dein#add('/home/wil/.config/nvim/dein/repos/github.com/Shougo/dein.vim')

  " Utilities

  " Startup screen
  call dein#add('mhinz/vim-startify')

  " Snippets
  call dein#add('SirVer/ultisnips')
  call dein#add('honza/vim-snippets')

  " Status line
  call dein#add('vim-airline/vim-airline')
  call dein#add('vim-airline/vim-airline-themes')

  " Movement
  call dein#add('easymotion/vim-easymotion')

  " Tmux and vim split navigation
  call dein#add('christoomey/vim-tmux-navigator')

  " Color visualization
  call dein#add('chrisbra/Colorizer')

  " Indentation tracking
  call dein#add('yggdroot/indentLine')

  " Comments
  call dein#add('scrooloose/nerdcommenter')

  " Wrapping/delimiters
  call dein#add('tpope/vim-surround')
  call dein#add('jiangmiao/auto-pairs')
  call dein#add('luochen1990/rainbow')

  " Easier Dein use
  call dein#add('haya14busa/dein-command.vim')

  " Undo/redo
  call dein#add('mbbill/undotree')

  " Searching
  call dein#add('haya14busa/incsearch.vim')

  " Prettification
  call dein#add('junegunn/vim-easy-align')

  " Text objects
  call dein#add('wellle/targets.vim')

  " Tags
  call dein#add('ludovicchabant/vim-gutentags')
  call dein#add('majutsushi/tagbar', {'on_cmd': 'TagbarToggle'})

  " File opening/fuzzy finding
  call dein#add('junegunn/fzf', {'build': './install --all'})
  call dein#add('junegunn/fzf.vim')

  " Writing
  call dein#add('junegunn/goyo.vim', {'on_cmd': 'Goyo'})
  call dein#add('junegunn/limelight.vim', {'on_cmd': 'Limelight'})

  " Special symbols
  call dein#add('chrisbra/unicode.vim')

  " Project Management
  call dein#add('airblade/vim-rooter')

  " REPL
  call dein#add('hkupty/iron.nvim')

  " Easier remote editing
  call dein#add('zenbro/mirror.vim')

  " Color schemes
  call dein#add('flazz/vim-colorschemes')
  call dein#add('chriskempson/base16-vim')

  " Journaling
  call dein#add('vimwiki/vimwiki')

  " Unite and Denite
  call dein#add('Shougo/denite.nvim', {'merged': 0, 'loadconf': 1})
  call dein#add('Shougo/unite.vim', {'merged': 0 , 'loadconf': 1})

  " Dependencies
  call dein#add('Shougo/vimproc.vim', {'build': 'make'})

  " Sources
  call dein#add('Shougo/vimfiler.vim')

  " Git
  call dein#add('tpope/vim-fugitive')
  call dein#add('junegunn/gv.vim')
  call dein#add('airblade/vim-gitgutter')

  " Completion
  call dein#add('Valloric/YouCompleteMe', {'merged': 0})
  call dein#add('rdnetto/YCM-Generator', {'rev': 'stable'})

  " Checkers
  call dein#add('vim-syntastic/syntastic', {'on_ft': 'clojure'})
  call dein#add('w0rp/ale')

  " Async building
  call dein#add('wbthomason/buildit.nvim')

  " Languages
  
  " Python
  call dein#add('fisadev/vim-isort', {'on_ft': 'python'})

  " Rust
  call dein#add('rust-lang/rust.vim', {'on_ft': 'rust'})
  call dein#add('racer-rust/vim-racer', {'on_ft': 'rust'})

  " Clojure
  call dein#add('venantius/vim-eastwood', {'on_ft': 'clojure'})
  call dein#add('venantius/vim-cljfmt', {'on_ft': 'clojure'})
  call dein#add('guns/vim-clojure-static', {'on_ft': 'clojure'})
  call dein#add('guns/vim-sexp', {'on_ft': 'clojure'})
  call dein#add('tpope/vim-sexp-mappings-for-regular-people', {'on_ft': 'clojure'})
  call dein#add('guns/vim-clojure-highlight', {'on_ft': 'clojure'})
  call dein#add('hkupty/acid.nvim', {'on_ft': 'clojure'})

  " Pandoc/Markdown
  call dein#add('vim-pandoc/vim-pandoc', {'on_ft': ['markdown', 'pandoc', 'markdown.pandoc']})
  call dein#add('vim-pandoc/vim-pandoc-syntax', {'on_ft': ['markdown', 'pandoc', 'markdown.pandoc']})
  call dein#add('vim-pandoc/vim-pandoc-after', {'on_ft': ['markdown', 'pandoc', 'markdown.pandoc']})

  " TOML
  call dein#add('cespare/vim-toml', {'on_ft': 'toml'})

  " Yaml
  call dein#add('stephpy/vim-yaml', {'on_ft': 'yaml'})

  " JS
  call dein#add('marijnh/tern_for_vim', {'on_ft': 'javascript'})
  call dein#add('pangloss/vim-javascript', {'on_ft': 'javascript'})
  call dein#add('wookiehangover/jshint.vim', {'on_ft': 'javascript'})

  " Idris
  call dein#add('idris-hackers/idris-vim', {'on_ft': 'idris'})

  " Haskell
  call dein#add('neovimhaskell/haskell-vim', {'on_ft': 'haskell'})
  call dein#add('eagletmt/neco-ghc', {'on_ft': 'haskell'})
  call dein#add('eagletmt/ghcmod-vim', {'on_ft': 'haskell'})
  call dein#add('parsonsmatt/intero-neovim', {'on_ft': 'haskell'})

  " Vimscript
  call dein#add('Shougo/neco-vim', {'on_ft': 'vim'})

  " Coffeescript
  call dein#add('kchmck/vim-coffee-script', {'on_ft': 'coffeescript'})

  " Jade
  call dein#add('digitaltoad/vim-jade', {'on_ft': 'jade'})

  " Less
  call dein#add('groenewege/vim-less', {'on_ft': 'less'})

  " Elm
  call dein#add('lambdatoast/elm.vim', {'on_ft': 'elm'})

  " OCaml
  call dein#add('rgrinberg/vim-ocaml', {'on_ft': 'ocaml'})

  " LaTeX
  call dein#add('lervag/vimtex', {'on_ft': 'tex'})

  " Torch
  call dein#add('jakezhaojb/vim-torch-snipmate', {'on_ft': 'lua'})

  " Elixir
  call dein#add('elixir-lang/vim-elixir', {'on_ft': 'elixir'})
  call dein#add('slashmili/alchemist.vim', { 'on_ft': 'elixir'})

  " Scala
  call dein#add('derekwyatt/vim-scala', {'on_ft': 'scala'})

  " Required:
  call dein#end()
  call dein#save_state()
endif

" Required:
filetype plugin indent on
syntax enable

" If you want to install not installed plugins on startup.
if dein#check_install()
  call dein#install()
endif


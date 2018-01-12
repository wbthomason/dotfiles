" Plugin installation

call plug#begin('~/.local/share/nvim/plugged')

  " Utilities
  
  " Tweaks
  Plug 'tpope/vim-unimpaired'
  Plug 'tpope/vim-repeat'

  " Mappings
  Plug 'hecal3/vim-leader-guide'

  " Paren matching
  Plug 'itchyny/vim-parenmatch'

  " Registers
  Plug 'junegunn/vim-peekaboo' 

  " Marks
  Plug 'kshenoy/vim-signature'

  " Quickfix
  Plug 'romainl/vim-qf'

  " Buffer management
  Plug 'mhinz/vim-sayonara'
  Plug 'danro/rename.vim'

  " Startup screen
  Plug 'mhinz/vim-startify'

  " Snippets
  Plug 'SirVer/ultisnips', {'on': []}
  Plug 'honza/vim-snippets'

  " Status line
  Plug 'vim-airline/vim-airline-themes'
  Plug 'vim-airline/vim-airline'

  " Movement
  Plug 'yangmillstheory/vim-snipe'
  Plug 'rhysd/clever-f.vim'
  Plug 'chaoren/vim-wordmotion'

  " Tmux and vim split navigation
  Plug 'christoomey/vim-tmux-navigator'

  " Color visualization
  Plug 'chrisbra/Colorizer'

  " Indentation tracking
  Plug 'yggdroot/indentLine'

  " Comments
  Plug 'tpope/vim-commentary'

  " Wrapping/delimiters
  " Plug 'tpope/vim-surround'
  Plug 'machakann/vim-sandwich'
  Plug 'luochen1990/rainbow'
  Plug 'ozelentok/vim-closer'
  Plug 'tpope/vim-endwise'

  " Undo/redo
  Plug 'mbbill/undotree'

  " Searching
  Plug 'haya14busa/incsearch.vim'
  " Plug 'pelodelfuego/vim-swoop'

  " Yank highlighting
  Plug 'machakann/vim-highlightedyank'

  " Prettification
  Plug 'junegunn/vim-easy-align'
  Plug 'sbdchd/neoformat'

  " Text objects
  Plug 'wellle/targets.vim'
  Plug 'vim-scripts/argtextobj.vim'

  " Tags
  " Slow to load, so we use an autocommand after Vim starts
  Plug 'ludovicchabant/vim-gutentags', {'on': []}
  Plug 'majutsushi/tagbar'

  " File opening/fuzzy finding
  Plug 'junegunn/fzf'
  Plug 'junegunn/fzf.vim'
  Plug 'fszymanski/fzf-gitignore', {'do': ':UpdateRemotePlugins'}

  " Writing
  Plug 'junegunn/goyo.vim'
  Plug 'junegunn/limelight.vim'

  " Special symbols
  Plug 'chrisbra/unicode.vim'

  " Project Management
  Plug 'airblade/vim-rooter'
  Plug 'tpope/vim-obsession'
  Plug 'dhruvasagar/vim-prosession'

  " REPL
  Plug 'hkupty/iron.nvim'

  " Color schemes
  " Plug 'rafi/awesome-vim-colorschemes'
  " Plug 'chriskempson/base16-vim'
  Plug 'w0ng/vim-hybrid'

  " Notes/Wiki
  Plug 'vimwiki/vimwiki'
  Plug 'vim-scripts/SyntaxRange'
  Plug 'dhruvasagar/vim-table-mode', {'for': ['markdown', 'pandoc', 'markdown.pandoc']}
  function! BuildComposer(info)
    if a:info.status != 'unchanged' || a:info.force
      if has('nvim')
        !cargo build --release
      else
        !cargo build --release --no-default-features --features json-rpc
      endif
    endif
  endfunction

  Plug 'euclio/vim-markdown-composer', { 'do': function('BuildComposer') }

  " File explorer
  " Plug 'justinmk/vim-dirvish'
  Plug 'cocopon/vaffle.vim'
  " Plug 'Shougo/unite.vim'
  " Plug 'Shougo/vimfiler.vim'
  " Plug 'romgrk/vimfiler-prompt'

  " Directory creation
  Plug 'duggiefresh/vim-easydir'

  " Git
  Plug 'tpope/vim-fugitive'
  Plug 'tpope/vim-rhubarb'
  Plug 'junegunn/gv.vim'
  Plug 'airblade/vim-gitgutter'
  Plug 'rhysd/committia.vim', {'for': 'gitcommit'}
  Plug 'tpope/vim-git', {'for': ['git', 'gitcommit', 'gitconfig', 'gitrebase', 'gitsendemail']}

  " Completion
  Plug 'Shougo/neco-syntax'
  Plug 'Shougo/context_filetype.vim'
  Plug 'Shougo/echodoc.vim'
  Plug 'Shougo/neoinclude.vim', {'for': ['c', 'cpp']}
  Plug 'roxma/nvim-completion-manager'

  " Github
  Plug 'roxma/ncm-github', {'for': ['git', 'gitcommit', 'gitconfig', 'gitrebase', 'gitsendemail']}

  " Checkers
  Plug 'w0rp/ale'

  " Async building & commands
  Plug 'wbthomason/buildit.nvim'
  Plug 'tpope/vim-dispatch'
  Plug 'radenling/vim-dispatch-neovim'

  " Block manipulation
  Plug 'kana/vim-niceblock'
  Plug 'lgalke/splitjoin.vim'
  Plug 'machakann/vim-swap'
  Plug 'zirrostig/vim-schlepp'

  " Languages
  
  " LSP
  Plug 'autozimu/LanguageClient-neovim', {'do': ':UpdateRemotePlugins'}
  
  " Python
  Plug 'fisadev/vim-isort'
  Plug 'vim-scripts/python_match.vim'

  " C/C++
  Plug 'roxma/ncm-clang'
  
  " Go
  " Plug 'fatih/vim-go', {'for': 'go'}

  " C#
  " Plug 'OmniSharp/omnisharp-vim', {'for': 'cs'}

  " Java
  Plug 'sassanh/nvim-cm-eclim'

  " Clojure
  Plug 'venantius/vim-cljfmt'
  Plug 'guns/vim-sexp', {'for': ['clojure', 'lisp', 'scheme', 'racket', 'jbuild']}
  Plug 'tpope/vim-sexp-mappings-for-regular-people', {'for': ['clojure', 'lisp', 'scheme', 'racket', 'jbuild']}
  " Plug 'clojure-vim/acid.nvim'
  Plug 'clojure-vim/async-clj-omni'

  " Pandoc/Markdown
  Plug 'vim-pandoc/vim-pandoc'
  Plug 'vim-pandoc/vim-pandoc-syntax'
  Plug 'vim-pandoc/vim-pandoc-after'

  " TOML
  Plug 'cespare/vim-toml'

  " Yaml
  Plug 'stephpy/vim-yaml'

  " JS
  Plug 'pangloss/vim-javascript'
  Plug 'roxma/nvim-cm-tern',  {'do': 'npm install'}

  " Idris
  Plug 'idris-hackers/idris-vim'

  " Haskell
  Plug 'neovimhaskell/haskell-vim'
  Plug 'parsonsmatt/intero-neovim'
  Plug 'eagletmt/neco-ghc'

  " Vimscript
  Plug 'Shougo/neco-vim'

  " CSS
  Plug 'calebeby/ncm-css'

  " Lua
  Plug 'tbastos/vim-lua'

  " Elm
  Plug 'lambdatoast/elm.vim'
  Plug 'roxma/ncm-elm-oracle'

  " OCaml
  Plug 'rgrinberg/vim-ocaml'

  " LaTeX
  Plug 'lervag/vimtex'

  " Elixir
  Plug 'elixir-lang/vim-elixir'
  Plug 'slashmili/alchemist.vim'

  " Scala
  Plug 'ensime/ensime-vim', {'for': 'scala'}

  " TypeScript
  Plug 'HerringtonDarkholme/yats.vim'
  Plug 'leafgarland/typescript-vim'

  " Racket
  Plug 'wlangstroth/vim-racket'

  " Fish
  Plug 'wilriker/vim-fish'

  " Coq
  Plug 'epdtry/neovim-coq'
  " Plug 'let-def/vimbufsync', {'for': 'coq'}
  " Plug 'the-lambda-church/coquille', {'for': 'coq'}

  " Pretty pretty symbols
  Plug 'ryanoasis/vim-devicons'

  " Profiling
  Plug 'tweekmonster/startuptime.vim'

call plug#end()

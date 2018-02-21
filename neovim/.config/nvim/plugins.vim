" Plugin installation

if dein#load_state('/home/wil/.cache/dein')
  call dein#begin('/home/wil/.cache/dein')
  call dein#add('/home/wil/.cache/dein/repos/github.com/Shougo/dein.vim')
  call dein#add('haya14busa/dein-command.vim')
  " Utilities

  " Tweaks
  " call dein#add('tpope/vim-unimpaired')
  call dein#add('tpope/vim-repeat')

  " Mappings
  call dein#add('hecal3/vim-leader-guide')

  " Paren matching
  call dein#add('itchyny/vim-parenmatch')

  " Registers
  call dein#add('junegunn/vim-peekaboo')

  " Marks
  call dein#add('kshenoy/vim-signature')

  " Quickfix
  " call dein#add('romainl/vim-qf')

  " Buffer management
  call dein#add('mhinz/vim-sayonara')

  " Startup screen
  call dein#add('mhinz/vim-startify')

  " Snippets
  call dein#add('SirVer/ultisnips')
  call dein#add('honza/vim-snippets')

  " Status line
  call dein#add('bling/vim-bufferline')
  call dein#add('vim-airline/vim-airline-themes')
  call dein#add('vim-airline/vim-airline', {'depends': ['bufferline']})

  " Movement
  " call dein#add('yangmillstheory/vim-snipe')
  call dein#add('rhysd/clever-f.vim')
  call dein#add('chaoren/vim-wordmotion')

  " Tmux and vim split navigation
  call dein#add('christoomey/vim-tmux-navigator')

  " Color visualization
  call dein#add('chrisbra/Colorizer')

  " Indentation tracking
  call dein#add('yggdroot/indentLine')

  " Comments
  call dein#add('tomtom/tcomment_vim')

  " Wrapping/delimiters
  call dein#add('machakann/vim-sandwich')
  call dein#add('luochen1990/rainbow')
  call dein#add('ozelentok/vim-closer')
  call dein#add('tpope/vim-endwise')

  " Undo/redo
  " call dein#add('mbbill/undotree')

  " Searching
  call dein#add('haya14busa/incsearch.vim')

  " Yank highlighting
  call dein#add('machakann/vim-highlightedyank')

  " Prettification
  call dein#add('junegunn/vim-easy-align')
  call dein#add('sbdchd/neoformat')

  " Text objects
  call dein#add('wellle/targets.vim')

  " Tags
  " Slow to load, so we use an autocommand after Vim starts
  call dein#add('ludovicchabant/vim-gutentags')
  call dein#add('majutsushi/tagbar')

  " File opening/fuzzy finding
  " call dein#add('junegunn/fzf', {'merged': 0})
  " call dein#add('junegunn/fzf.vim', {'depends': 'fzf', 'lazy': 0})
  " call dein#add('fszymanski/fzf-gitignore')

  " Denite
  call dein#add('Shougo/denite.nvim')
  call dein#add('chemzqm/denite-extra')
  call dein#add('nixprime/cpsm', {'build': 'env PY3=ON ./install.sh'})

  " Writing
  call dein#add('junegunn/goyo.vim')
  call dein#add('junegunn/limelight.vim')

  " Special symbols
  call dein#add('chrisbra/unicode.vim')

  " Project Management
  call dein#add('airblade/vim-rooter')
  call dein#add('tpope/vim-obsession')
  call dein#add('dhruvasagar/vim-prosession')

  " REPL
  call dein#add('hkupty/iron.nvim')

  " Color schemes
  call dein#add('https://gitlab.com/yorickpeterse/happy_hacking.vim')

  " Notes/Wiki
  call dein#add('fmoralesc/vim-pad')
  call dein#add('tpope/vim-speeddating')
  call dein#add('mattn/calendar-vim')

  " File explorer
  call dein#add('cocopon/vaffle.vim')
  " call dein#add('Shougo/defx.nvim')

  " Git
  call dein#add('tpope/vim-fugitive')
  " call dein#add('tpope/vim-rhubarb')
  call dein#add('airblade/vim-gitgutter')
  call dein#add('rhysd/committia.vim')
  call dein#add('tpope/vim-git')

  " Completion
  call dein#add('Shougo/neco-syntax')
  call dein#add('Shougo/context_filetype.vim')
  " call dein#add('Shougo/echodoc.vim')
  call dein#add('Shougo/neoinclude.vim', {'on_ft': ['c', 'cpp']})
  call dein#add('roxma/nvim-completion-manager')

  " Checkers
  call dein#add('w0rp/ale')

  " Async building & commands
  call dein#add('wbthomason/buildit.nvim')
  call dein#add('tpope/vim-dispatch')
  call dein#add('radenling/vim-dispatch-neovim')

  " Block manipulation
  call dein#add('kana/vim-niceblock')
  call dein#add('AndrewRadev/splitjoin.vim')
  call dein#add('machakann/vim-swap')
  call dein#add('zirrostig/vim-schlepp')

  " Languages

  " LSP
  call dein#add('autozimu/LanguageClient-neovim', {
    \ 'rev': 'next',
    \ 'build': 'bash install.sh',
    \ })

  " Python
  " call dein#add('fisadev/vim-isort')
  call dein#add('vim-scripts/python_match.vim')

  " C/C++
  call dein#add('roxma/ncm-clang')

  " Java
  call dein#add('sassanh/nvim-cm-eclim')

  " Clojure/Lisps/Scheme
  call dein#add('guns/vim-sexp', {'on_ft': ['clojure', 'lisp', 'scheme', 'racket', 'jbuild']})
  call dein#add('tpope/vim-sexp-mappings-for-regular-people', {'on_ft': ['clojure', 'lisp', 'scheme', 'racket', 'jbuild']})
  call dein#add('clojure-vim/async-clj-omni', {'on_ft': 'clojure'})
  call dein#add('wlangstroth/vim-racket', {'on_ft': 'racket'})
  call dein#add('kovisoft/slimv', {'on_ft': ['clojure', 'lisp', 'scheme', 'racket']})
  " call dein#add('bhurlow/vim-parinfer', {'on_ft': ['clojure', 'lisp', 'scheme', 'racket', 'jbuild']})

  " Pandoc/Markdown
  call dein#add('vim-pandoc/vim-pandoc', {'depends': ['vim-pandoc-syntax', 'vim-pandoc-after']})
  call dein#add('vim-pandoc/vim-pandoc-syntax')
  call dein#add('vim-pandoc/vim-pandoc-after')
  call dein#add('dhruvasagar/vim-table-mode',
        \ {'on_ft': ['vimwiki', 'markdown', 'pandoc', 'markdown.pandoc']})

  call dein#add('euclio/vim-markdown-composer',
        \ {'build': 'cargo build --release',
        \ 'on_ft': ['vimwiki', 'markdown', 'pandoc', 'markdown.pandoc']})
  call dein#add('wbthomason/vim-madoko')

  " TOML
  call dein#add('cespare/vim-toml')

  " Yaml
  call dein#add('stephpy/vim-yaml')

  " JS
  call dein#add('pangloss/vim-javascript')
  call dein#add('roxma/nvim-cm-tern',  {'build': 'npm install'})

  " Idris
  call dein#add('idris-hackers/idris-vim')

  " Haskell
  call dein#add('neovimhaskell/haskell-vim')
  call dein#add('parsonsmatt/intero-neovim')
  call dein#add('eagletmt/neco-ghc')

  " Vimscript
  call dein#add('Shougo/neco-vim')

  " CSS
  call dein#add('calebeby/ncm-css')

  " Lua
  call dein#add('tbastos/vim-lua')

  " Elm
  call dein#add('lambdatoast/elm.vim')
  call dein#add('roxma/ncm-elm-oracle')

  " OCaml
  call dein#add('rgrinberg/vim-ocaml')

  " LaTeX
  call dein#add('lervag/vimtex')

  " Elixir
  call dein#add('elixir-lang/vim-elixir')
  call dein#add('slashmili/alchemist.vim')

  " TypeScript
  call dein#add('HerringtonDarkholme/yats.vim')
  call dein#add('leafgarland/typescript-vim')

  " Fish
  call dein#add('wilriker/vim-fish')

  " Coq
  call dein#add('epdtry/neovim-coq')

  " C#
  call dein#add('cyansprite/omnisharp.nvim', {'build': './install.sh', 'on_ft': 'cs'})

  " Meson
  call dein#add('stfl/meson.vim')

  " Pretty pretty symbols
  call dein#add('ryanoasis/vim-devicons')

  " Profiling
  call dein#add('tweekmonster/startuptime.vim')

  call dein#end()
  call dein#save_state()
endif
call dein#remote_plugins()

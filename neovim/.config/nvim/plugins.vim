" Plugin installation
if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
  silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs
		\ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  au VimEnter * PlugInstall --sync | source ~/.config/nvim/init.vim
endif

call plug#begin('~/.local/share/nvim/plugs')

" Tweaks
Plug 'vim-scripts/LargeFile'

" Split resizing
Plug 'roman/golden-ratio'

" Mappings
Plug 'hecal3/vim-leader-guide'

" Registers
Plug 'junegunn/vim-peekaboo'

" Marks
Plug 'kshenoy/vim-signature'

" Buffer management
Plug 'mhinz/vim-sayonara'

" Startup screen
Plug 'mhinz/vim-startify'

" Status line
if !exists('g:gui_oni')
  Plug 'bling/vim-bufferline'
  Plug 'vim-airline/vim-airline-themes'
  Plug 'vim-airline/vim-airline'
endif

" Movement
Plug 'chaoren/vim-wordmotion'
Plug 'tpope/vim-repeat'
Plug 'justinmk/vim-sneak'

" Tmux and vim split navigation
Plug 'christoomey/vim-tmux-navigator'

" Color visualization
Plug 'chrisbra/Colorizer'

" Indentation tracking
Plug 'yggdroot/indentLine'

" Comments
Plug 'tomtom/tcomment_vim'
" Plug 'tpope/vim-commentary'

" Wrapping/delimiters
Plug 'machakann/vim-sandwich'
Plug 'luochen1990/rainbow'
Plug 'ozelentok/vim-closer'
Plug 'tpope/vim-endwise'
Plug 'itchyny/vim-parenmatch'

" Searching
Plug 'haya14busa/incsearch.vim'
Plug 'markonm/traces.vim'

" Yank highlighting
Plug 'machakann/vim-highlightedyank'

" Prettification
Plug 'junegunn/vim-easy-align'
Plug 'sbdchd/neoformat'

" Text objects
Plug 'wellle/targets.vim'
Plug 'tommcdo/vim-exchange'

" Block manipulation
Plug 'machakann/vim-swap'
Plug 'zirrostig/vim-schlepp'

" Tags
" Plug 'ludovicchabant/vim-gutentags'

" FZF
" Plug 'junegunn/fzf.vim'
" Plug 'fszymanski/fzf-gitignore'

" Denite
Plug 'nixprime/cpsm', {'do': 'env PY3=ON ./install.sh'}
Plug 'Shougo/denite.nvim'
Plug 'iamcco/gitignore.vim'
Plug 'dunstontc/projectile.nvim'
Plug 'ozelentok/denite-gtags'
Plug 'neoclide/denite-extra'

" Writing
Plug 'junegunn/goyo.vim'
Plug 'junegunn/limelight.vim'

" Special symbols
Plug 'chrisbra/unicode.vim'

" Project Management/Sessions
Plug 'airblade/vim-rooter'
Plug 'tpope/vim-obsession'
Plug 'dhruvasagar/vim-prosession'

" REPL
Plug 'hkupty/iron.nvim'

" Jupyter
" Plug 'vyzyv/vimpyter'

" Undo tree
Plug 'simnalamburt/vim-mundo'

" Color scheme
Plug 'wbthomason/vim-nazgul'
Plug 'lifepillar/vim-solarized8'

" Notes/Wiki
Plug 'fmoralesc/vim-pad', {'branch': 'devel'}

" File explorer
Plug 'cocopon/vaffle.vim'
" Plug 'Shougo/defx.nvim'

" Git
Plug 'mhinz/vim-signify'
Plug 'tpope/vim-fugitive'
Plug 'jreybert/vimagit'

" Completion
Plug 'Shougo/echodoc.vim'
Plug 'roxma/nvim-yarp'
Plug 'ncm2/ncm2'
Plug 'ncm2/ncm2-bufword'
Plug 'ncm2/ncm2-path'
Plug 'ncm2/ncm2-ultisnips'
Plug 'ncm2/ncm2-tagprefix'
Plug 'ncm2/ncm2-syntax' | Plug 'Shougo/neco-syntax'

" Snippets
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'

" Checkers
Plug 'w0rp/ale'

" Async building & commands
Plug 'wbthomason/buildit.nvim'
Plug 'tpope/vim-dispatch'
Plug 'radenling/vim-dispatch-neovim'

" Language multipack
Plug 'sheerun/vim-polyglot'

" Path changing
Plug 'tpope/vim-apathy'

" LSP
Plug 'autozimu/LanguageClient-neovim', {
	  \ 'branch': 'next',
	  \ 'do': 'bash install.sh',
	  \ }

" CSS
Plug 'ncm2/ncm2-cssomni'

" Python
Plug 'ncm2/ncm2-jedi'

" C/C++
Plug 'ncm2/ncm2-pyclang'

" Clojure/Lisps/Scheme
" Plug 'guns/vim-sexp', {'for': ['clojure', 'lisp', 'scheme', 'racket', 'jbuild']}
" Plug 'tpope/vim-sexp-mappings-for-regular-people', {'for': ['clojure', 'lisp', 'scheme', 'racket', 'jbuild']}
Plug 'vim-scripts/scribble.vim'
Plug 'kovisoft/slimv', {'for': ['clojure', 'scheme', 'racket']}
Plug 'l04m33/vlime', {'rtp': 'vim/', 'for': 'lisp'}

" Pandoc/Markdown
Plug 'vim-pandoc/vim-pandoc'
Plug 'vim-pandoc/vim-pandoc-syntax'
Plug 'vim-pandoc/vim-pandoc-after'

" Haskell
Plug 'parsonsmatt/intero-neovim'
Plug 'eagletmt/neco-ghc'

" Rust
Plug 'ncm2/ncm2-racer'

" Vimscript
Plug 'ncm2/ncm2-vim' | Plug 'Shougo/neco-vim'

" LaTeX
Plug 'lervag/vimtex'

" Meson
Plug 'stfl/meson.vim'
Plug 'igankevich/mesonic'

" PDDL
Plug 'PontusPersson/pddl.vim'

" Pretty pretty symbols
Plug 'ryanoasis/vim-devicons'
Plug 'calebsmith/vim-lambdify'

" Profiling
Plug 'tweekmonster/startuptime.vim'

call plug#end()

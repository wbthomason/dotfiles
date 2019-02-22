" Plugin installation
if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
  silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs
		\ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  au VimEnter * PlugInstall --sync | source ~/.config/nvim/init.vim
endif

call plug#begin('~/.local/share/nvim/plugs')

" Moonscript
Plug 'svermeulen/nvim-moonmaker'

" Tweaks
Plug 'vim-scripts/LargeFile'

" Split resizing
Plug 'roman/golden-ratio'

" Mappings
Plug 'liuchengxu/vim-which-key'

" Registers
Plug 'junegunn/vim-peekaboo'

" Marks
Plug 'kshenoy/vim-signature'

" Buffer management
Plug 'mhinz/vim-sayonara', {'on': 'Sayonara'}

" Startup screen
Plug 'mhinz/vim-startify'

" Status line
Plug 'itchyny/lightline.vim'
Plug 'maximbaz/lightline-ale'

" System utilities
Plug 'tpope/vim-eunuch'

" Movement
Plug 'chaoren/vim-wordmotion'
Plug 'tpope/vim-repeat'
Plug 'justinmk/vim-sneak'
" Plug 'tpope/vim-unimpaired'

" Tmux and vim split navigation
Plug 'christoomey/vim-tmux-navigator'

" Color visualization
Plug 'lilydjwg/colorizer'

" Indentation tracking
Plug 'yggdroot/indentLine'

" Comments
Plug 'tomtom/tcomment_vim'

" Wrapping/delimiters
Plug 'machakann/vim-sandwich'
Plug 'luochen1990/rainbow'
" Plug 'itchyny/vim-parenmatch'
" Plug 'djdt/vim-matchparenalways'
Plug 'andymass/vim-matchup'
Plug 'rstacruz/vim-closer' " - Causes problems with ncm2-ultisnips default CR binding
Plug 'tpope/vim-endwise' " - Causes problems with ncm2-ultisnips default CR binding
Plug 'machakann/vim-swap'

" Yank highlighting
Plug 'machakann/vim-highlightedyank'

" Expand region
Plug 'landock/vim-expand-region'

" Prettification
Plug 'junegunn/vim-easy-align'
Plug 'sbdchd/neoformat', {'on': 'Neoformat'}

" Text objects
Plug 'wellle/targets.vim'

" Tags
Plug 'ludovicchabant/vim-gutentags'

" FZF
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim' 
Plug 'fszymanski/fzf-gitignore', {'on': 'FzfGitignore'}
Plug 'justinhoward/fzf-neoyank' | Plug 'Shougo/neoyank.vim'

" Writing
" Plug 'junegunn/goyo.vim', {'on': 'Goyo'}
" Plug 'junegunn/limelight.vim', {'on': 'Limelight!!'}

" Special symbols
Plug 'chrisbra/unicode.vim', {'on': 'UnicodeSearch'}

" Project Management/Sessions
Plug 'airblade/vim-rooter'
Plug 'tpope/vim-obsession', {'on': 'Prosession'}
Plug 'dhruvasagar/vim-prosession', {'on': 'Prosession'}

" REPL
Plug 'Vigemus/iron.nvim'

" Jupyter
" Plug 'vyzyv/vimpyter'

" Undo tree
Plug 'simnalamburt/vim-mundo', {'on': ['MundoShow', 'MundoToggle']}

" Notes/Wiki
Plug 'fmoralesc/vim-pad', {'branch': 'devel'}

" File explorer
Plug 'cocopon/vaffle.vim'

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
Plug 'ncm2/ncm2-tagprefix'
Plug 'ncm2/ncm2-syntax' | Plug 'Shougo/neco-syntax'
Plug 'ncm2/ncm2-neoinclude' | Plug 'Shougo/neoinclude.vim'

" Snippets
Plug 'ncm2/ncm2-neosnippet'
Plug 'Shougo/neosnippet.vim'
Plug 'Shougo/neosnippet-snippets'

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
      \ 'do': 'make -j 8 release',
      \ }

" CSS
Plug 'ncm2/ncm2-cssomni'

" Python
Plug 'ncm2/ncm2-jedi'

" C/C++
Plug 'ncm2/ncm2-pyclang'
Plug 'sakhnik/nvim-gdb', {'do': './install.sh', 'for': ['c', 'cpp', 'python']}

" Clojure/Lisps/Scheme
Plug 'guns/vim-sexp', {'for': ['clojure', 'lisp', 'scheme', 'racket', 'jbuild']}
Plug 'tpope/vim-sexp-mappings-for-regular-people', {'for': ['clojure', 'lisp', 'scheme', 'racket', 'jbuild']}
Plug 'vim-scripts/scribble.vim'
Plug 'kovisoft/slimv', {'for': ['clojure', 'scheme', 'racket']}
Plug 'l04m33/vlime', {'rtp': 'vim/', 'for': 'lisp'}

" Pandoc/Markdown
Plug 'vim-pandoc/vim-pandoc'
Plug 'vim-pandoc/vim-pandoc-syntax'
Plug 'vim-pandoc/vim-pandoc-after'

" Org-mode
Plug 'vim-scripts/utl.vim', {'for': 'org'}
Plug 'itchyny/calendar.vim', {'for': 'org'}
Plug 'tpope/vim-speeddating', {'for': 'org'}
Plug 'chrisbra/NrrwRgn', {'for': 'org'}
Plug 'inkarkat/vim-SyntaxRange', {'for': 'org'}
Plug 'jceb/vim-orgmode'
Plug 'wbthomason/capture.vim'
Plug '/home/wil/projects/personal/orgmode.nvim'

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

" Coq
Plug 'jvoorhis/coq.vim'

" Pretty pretty symbols
Plug 'calebsmith/vim-lambdify'

" Profiling
Plug 'tweekmonster/startuptime.vim'

" Color scheme
Plug 'wbthomason/vim-nazgul'
" Plug 'chriskempson/base16-vim'

call plug#end()

" Plugin installation
if empty(glob('~/.config/nvim/pack/minpac/opt/plugpac.vim'))
  silent !git clone https://github.com/k-takata/minpac.git ~/.config/nvim/pack/minpac/opt/minpac
  silent !curl -fLo ~/.config/nvim/autoload/plugpac.vim --create-dirs https://raw.githubusercontent.com/bennyyip/plugpac.vim/master/plugpac.vim
  au VimEnter * PackInstall
  source ~/.config/nvim/init.vim
endif

call plugpac#begin()

" minpac
Pack 'k-takata/minpac', {'type': 'opt'}
Pack 'bennyyip/plugpac.vim', {'type': 'opt', 'do': ':silent !cp ~/.config/nvim/pack/minpac/plugpac.vim/plugpac.vim ~/.config/nvim/autoload/plugpac.vim'}

" " Configure with Fennel
" Pack 'Olical/aniseed', {'type': 'opt'}
" Pack 'bakpakin/fennel.vim'

" Async building & commands
" Pack 'radenling/vim-dispatch-neovim'
Pack 'tpope/vim-dispatch', {'on': ['Dispatch', 'Make', 'Focus', 'Start']}

" More efficient large files (disabled because it's not clear if it's still necessary)
" Pack 'vim-scripts/LargeFile'

" Registers
Pack 'junegunn/vim-peekaboo'

" Marks
Pack 'kshenoy/vim-signature'

" Buffer management
Pack 'mhinz/vim-sayonara', {'on': 'Sayonara'}

" Startup screen
Pack 'mhinz/vim-startify'

" Movement
Pack 'chaoren/vim-wordmotion'
Pack 'tpope/vim-repeat'
Pack 'justinmk/vim-sneak'
Pack 'unblevable/quick-scope', {'type': 'opt'}

" Tmux and vim split navigation
Pack 'christoomey/vim-tmux-navigator'

" Quickfix
Pack 'romainl/vim-qf' 
Pack 'Olical/vim-enmasse'

" Indentation tracking
Pack 'yggdroot/indentLine'

" Comments
" Pack 'tomtom/tcomment_vim'
Pack 'tpope/vim-commentary'

" Wrapping/delimiters
Pack 'machakann/vim-sandwich'
Pack 'andymass/vim-matchup', {'type': 'opt'}
Pack 'rstacruz/vim-closer'
Pack 'tpope/vim-endwise'

" Splitting/joining
Pack 'AndrewRadev/splitjoin.vim'

" Search
Pack 'romainl/vim-cool'
Pack 'eugen0329/vim-esearch', {'type': 'opt'}

" Yank highlighting
Pack 'machakann/vim-highlightedyank'

" Pattern preview
Pack 'markonm/traces.vim'

" Prettification
Pack 'junegunn/vim-easy-align'
Pack 'sbdchd/neoformat', {'on': 'Neoformat'}

" Text objects
Pack 'wellle/targets.vim'

" Search
" Pack 'junegunn/fzf.vim'
Pack 'liuchengxu/vim-clap', {'do': 'call clap#helper#build_all()'}

" Special symbols
Pack 'chrisbra/unicode.vim', {'on': 'UnicodeSearch'}

" Project Management/Sessions
Pack 'tpope/vim-obsession'
Pack 'dhruvasagar/vim-prosession'

" REPL
Pack 'Vigemus/iron.nvim', {'type': 'opt'}

" Undo tree
Pack 'simnalamburt/vim-mundo'

" Git
Pack 'mhinz/vim-signify'
Pack 'tpope/vim-fugitive'
Pack 'itchyny/vim-gitbranch'

" Completion
Pack 'neoclide/coc-sources'
Pack 'neoclide/coc.nvim', {'do': {-> jobstart('yarn install --frozen-lockfile')}, 'type': 'opt'}

" Snippets
Pack 'honza/vim-snippets'

" Checkers
Pack 'w0rp/ale'

" Language multipack
Pack 'sheerun/vim-polyglot'

" Path changing
Pack 'tpope/vim-apathy'

" C/C++/Python debugging
Pack 'sakhnik/nvim-gdb', {'do': {-> jobstart('./install.sh')}, 'on': ['GdbStart', 'GdbStartLLDB', 'GdbStartPDB']}

" Clojure/Lisps/Scheme
let g:lisps = ['clojure', 'lisp', 'scheme', 'racket', 'jbuild', 'fennel']
Pack 'guns/vim-sexp', {'type': 'opt'}
Pack 'tpope/vim-sexp-mappings-for-regular-people', {'type': 'opt'}
Pack 'vim-scripts/scribble.vim'
Pack 'phmarek/vlime'
Pack 'Olical/conjure', {'for': 'clojure', 'do': {-> jobstart('bin/compile')}}
Pack 'eraserhd/parinfer-rust', {'do': {-> jobstart('cargo build --release')}, 'for': g:lisps}
let g:sexp_filetypes = join(g:lisps, ',')
unlet g:lisps

" Org-mode
Pack 'axvr/org.vim'
Pack 'dhruvasagar/vim-dotoo'

" Vimscript
Pack 'Shougo/neco-vim'
Pack 'neoclide/coc-neco'

" LaTeX
Pack 'lervag/vimtex'

" Meson
Pack 'igankevich/mesonic'

" PDDL
Pack 'PontusPersson/pddl.vim'

" Coq
Pack 'jvoorhis/coq.vim'

" Profiling
Pack 'tweekmonster/startuptime.vim'

" Color scheme
Pack 'wbthomason/vim-nazgul'
" Pack 'bruth/vim-newsprint-theme'
" Pack 'chriskempson/base16-vim'

call plugpac#end()

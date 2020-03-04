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

" Async building & commands
Pack 'tpope/vim-dispatch', {'on': ['Dispatch', 'Make', 'Focus', 'Start']}

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

" Quickfix
" Pack 'romainl/vim-qf' 
Pack 'Olical/vim-enmasse'

" Grepping
Pack 'mhinz/vim-grepper', { 'on': ['Grepper', '<plug>(GrepperOperator)'] }

" Indentation tracking
Pack 'yggdroot/indentLine'

" Comments
Pack 'tomtom/tcomment_vim'

" Wrapping/delimiters
Pack 'machakann/vim-sandwich'
Pack 'andymass/vim-matchup', {'type': 'opt'}
Pack '9mm/vim-closer'
Pack 'tpope/vim-endwise'

" Splitting/joining
" Pack 'AndrewRadev/splitjoin.vim'

" Search
Pack 'romainl/vim-cool'

" Yank and undo highlighting
Pack 'machakann/vim-highlightedyank'
Pack 'machakann/vim-highlightedundo'

" Pattern preview
Pack 'markonm/traces.vim'

" Prettification
Pack 'junegunn/vim-easy-align'
Pack 'sbdchd/neoformat', {'on': 'Neoformat'}

" Text objects
Pack 'wellle/targets.vim'

" Search
Pack 'liuchengxu/vim-clap', {'do': 'Clap install-binary'}

" Special symbols
Pack 'chrisbra/unicode.vim', {'on': ['UnicodeSearch', 'UnicodeName']}

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
Pack 'tpope/vim-rhubarb'
Pack 'itchyny/vim-gitbranch'

" Terminal
Pack 'voldikss/vim-floaterm'

" Completion and linting
Pack 'neoclide/coc.nvim', {'do': ':Start! yarn install --frozen-lockfile', 'type': 'opt'}
Pack 'Shougo/echodoc.vim'

" Symbol jumping
Pack 'pechorin/any-jump.nvim'

" Linting
Pack 'w0rp/ale', {'type': 'opt'}

" Language multipack
Pack 'sheerun/vim-polyglot'

" Path changing
Pack 'tpope/vim-apathy'

" C/C++/Python debugging
" Pack 'sakhnik/nvim-gdb', {'do': {-> jobstart('./install.sh')}, 'on': ['GdbStart', 'GdbStartLLDB', 'GdbStartPDB']}

" Clojure/Lisps/Scheme
let g:lisps = ['clojure', 'lisp', 'scheme', 'racket', 'jbuild', 'fennel']
Pack 'guns/vim-sexp', {'type': 'opt'}
Pack 'tpope/vim-sexp-mappings-for-regular-people', {'type': 'opt'}
Pack 'vim-scripts/scribble.vim'
Pack 'phmarek/vlime'
" Pack 'Olical/conjure', {'for': 'clojure', 'do': {-> jobstart('bin/compile')}}
Pack 'eraserhd/parinfer-rust', {'do': {-> jobstart('cargo build --release')}, 'for': g:lisps}
let g:sexp_filetypes = join(g:lisps, ',')
unlet g:lisps

" Org-mode
Pack 'axvr/org.vim'

" LaTeX
Pack 'lervag/vimtex'

" Meson
Pack 'igankevich/mesonic'

" PDDL
Pack 'PontusPersson/pddl.vim'

" Profiling
Pack 'tweekmonster/startuptime.vim'

" Highlight colors
Pack 'norcalli/nvim-colorizer.lua'

" Color scheme
Pack 'wbthomason/vim-nazgul'
Pack 'bruth/vim-newsprint-theme'
Pack 'chriskempson/base16-vim'

" Markdown
Pack 'iamcco/markdown-preview.nvim', {'do': 'cd app && yarn install'} 

call plugpac#end()

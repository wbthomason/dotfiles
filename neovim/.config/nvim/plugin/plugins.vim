" Plugin installation
if empty(glob('~/.config/nvim/pack/packager/opt/vim-packager'))
  silent !git clone https://github.com/kristijanhusak/vim-packager ~/.config/nvim/pack/packager/opt/vim-packager
  au VimEnter * PackagerInstall
  source ~/.config/nvim/init.vim
endif
let g:lisps = ['clojure', 'lisp', 'scheme', 'racket', 'jbuild', 'fennel']
let g:sexp_filetypes = join(g:lisps, ',')
function! PackagerInit() abort
  packadd vim-packager
  call packager#init()

  " Packager
  call packager#add('kristijanhusak/vim-packager', {'type': 'opt'})

  " Async building & commands
  call packager#add('tpope/vim-dispatch', {'type': 'opt'})

  " Registers
  call packager#add('junegunn/vim-peekaboo')

  " Marks
  call packager#add('kshenoy/vim-signature')

  " Buffer management
  call packager#add('mhinz/vim-sayonara')

  " Startup screen
  call packager#add('mhinz/vim-startify')

  " Movement
  call packager#add('chaoren/vim-wordmotion')
  call packager#add('tpope/vim-repeat')
  call packager#add('justinmk/vim-sneak')
  call packager#add('unblevable/quick-scope', {'type': 'opt'})

  " Quickfix
  " Pack 'romainl/vim-qf' 
  call packager#add('Olical/vim-enmasse')

  " Grepping
  call packager#add('mhinz/vim-grepper', {'type': 'opt'})

  " Indentation tracking
  call packager#add('yggdroot/indentLine')

  " Comments
  call packager#add('tomtom/tcomment_vim')

  " Wrapping/delimiters
  call packager#add('machakann/vim-sandwich')
  call packager#add('andymass/vim-matchup', {'type': 'opt'})
  call packager#add('9mm/vim-closer')
  call packager#add('tpope/vim-endwise')

  " Splitting/joining
  " Pack 'AndrewRadev/splitjoin.vim'

  " Search
  call packager#add('romainl/vim-cool')

  " Yank and undo highlighting
  call packager#add('machakann/vim-highlightedyank')
  call packager#add('machakann/vim-highlightedundo')

  " Pattern preview
  call packager#add('markonm/traces.vim')

  " Prettification
  call packager#add('junegunn/vim-easy-align')
  call packager#add('sbdchd/neoformat')

  " Text objects
  call packager#add('wellle/targets.vim')

  " Search
  call packager#add('liuchengxu/vim-clap', {'do': ':Clap install-binary'})

  " Special symbols
  call packager#add('chrisbra/unicode.vim')

  " Project Management/Sessions
  call packager#add('tpope/vim-obsession')
  call packager#add('dhruvasagar/vim-prosession')

  " REPL
  call packager#add('Vigemus/iron.nvim', {'type': 'opt'})

  " Undo tree
  call packager#add('simnalamburt/vim-mundo')

  " Git
  call packager#add('mhinz/vim-signify')
  call packager#add('tpope/vim-fugitive')
  call packager#add('tpope/vim-rhubarb')
  call packager#add('itchyny/vim-gitbranch')

  " Terminal
  call packager#add('voldikss/vim-floaterm')

  " Completion and linting
  call packager#add('neoclide/coc.nvim', {'do': funcref('StartBuild', ['yarn install --frozen-lockfile']), 'type': 'opt'})
  call packager#add('Shougo/echodoc.vim')

  " Symbol jumping
  call packager#add('pechorin/any-jump.nvim')

  " Linting
  call packager#add('w0rp/ale', {'type': 'opt'})

  " Language multipack
  call packager#add('sheerun/vim-polyglot')

  " Path changing
  call packager#add('tpope/vim-apathy')

  " C/C++ semantic highlighting
  call packager#add('jackguo380/vim-lsp-cxx-highlight')

  " C/C++/Python debugging
  " Pack 'sakhnik/nvim-gdb', {'do': {-> jobstart('./install.sh')}, 'on': ['GdbStart', 'GdbStartLLDB', 'GdbStartPDB']}

  " Clojure/Lisps/Scheme
  call packager#add('guns/vim-sexp', {'type': 'opt'})
  call packager#add('tpope/vim-sexp-mappings-for-regular-people', {'type': 'opt'})
  call packager#add('vim-scripts/scribble.vim')
  call packager#add('phmarek/vlime')
  " Pack 'Olical/conjure', {'for': 'clojure', 'do': {-> jobstart('bin/compile')}}
  call packager#add('eraserhd/parinfer-rust', {'do': funcref('StartBuild', ['cargo build --release']), 'type': 'opt'})

  " Org-mode
  call packager#add('axvr/org.vim')

  " LaTeX
  call packager#add('lervag/vimtex')

  " Meson
  call packager#add('igankevich/mesonic')

  " PDDL
  call packager#add('PontusPersson/pddl.vim')

  " Profiling
  call packager#add('tweekmonster/startuptime.vim')

  " Highlight colors
  call packager#add('norcalli/nvim-colorizer.lua')

  " Color scheme
  call packager#add('wbthomason/vim-nazgul')
  call packager#add('bruth/vim-newsprint-theme')
  call packager#add('chriskempson/base16-vim')
  call packager#add('hardselius/warlock')

  " Markdown
  call packager#add('iamcco/markdown-preview.nvim', {'do': funcref('StartBuild', ['cd app && yarn install'])})

  " Tags
  call packager#add('ludovicchabant/vim-gutentags', {'type': 'opt'})

  " Wiki
  call packager#add('lervag/wiki.vim')

  " TODO
  call packager#add('https://gitlab.com/dbeniamine/todo.txt-vim')
endfunction

command! PackagerInstall call PackagerInit() | call packager#install()
command! -bang PackagerUpdate call PackagerInit() | call packager#update({ 'force_hooks': '<bang>' })
command! PackagerClean call PackagerInit() | call packager#clean()
command! PackagerStatus call PackagerInit() | call packager#status()

function! StartBuild(cmd, plugin) abort
  execute ':Start! -dir=' . a:plugin.dir . ' ' . a:cmd
endfunction

augroup packager_filetype
  au!
augroup END

execute 'autocmd packager_filetype FileType ' . g:sexp_filetypes . '++once packadd parinfer-rust'

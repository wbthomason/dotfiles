" Plugin installation
if empty(glob('~/.config/nvim/pack/packager/opt/vim-packager'))
  silent !git clone https://github.com/kristijanhusak/vim-packager ~/.config/nvim/pack/packager/opt/vim-packager
  au VimEnter * ++once PackagerInstall
  source ~/.config/nvim/init.vim
endif
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
  call packager#add('Yilin-Yang/vim-markbar', {'type': 'opt'})

  " Buffer management
  call packager#add('mhinz/vim-sayonara')

  " Startup screen
  call packager#add('mhinz/vim-startify')

  " Movement
  call packager#add('chaoren/vim-wordmotion')
  call packager#add('tpope/vim-repeat')
  call packager#add('justinmk/vim-sneak')
  " call packager#add('unblevable/quick-scope', {'type': 'opt'})

  " Quickfix
  " Pack 'romainl/vim-qf' 
  call packager#add('Olical/vim-enmasse', {'type': 'opt'})

  " Grepping
  call packager#add('mhinz/vim-grepper', {'type': 'opt'})

  " Indentation tracking
  call packager#add('yggdroot/indentLine')

  " Commenting
  call packager#add('tomtom/tcomment_vim')

  " Wrapping/delimiters
  call packager#add('machakann/vim-sandwich')
  call packager#add('andymass/vim-matchup', {'type': 'opt'})
  call packager#add('9mm/vim-closer')
  call packager#add('tpope/vim-endwise')

  " Search
  call packager#add('romainl/vim-cool')

  " Pattern preview
  " call packager#add('markonm/traces.vim')

  " Prettification
  call packager#add('junegunn/vim-easy-align')
  call packager#add('sbdchd/neoformat')

  " Text objects
  call packager#add('wellle/targets.vim')
  call packager#add('PeterRincker/vim-argumentative')

  " Search
  call packager#add('liuchengxu/vim-clap', {'do': ':Clap install-binary'})

  " Special symbols
  " call packager#add('chrisbra/unicode.vim', {'type': 'opt'})

  " Project Management/Sessions
  call packager#add('tpope/vim-obsession', {'type': 'opt'})
  call packager#add('dhruvasagar/vim-prosession', {'type': 'opt'})

  " REPL
  call packager#add('Vigemus/iron.nvim', {'type': 'opt'})

  " Undo tree
  call packager#add('simnalamburt/vim-mundo', {'type': 'opt'})

  " Git
  call packager#add('mhinz/vim-signify')
  call packager#add('tpope/vim-fugitive', {'type': 'opt'})
  call packager#add('tpope/vim-rhubarb')
  call packager#add('itchyny/vim-gitbranch')

  " Terminal
  " call packager#add('voldikss/vim-floaterm')

  " Completion and linting
  " call packager#add('neoclide/coc.nvim', {'do': funcref('StartBuild', ['yarn install --frozen-lockfile']), 'type': 'opt'})
  " call packager#add('Shougo/echodoc.vim')

  call packager#add('neovim/nvim-lsp', {'type': 'opt'})
  call packager#local('~/projects/personal/lsp-status.nvim')
  call packager#add('haorenW1025/completion-nvim', {'type': 'opt'})
  call packager#local('~/projects/personal/hover.nvim')
  call packager#add('haorenW1025/diagnostic-nvim', {'type': 'opt'})
  call packager#add('hrsh7th/vim-vsnip', {'type': 'opt'})
  call packager#add('hrsh7th/vim-vsnip-integ', {'type': 'opt'})

  " Linting
  call packager#add('w0rp/ale', {'type': 'opt'})

  " Language multipack
  call packager#add('sheerun/vim-polyglot')

  " Path changing
  call packager#add('tpope/vim-apathy')

  " C/C++ semantic highlighting
  " call packager#add('jackguo380/vim-lsp-cxx-highlight')

  " Clojure/Lisps/Scheme
  call packager#add('guns/vim-sexp', {'type': 'opt'})
  call packager#add('tpope/vim-sexp-mappings-for-regular-people', {'type': 'opt'})
  call packager#add('phmarek/vlime')
  call packager#add('Olical/conjure', {'type': 'opt'})
  call packager#add('eraserhd/parinfer-rust', {'do': funcref('StartBuild', ['cargo build --release']), 'type': 'opt'})

  " LaTeX
  call packager#add('lervag/vimtex')

  " Meson
  " call packager#add('igankevich/mesonic')

  " PDDL
  call packager#add('PontusPersson/pddl.vim')

  " Profiling
  call packager#add('dstein64/vim-startuptime', {'type': 'opt'})

  " Highlight colors
  call packager#add('norcalli/nvim-colorizer.lua')

  " Color scheme
  call packager#local('~/projects/personal/vim-nazgul')
  " call packager#add('chriskempson/base16-vim')
  " call packager#add('hardselius/warlock')
  " call packager#add('arzg/vim-substrata')
  call packager#add('tjdevries/colorbuddy.vim')

  " Markdown
  call packager#add('iamcco/markdown-preview.nvim', {'do': funcref('StartBuild', ['cd app && yarn install'])})

  " Tags
  " call packager#add('ludovicchabant/vim-gutentags', {'type': 'opt'})

  " Wiki
  call packager#add('lervag/wiki.vim', {'type': 'opt'})

  " Notes
  call packager#local('~/projects/personal/pdf-scribe.nvim')

  " Tasks
  call packager#local('~/projects/personal/todoist-import.nvim')
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

let g:sexp_filetypes = join(['clojure', 'lisp', 'scheme', 'racket', 'jbuild', 'fennel', 'pddl'], ',')
execute 'autocmd packager_filetype FileType ' . g:sexp_filetypes . '++once packadd parinfer-rust'

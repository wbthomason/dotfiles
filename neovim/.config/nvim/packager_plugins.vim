" Plugin installation
if empty(glob('~/.config/nvim/pack/packager/opt/vim-packager'))
  silent !git clone https://github.com/kristijanhusak/vim-packager ~/.config/nvim/pack/packager/opt/vim-packager
  augroup install_plugins
    au!
    au VimEnter * PackagerInstall
  augroup end
  source ~/.config/nvim/init.vim
endif

function! PackagerInit() abort
  packadd vim-packager
  call packager#init()
" vim-packager
call packager#add('kristijanhusak/vim-packager', {'type': 'opt'})

" Async building & commands
call packager#add('tpope/vim-dispatch')
" call packager#add('radenling/vim-dispatch-neovim')

" Moonscript
" call packager#add('svermeulen/nvim-moonmaker')

" Tweaks
call packager#add('vim-scripts/LargeFile')

" QuickFix help
" call packager#add('romainl/vim-qf')

" Mappings
call packager#add('liuchengxu/vim-which-key')

" Registers
call packager#add('junegunn/vim-peekaboo')

" Marks
call packager#add('kshenoy/vim-signature')

" Buffer management
call packager#add('mhinz/vim-sayonara', {'type': 'opt'})

" Startup screen
call packager#add('mhinz/vim-startify')

" Status line
call packager#add('itchyny/lightline.vim')
call packager#add('maximbaz/lightline-ale')

" Movement
call packager#add('chaoren/vim-wordmotion')
call packager#add('tpope/vim-repeat')
call packager#add('justinmk/vim-sneak')

" Tmux and vim split navigation
call packager#add('christoomey/vim-tmux-navigator')

" Indentation tracking
call packager#add('yggdroot/indentLine')

" Comments
call packager#add('tomtom/tcomment_vim')

" Wrapping/delimiters
call packager#add('machakann/vim-sandwich')
call packager#add('luochen1990/rainbow')
" call packager#add('itchyny/vim-parenmatch')
" call packager#add('djdt/vim-matchparenalways')
call packager#add('andymass/vim-matchup')
call packager#add('rstacruz/vim-closer')
call packager#add('tpope/vim-endwise')
call packager#add('machakann/vim-swap')

" Search
call packager#add('romainl/vim-cool')

" Yank highlighting
call packager#add('machakann/vim-highlightedyank')

" Prettification
call packager#add('junegunn/vim-easy-align')
call packager#add('sbdchd/neoformat', {'type': 'opt'})

" Text objects
call packager#add('wellle/targets.vim')

" FZF
call packager#add('junegunn/fzf.vim')
call packager#add('justinhoward/fzf-neoyank', {'type': 'opt'})
call packager#add('Shougo/neoyank.vim')

" Special symbols
call packager#add('chrisbra/unicode.vim', {'type': 'opt'})

" Project Management/Sessions
call packager#add('tpope/vim-obsession', {'type': 'opt'})
call packager#add('dhruvasagar/vim-prosession', {'type': 'opt'})

" REPL
call packager#add('Vigemus/iron.nvim')

" Undo tree
call packager#add('simnalamburt/vim-mundo', {'type': 'opt'})

" File explorer
call packager#add('cocopon/vaffle.vim')

" Git
call packager#add('mhinz/vim-signify')
" call packager#add('tpope/vim-fugitive', {'on': ['Gstatus', 'Gpush', 'Gpull', 'Gdiff']})
call packager#add('jreybert/vimagit', {'type': 'opt'})
call packager#add('itchyny/vim-gitbranch')

" Completion
call packager#add('Shougo/echodoc.vim')
call packager#add('jsfaint/coc-neoinclude')
call packager#add('neoclide/coc-sources')
call packager#add('neoclide/coc.nvim', {'do': function('InstallCoc')})
" call packager#add('Shougo/neco-syntax')
call packager#add('Shougo/neoinclude.vim')

" Snippets
call packager#add('honza/vim-snippets')

" Checkers
call packager#add('w0rp/ale')

" Language multipack
call packager#add('sheerun/vim-polyglot')

" Path changing
call packager#add('tpope/vim-apathy')

" C/C++/Python debugging
call packager#add('sakhnik/nvim-gdb', {'do': './install.sh', 'type': 'opt'})

" Clojure/Lisps/Scheme
call packager#add('guns/vim-sexp', {'type': 'opt'})
call packager#add('tpope/vim-sexp-mappings-for-regular-people', {'type': 'opt'})
call packager#add('vim-scripts/scribble.vim')
call packager#add('kovisoft/slimv')
" call packager#add('l04m33/vlime', {'rtp': 'vim/', 'for': 'lisp'})

" Pandoc/Markdown
call packager#add('vim-pandoc/vim-pandoc')
call packager#add('vim-pandoc/vim-pandoc-syntax')
call packager#add('vim-pandoc/vim-pandoc-after')

" Org-mode
" call packager#add('vim-scripts/utl.vim', {'for': 'org'})
" call packager#add('itchyny/calendar.vim', {'for': 'org'})
" call packager#add('tpope/vim-speeddating', {'for': 'org'})
" call packager#add('chrisbra/NrrwRgn', {'for': 'org'})
" call packager#add('inkarkat/vim-SyntaxRange', {'for': 'org'})
" call packager#add('jonathanbranam/vim-orgmode')
" call packager#add('wbthomason/capture.vim')
" call packager#add('/home/wil/projects/personal/orgmode.nvim')

" Vimscript
call packager#add('Shougo/neco-vim')
call packager#add('neoclide/coc-neco')

" LaTeX
call packager#add('lervag/vimtex')

" Meson
call packager#add('stfl/meson.vim')
call packager#add('igankevich/mesonic')

" PDDL
call packager#add('PontusPersson/pddl.vim')

" Coq
call packager#add('jvoorhis/coq.vim')

" Profiling
call packager#add('tweekmonster/startuptime.vim')

" Color scheme
call packager#add('wbthomason/vim-nazgul')
call packager#add('logico-software/typewriter')
call packager#add('bruth/vim-newsprint-theme')
endfunction

command! PackagerInstall call PackagerInit() | call packager#install()
command! -bang PackagerUpdate call PackagerInit() | call packager#update({ 'force_hooks': '<bang>' })
command! PackagerClean call PackagerInit() | call packager#clean()
command! PackagerStatus call PackagerInit() | call packager#status()

set nocompatible
filetype off

" Plugins
call plug#begin()

" Git
Plug 'tpope/vim-fugitive'

" Completion
Plug 'benekastah/neomake'
Plug 'rdnetto/YCM-Generator', {'branch': 'stable'}
Plug 'valloric/YouCompleteMe'
Plug 'scrooloose/syntastic', {'for': ['OCaml', 'clojure']}

" Rust
Plug 'rust-lang/rust.vim', {'for': 'rust'}

" Clojure
Plug 'guns/vim-clojure-static', {'for': 'clojure'}
Plug 'tpope/vim-fireplace', {'for': 'clojure'}
Plug 'tpope/vim-salve', {'for': 'clojure'}
 Plug 'guns/vim-sexp', {'for': 'clojure'}
 Plug 'tpope/vim-sexp-mappings-for-regular-people', {'for': 'clojure'}
Plug 'guns/vim-clojure-highlight', {'for': 'clojure'}
Plug 'venantius/vim-eastwood', {'for': 'clojure'}
Plug 'venantius/vim-cljfmt', {'for': 'clojure'}

" Pandoc/Markdown
Plug 'vim-pandoc/vim-pandoc', {'for': 'markdown'}
Plug 'vim-pandoc/vim-pandoc-syntax', {'for': 'markdown'}

" JS
Plug 'marijnh/tern_for_vim', {'for': 'javascript'}
Plug 'pangloss/vim-javascript', {'for': 'javascript'}
Plug 'wookiehangover/jshint.vim', {'for': 'javascript'}

" Utilities
Plug 'SirVer/ultisnips'
Plug 'airblade/vim-rooter'
Plug 'bling/vim-airline'
Plug 'easymotion/vim-easymotion'
Plug 'honza/vim-snippets'
Plug 'jiangmiao/auto-pairs'
Plug 'kien/ctrlp.vim'
Plug 'kien/rainbow_parentheses.vim'
Plug 'nathanaelkane/vim-indent-guides'
Plug 'rking/ag.vim'
Plug 'scrooloose/nerdcommenter'
Plug 'scrooloose/nerdtree'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
" Plug 'vim-scripts/paredit.vim'
Plug 'whatyouhide/vim-gotham'
Plug 'xuyuanp/nerdtree-git-plugin'
Plug 'yggdroot/indentLine'

" Coffeescript
Plug 'kchmck/vim-coffee-script', {'for': 'coffeescript'}

" Jade
Plug 'digitaltoad/vim-jade', {'for': 'jade'}

" Less
Plug 'groenewege/vim-less', {'for': 'less'}

" Elm
Plug 'lambdatoast/elm.vim', {'for': 'elm'}

" OCaml
Plug 'the-lambda-church/merlin', {'for': 'ocaml'}
Plug 'OCamlPro/ocp-indent', {'for': 'ocaml'}

" LaTeX
Plug 'lervag/vimtex', {'for': 'latex'}

call plug#end()

filetype plugin indent on

" Custom sequence bindings
let mapleader = "\<Space>"
nnoremap <Leader>hh :nohl<CR>
nnoremap <Leader>o :CtrlP<CR>
nnoremap <Leader>w :w<CR>
nnoremap <Leader>q :q<CR>
nnoremap <Leader>x :x<CR>
nnoremap <Leader>g :bn<CR>
nnoremap <Leader>t :bp<CR>
nnoremap <Leader>d :bd<CR>
nnoremap <Leader>e :enew<CR>:CtrlP<CR>
nnoremap <leader>f 1z=
nnoremap <leader>s :set spell!

" General settings
set title
set wildmenu
set autoread
set tw=80
set formatoptions+=t
set so=7
set wildignore=*.o,*~,*.pyc
set backspace=eol,start,indent
set whichwrap+=<,>,h,l
set incsearch
set lazyredraw
set magic
set showmatch
set noerrorbells
set novisualbell
set ignorecase
set smartcase
syntax enable
colorscheme gotham
set background=dark
set expandtab
set smarttab
set shiftwidth=2
set tabstop=2
set number
set ai
set si
set wrap
set laststatus=2 " Always display the statusline in all windows
set showtabline=2 " Always display the tabline, even if there is only one tab
set noshowmode " Hide the default mode text (e.g. -- INSERT -- below the statusline)
set viminfo^=%
set hidden

" Eclim settings
let g:EclimCompletionMethod = 'omnifunc'

" Rainbow parens settings
let g:rbpt_colorpairs = [
    \ ['brown',       'RoyalBlue3'],
    \ ['Darkblue',    'SeaGreen3'],
    \ ['darkgray',    'DarkOrchid3'],
    \ ['darkgreen',   'firebrick3'],
    \ ['darkcyan',    'RoyalBlue3'],
    \ ['darkred',     'SeaGreen3'],
    \ ['darkmagenta', 'DarkOrchid3'],
    \ ['brown',       'firebrick3'],
    \ ['gray',        'RoyalBlue3'],
    \ ['black',       'SeaGreen3'],
    \ ['darkmagenta', 'DarkOrchid3'],
    \ ['Darkblue',    'firebrick3'],
    \ ['darkgreen',   'RoyalBlue3'],
    \ ['darkcyan',    'SeaGreen3'],
    \ ['darkred',     'DarkOrchid3'],
    \ ['red',         'firebrick3'],
    \ ]

let g:rbpt_max = 16
let g:rbpt_loadcmd_toggle = 0

" Airline settings
let g:airline_powerline_fonts = 1
" Enable the list of buffers
let g:airline#extensions#tabline#enabled = 1
" Show just the filename
let g:airline#extensions#tabline#fnamemod = ':t'

" CtrlP settings
if executable('ag')
  " Use Ag over Grep
  set grepprg=ag\ --nogroup\ --nocolor
  let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
endif

" Madoko build function
function! BuildMadoko(filename)
  let job1 = jobstart(['madoko', '--pdf', a:filename])
endfunction

" Autocommands
au BufReadPost *
	\ if line("'\"") > 0 && line("'\"") <= line("$") |
	\ exe "normal! g`\"" |
	\ endif
au BufWinEnter * checktime
au VimEnter * RainbowParenthesesToggle
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadSquare
au Syntax * RainbowParenthesesLoadBraces
au BufWritePost * Neomake
au BufWritePost *.mdk :call BuildMadoko(expand("%"))
au BufRead * Neomake
au CompleteDone * pclose
au BufNewFile,BufReadPost *.md set filetype=markdown

" Opam/OCaml settings
let g:opamshare = substitute(system('opam config var share'),'\n$','','''')
execute "set rtp+=" . g:opamshare . "/merlin/vim"
set rtp^="/usr/local/share/ocp-indent/vim"

" Racer/Rust settings
let g:racer_cmd = "/usr/bin/racer"
let $RUST_SRC_PATH="/home/wil/rust/src/"

" YCM Settings
if !exists('g:ycm_semantic_triggers')
  let g:ycm_semantic_triggers = {}
endif
let g:ycm_semantic_triggers.tex = ['re!\\[A-Za-z]*(ref|cite)[A-Za-z]*([^]]*])?{([^}]*, ?)*']

" Neomake settings
let g:neomake_cpp_clang_args = ['-std=c++14']

" IndentLine settings
let g:indentLine_color_term = 239
let g:indentLine_color_gui = '#09AA08'
let g:indentLine_char = '│'

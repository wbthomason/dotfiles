" General settings

set title
set wildmenu
set autoread
set autochdir
set textwidth=100
set formatoptions+=t
set formatoptions+=j
set scrolloff=7
set wildignore=*.o,*~,*.pyc
set backspace=eol,start,indent
set whichwrap+=<,>,h,l
set incsearch
set inccommand=nosplit
set lazyredraw
set magic
set showmatch
set noerrorbells
set novisualbell
set ignorecase
set smartcase
let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
set tabstop=4
set softtabstop=0
set expandtab
set shiftwidth=2
set smarttab
set number
set relativenumber
set autoindent
set smartindent
set wrap
set laststatus=2 " Always display the statusline in all windows
set showtabline=2 " Always display the tabline, even if there is only one tab
set noshowmode " Hide the default mode text (e.g. -- INSERT -- below the statusline)
set viminfo^=%
set hidden
set nofoldenable
set guifont=Fura\ Code\ Retina\ Nerd\ Font\ Complete\ 7
set encoding=utf-8
set ttyfast
set shortmess+=c
set guicursor=n-v-c:block,i-ci-ve:ver25,r-cr:hor20,o:hor50
set completeopt+=preview
filetype plugin indent on
syntax enable

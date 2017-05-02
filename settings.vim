" General settings

set title
set wildmenu
set autoread
set autochdir
set tw=100
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
set termguicolors
let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
set tabstop=4
set softtabstop=0
set expandtab
set shiftwidth=2
set smarttab
set number
set relativenumber
set ai
set si
set wrap
set laststatus=2 " Always display the statusline in all windows
set showtabline=2 " Always display the tabline, even if there is only one tab
set noshowmode " Hide the default mode text (e.g. -- INSERT -- below the statusline)
set viminfo^=%
set hidden
set bg=dark
set nofoldenable
set guicursor=n-v-c-sm:block-blinkon1,i-ci-ve:ver25-blinkon1,r-cr-o:hor20-blinkon1

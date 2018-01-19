" Keybindings

" Set the leader keys
let mapleader = "\<space>"
let maplocalleader = ","

" More convenient ESC
inoremap jj <ESC>

" Setup descriptions
let g:lmaps = {}
let g:llmaps = {}
augroup local_filetype_guides
  au!
  au FileType tex let g:llmaps.l = { 'name' : 'vimtex' }
augroup END

" Exiting
nnoremap <leader>q :qa<CR>
let g:lmaps.q = ['q', 'Quit']
nnoremap <leader>x :x<CR>
let g:lmaps.x = ['x', 'Write and quit']

" Buffer bindings
let g:lmaps.b = {'name': 'Buffers'}
nnoremap <leader>w :w<CR>
let g:lmaps.w = ['w', 'Write buffer']
nnoremap <leader>d :Sayonara<CR>
let g:lmaps.d = ['Sayonara', 'Close buffer']
nnoremap <leader>a :bp<CR>
let g:lmaps.a = ['bp', 'Go to previous buffer']
nnoremap <leader>s :bn<CR> 
let g:lmaps.s = ['bn', 'Go to next buffer']
nnoremap <leader>bl :b#<CR>
let g:lmaps.b.l = ['b#', 'Go to last used buffer']
nnoremap <leader>bb :Buffers<CR>
let g:lmaps.b.b = ['Buffers', 'Select a buffer']

" Configuration bindings
let g:lmaps.c = {'name': 'Configuration'}
let g:lmaps.c.s = {'name': 'Sourcing'}
nnoremap <leader>cst :so %<CR>
let g:lmaps.c.s.t = ['so %', 'Source this file']
nnoremap <leader>csc :so ~/.config/nvim/init.vim<CR>
let g:lmaps.c.s.c = ['so ~/.config/nvim/init.vim', 'Source config file']
nnoremap <leader>cc :e ~/.config/nvim/init.vim<CR>
let g:lmaps.c.c = ['e ~/.config/nvim/init.vim', 'Open Neovim config']

" Error bindings
let g:lmaps.e = {'name': 'Errors'}
nnoremap <leader>eo :lopen<CR>
let g:lmaps.e.c = ['lopen', 'Open error list']
nnoremap <leader>ec :lclose<CR>
let g:lmaps.e.c = ['lclose', 'Close error list']

" Finder bindings
let g:lmaps.f = {'name': 'Find'}
nnoremap <leader>ff :Files<CR>
let g:lmaps.f.f = ['Files', 'Search for files in the current directory']
nnoremap <leader>fa :Files <C-r>=getcwd()<CR>
let g:lmaps.f.a = ['Files <C-r>=getcwd()', 'Search for files in an arbitrary directory']
nnoremap <leader>fg :GFiles<CR>
let g:lmaps.f.g = ['GFiles', 'Search for files in the current Git repo']
nnoremap <leader>fh :Helptags<CR>
let g:lmaps.f.h = ['Helptags', 'Search in help tags']
nnoremap <leader>fr :History<CR>
let g:lmaps.f.r = ['History', 'Search for recently edited files']
let g:lmaps.f.t = {'name': 'Tags'}
nnoremap <leader>ftt :Tags<CR>
let g:lmaps.f.t.t = ['Files', 'Search in tags for the current directory']
nnoremap <leader>ftb :BTags<CR>
let g:lmaps.f.t.b = ['BTags', 'Search in tags for the current buffer']
nnoremap <leader>fi :Rg 
let g:lmaps.f.i = ['Rg', 'Search for text in files in the current directory']
nnoremap <leader>fl :Lines<CR>
let g:lmaps.f.l = ['Lines', 'Search for text in currently open buffers']
let g:lmaps.f.s = {'name': 'Symbols'}
nnoremap <silent> <leader>fsb :call LanguageClient_textDocument_documentSymbol()<CR>
let g:lmaps.f.s.b = ['call LanguageClient_textDocument_documentSymbol()', 'Search symbols in the current buffer']
nnoremap <silent> <leader>fsp :call LanguageClient_workspace_symbol()<CR>
let g:lmaps.f.s.p = ['call LanguageClient_workspace_symbol()', 'Search symbols in the current project']
nnoremap <leader>fd :Vaffle<CR>
let g:lmaps.f.d = ['Vaffle', 'Open Vaffle']

" Toggles
let g:lmaps.t = {'name': 'Toggles'}
nnoremap <leader>th :nohls<CR>
let g:lmaps.t.h = ['nohls', 'Reset search highlights']
nnoremap <leader>tt :TagbarToggle<CR>
let g:lmaps.t.t = ['TagbarToggle', 'Toggle Tagbar split']
nnoremap <leader>tu :UndotreeToggle<CR>
let g:lmaps.t.u = ['UndotreeToggle', 'Toggle Undotree split']
nnoremap <leader>tw :call Toggle_writer_mode()<CR>
let g:lmaps.t.w = ['call Toggle_writer_mode()', 'Toggle writer mode']

" Incsearch bindings
map /  <Plug>(incsearch-forward)
map ?  <Plug>(incsearch-backward)
map g/ <Plug>(incsearch-stay)
map n  <Plug>(incsearch-nohl-n)
map N  <Plug>(incsearch-nohl-N)
map *  <Plug>(incsearch-nohl-*)
map #  <Plug>(incsearch-nohl-#)
map g* <Plug>(incsearch-nohl-g*)
map g# <Plug>(incsearch-nohl-g#)

" Version control bindings
let g:lmaps.g = {'name': 'Git'}
nnoremap <leader>gw :Gwrite<CR>
let g:lmaps.g.w = ['Gwrite', 'Write and stage buffer']
nnoremap <leader>gm :Magit<CR>
let g:lmaps.g.m = ['Magit', 'Start Vimagit']
nnoremap <leader>gc :Gcommit<CR>
let g:lmaps.g.c = ['Gcommit', 'Create a git commit']
nnoremap <leader>gl :Gpull<CR>
let g:lmaps.g.l = ['Gpull', 'Run git pull']
nnoremap <leader>gp :Gpush<CR>
let g:lmaps.g.p = ['Gpush', 'Run git push']
nnoremap <leader>gd :Gdiff<CR>
let g:lmaps.g.d = ['Gdiff', 'Display git diff']
nnoremap <leader>gs :Gstatus<CR>
let g:lmaps.g.s = ['Gstatus', 'Display git status']

" Compilation bindings
let g:lmaps.m = {'name': 'Build/Make'}
nnoremap <leader>mi :BuildIt<CR>
let g:lmaps.m.i = ['BuildIt', 'Run BuildIt for the current buffer']
nnoremap <leader>ms :BuildItStatus<CR>
let g:lmaps.m.s = ['BuildItStatus', 'Check the BuildIt status']

" REPL and Terminal bindings
tnoremap jj <C-\><C-n>
nnoremap <leader>r :IronRepl<CR>
let g:lmaps.r = ['IronRepl', 'Start the REPL for the current filetype']

" Intero bindings
let g:lmaps.h = {'name': 'Haskell'}
nnoremap <leader>hio :InteroOpen<CR>
let g:lmaps.h.i = {'name': 'Intero'}
let g:lmaps.h.i.o = ['InteroOpen', 'Open Intero']
nnoremap <leader>hik :InteroKill<CR>
let g:lmaps.h.i.k = ['InteroKill', 'Kill Intero']
nnoremap <leader>hic :InteroHide<CR>
let g:lmaps.h.i.c = ['InteroHide', 'Hide Intero']
nnoremap <leader>hil :InteroLoadCurrentModule<CR>
let g:lmaps.h.i.l = ['InteroLoadCurrentModule', 'Load current module in Intero']
nnoremap <leader>hie :InteroEval<CR>
let g:lmaps.h.i.e = ['InteroEval', 'Eval in Intero']
nnoremap <leader>hit :InteroGenericType<CR>
let g:lmaps.h.i.t = ['InteroGenericType', 'Get generic type from Intero']
nnoremap <leader>hiT :InteroType<CR>
let g:lmaps.h.i.T = ['InteroType', 'Get type from Intero']
nnoremap <leader>hii :InteroInfo<CR>
let g:lmaps.h.i.i = ['InteroInfo', 'Get info from Intero']
nnoremap <leader>hiI :InteroTypeInsert<CR>
let g:lmaps.h.i.I = ['InteroTypeInsert', 'Insert type from Intero']
nnoremap <leader>hid :InteroGoToDef<CR>
let g:lmaps.h.i.d = ['InteroGoToDef', 'Go to definition from Intero']
nnoremap <leader>hiu :InteroUses<CR>
let g:lmaps.h.i.u = ['InteroUses', 'Find uses with Intero']

" LanguageClient bindings
let g:lmaps.l = {'name': 'LSP'}
nnoremap <silent> K :call LanguageClient_textDocument_hover()<CR>
nnoremap <silent> gd :call LanguageClient_textDocument_definition()<CR>
nnoremap <silent> <leader>ln :call LanguageClient_textDocument_rename()<CR>
let g:lmaps.l.n = ['call LanguageClient_textDocument_rename()', 'Rename symbol']
nnoremap <silent> <leader>lr :call LanguageClient_textDocument_references()<CR>
let g:lmaps.l.r = ['call LanguageClient_textDocument_references()', 'Find symbol references']

" Formatting Bindings
nnoremap <leader> bf :Neoformat<CR>
let g:lmaps.b.f = ['Neoformat', 'Format buffer with Neoformat']

" Easy-Align bindings
" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

" Vimwiki bindings
nmap <LocalLeader>wi <Plug>VimwikiIndex
nmap <LocalLeader>wt <Plug>VimwikiTabIndex 
nmap <LocalLeader>ws <Plug>VimwikiUISelect
nmap <LocalLeader>ji <Plug>VimwikiDiaryIndex
nmap <LocalLeader>j  <Plug>VimwikiMakeDiaryNote
nmap <LocalLeader>jt <Plug>VimwikiTabMakeDiaryNote
nmap <LocalLeader>jy <Plug>VimwikiMakeYesterdayDiaryNote
nmap <LocalLeader>jl <Plug>VimwikiDiaryGenerateLinks

" Schlepp bindings
vmap <unique> <up>    <Plug>SchleppUp
vmap <unique> <down>  <Plug>SchleppDown
vmap <unique> <left>  <Plug>SchleppLeft
vmap <unique> <right> <Plug>SchleppRight

" Leader guide bindings and settings
let g:allmaps = {}
let g:allmaps[' '] = g:lmaps
let g:allmaps[','] = g:llmaps
let g:allmaps[',']['name'] = '<localleader>'

call leaderGuide#register_prefix_descriptions('', 'g:allmaps')
call leaderGuide#register_prefix_descriptions(',', 'g:llmaps')
nnoremap <silent> <leader> :<c-u>LeaderGuide '<Space>'<CR>
vnoremap <silent> <leader> :<c-u>LeaderGuideVisual '<Space>'<CR>
map <leader>. <Plug>leaderguide-global
nnoremap <localleader> :<c-u>LeaderGuide  ','<CR>
vnoremap <localleader> :<c-u>LeaderGuideVisual  ','<CR>
map <localleader>. <Plug>leaderguide-buffer

" vim-snipe bindings
map <leader><leader>F <Plug>(snipe-F)
map <leader><leader>f <Plug>(snipe-f)
map <leader><leader>T <Plug>(snipe-T)
map <leader><leader>t <Plug>(snipe-t)
map <leader><leader>w <Plug>(snipe-w)
map <leader><leader>W <Plug>(snipe-W)
map <leader><leader>e <Plug>(snipe-e)
map <leader><leader>E <Plug>(snipe-E)
map <leader><leader>b <Plug>(snipe-b)
map <leader><leader>B <Plug>(snipe-B)
map <leader><leader>ge <Plug>(snipe-ge)
map <leader><leader>gE <Plug>(snipe-gE)
nmap <leader><leader>] <Plug>(snipe-f-xp)
nmap <leader><leader>[ <Plug>(snipe-f-xp)
nmap <leader><leader>x <Plug>(snipe-f-x)
nmap <leader><leader>X <Plug>(snipe-F-x)
nmap <leader><leader>r <Plug>(snipe-f-r)
nmap <leader><leader>R <Plug>(snipe-F-r)

" Make Backspace and Enter more useful
nnoremap <BS> {
onoremap <BS> {
vnoremap <BS> {

nnoremap <expr> <CR> empty(&buftype) ? '}' : '<CR>'
onoremap <expr> <CR> empty(&buftype) ? '}' : '<CR>'
vnoremap <CR> }

" NCM bindings
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
" inoremap <expr> <CR>  pumvisible() ?  "\<c-y>\<Plug>(expand_or_nl)" : "\<CR>"
" inoremap <expr> <Plug>(expand_or_nl) cm#completed_is_snippet() ? "\<C-U>":"\<CR>"

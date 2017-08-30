" Keybindings

" Set the leader keys
let mapleader = "\<space>"
let maplocalleader = "\<cr>"

" General editing bindings
nnoremap <leader>th :nohl<CR>
nnoremap <leader>w :w<CR>
nnoremap <leader>q :qa<CR>
nnoremap <leader>x :x<CR>
nnoremap <leader>d :Sayonara<CR>
nnoremap <leader>eo :lopen<CR>
nnoremap <leader>ec :lclose<CR>

" Navigation bindings
nnoremap <leader>a :bp<CR>
nnoremap <leader>s :bn<CR> 
nnoremap <leader>bl :b#<CR>
nnoremap <leader>tt :TagbarToggle<CR>
nnoremap <leader>tf :VimFilerSplit<CR>
nnoremap <leader>tu :UndotreeToggle<CR>

" Incsearch bindings
map /  <Plug>(incsearch-forward)
map ?  <Plug>(incsearch-backward)
map g/ <Plug>(incsearch-stay)

" Configuration bindings
nnoremap <leader>o :so %<CR>
nnoremap <leader>mc :e ~/.config/nvim/init.vim<CR>

" Search bindings
nnoremap <leader>ff :Files<CR>
nnoremap <leader>fa :Files <C-r>=getcwd()<CR>
nnoremap <leader>fg :GFiles<CR>
nnoremap <leader>fh :Helptags<CR>
" nnoremap <leader>bb :Denite buffer<CR>
nnoremap <leader>bb :Buffers<CR>
nnoremap <leader>fr :History<CR>
nnoremap <leader>ftt :Tags<CR>
nnoremap <leader>ftb :BTags<CR>
nnoremap <leader>fi :Rg 
nnoremap <leader>fc :Rg<CR>
nnoremap <leader>fl :Lines<CR>

" Version control bindings
nnoremap <leader>gg :Denite menu:git<CR>
nnoremap <leader>gw :Gwrite<CR>
nnoremap <leader>gc :Gcommit<CR>
nnoremap <leader>gl :Gpull<CR>
nnoremap <leader>gp :Gpush<CR>
nnoremap <leader>gd :Gdiff<CR>
nnoremap <leader>gs :Gstatus<CR>

" Compilation bindings
nnoremap <leader>bi :BuildIt<CR>
nnoremap <leader>bs :BuildItStatus<CR>

" REPL and Terminal bindings
tnoremap <leader><Esc> <C-\><C-n>
nnoremap <leader>r :IronRepl<CR>i

" Misc bindings
nnoremap <leader>. :call Toggle_writer_mode()<CR>

" Language specific bindings

" Intero bindings
" Process management:
nnoremap <leader>hio :InteroOpen<CR>
nnoremap <leader>hik :InteroKill<CR>
nnoremap <leader>hic :InteroHide<CR>
nnoremap <leader>hil :InteroLoadCurrentModule<CR>

" REPL commands
nnoremap <leader>hie :InteroEval<CR>
nnoremap <leader>hit :InteroGenericType<CR>
nnoremap <leader>hiT :InteroType<CR>
nnoremap <leader>hii :InteroInfo<CR>
nnoremap <leader>hiI :InteroTypeInsert<CR>

" Go to definition:
nnoremap <leader>hid :InteroGoToDef<CR>

" Highlight uses of identifier:
nnoremap <leader>hiu :InteroUses<CR>

" Reload the file in Intero after saving
autocmd! BufWritePost *.hs InteroReload

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

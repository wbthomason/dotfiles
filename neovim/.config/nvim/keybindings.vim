" Keybindings

" Set the leader keys
let mapleader = "\<space>"
let maplocalleader = ","

" More convenient ESC
inoremap <silent> jj <ESC>

" Setup descriptions
let g:lmaps = {}
let g:llmaps = {}
augroup local_filetype_guides
  au!
  au FileType tex let g:llmaps.l = { 'name' : 'vimtex' }
augroup END

" Re-run commands
nnoremap <silent> <localleader>r @:
let g:llmaps.r = ['@:', 'Rerun last colon command']

" Exiting
nnoremap <silent> <leader>q :qa<CR>
let g:lmaps.q = ['qa', 'Quit']
nnoremap <silent> <leader>x :x!<CR>
let g:lmaps.x = ['x!', 'Write and quit']

" Buffer bindings
let g:lmaps.b = {'name': 'Buffers'}
nnoremap <silent> <leader>w :w<CR>
let g:lmaps.w = ['w', 'Write buffer']
nnoremap <silent> <leader>d :Sayonara<CR>
let g:lmaps.d = ['Sayonara', 'Close buffer']
nnoremap <silent> <leader>a :bp<CR>
let g:lmaps.a = ['bp', 'Go to previous buffer']
nnoremap <silent> <leader>s :bn<CR> 
let g:lmaps.s = ['bn', 'Go to next buffer']
nnoremap <silent> <leader>bl :b#<CR>
let g:lmaps.b.l = ['b#', 'Go to last used buffer']
nnoremap <silent> <leader>bb :Denite buffer<CR>
let g:lmaps.b.b = ['Denite buffer', 'Select a buffer']

" Configuration bindings
let g:lmaps.c = {'name': 'Configuration'}
let g:lmaps.c.s = {'name': 'Sourcing'}
nnoremap <silent> <leader>cst :so %<CR>
let g:lmaps.c.s.t = ['so %', 'Source this file']
nnoremap <silent> <leader>csc :so ~/.config/nvim/init.vim<CR>
let g:lmaps.c.s.c = ['so ~/.config/nvim/init.vim', 'Source config file']
nnoremap <silent> <leader>cc :e ~/.config/nvim/init.vim<CR>
let g:lmaps.c.c = ['e ~/.config/nvim/init.vim', 'Open Neovim config']

" Error bindings
let g:lmaps.e = {'name': 'Errors'}
nnoremap <silent> <leader>eo :lopen<CR>
let g:lmaps.e.c = ['lopen', 'Open error list']
nnoremap <silent> <leader>ec :lclose<CR>
let g:lmaps.e.c = ['lclose', 'Close error list']

" Finder bindings
let g:lmaps.f = {'name': 'Find'}
nnoremap <silent> <leader>ff :Denite file/rec<CR>
let g:lmaps.f.f = ['Denite file/rec', 'Search for files in the current directory']
nnoremap <silent> <leader>fa :Denite file/rec:<C-r>=getcwd()<CR>
let g:lmaps.f.a = ['Denite file/rec:<C-r>=getcwd()', 'Search for files in an arbitrary directory']
nnoremap <silent> <leader>fg :DeniteProjectDir file/rec/git<CR>
let g:lmaps.f.g = ['DeniteProjectDir file/rec/git', 'Search for files in the current Git repo']
nnoremap <silent> <leader>fh :Denite help<CR>
let g:lmaps.f.h = ['Denite help', 'Search in help tags']
nnoremap <silent> <leader>fr :Denite file/old<CR>
let g:lmaps.f.r = ['Denite file/old', 'Search for recently edited files']
let g:lmaps.f.t = {'name': 'Tags'}
nnoremap <silent> <leader>ftt :Denite tag<CR>
let g:lmaps.f.t.t = ['Denite tag', 'Search in tags for the current directory']
nnoremap <silent> <leader>ftb :BTags<CR>
let g:lmaps.f.t.b = ['BTags', 'Search in tags for the current buffer']
nnoremap <silent> <leader>fi :Denite grep<CR>
let g:lmaps.f.i = ['Denite grep', 'Search for text in files in the current directory']
nnoremap <silent> <leader>fl :Denite line<CR>
let g:lmaps.f.l = ['Denite line', 'Search for text in currently open buffers']
let g:lmaps.f.s = {'name': 'Symbols'}
nnoremap <silent> <leader>fsb :Denite documentSymbol<CR>
let g:lmaps.f.s.b = ['Denite documentSymbol', 'Search symbols in the current buffer']
nnoremap <silent> <leader>fsp :Denite workspaceSymbol<CR>
let g:lmaps.f.s.p = ['Denite workspaceSymbol', 'Search symbols in the current project']
nnoremap <silent> <leader>fd :Vaffle<CR>
let g:lmaps.f.d = ['Vaffle', 'Open Vaffle']

" Toggles
let g:lmaps.t = {'name': 'Toggles'}
nnoremap <silent> <leader>th :nohls<CR>
let g:lmaps.t.h = ['nohls', 'Reset search highlights']
nnoremap <silent> <leader>ts :TagbarToggle<CR>
let g:lmaps.t.s = ['TagbarToggle', 'Toggle Tagbar split']
nnoremap <silent> <leader>tu :UndotreeToggle<CR>
let g:lmaps.t.u = ['UndotreeToggle', 'Toggle Undotree split']
nnoremap <silent> <leader>tw :call Toggle_writer_mode()<CR>
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
nnoremap <leader>gw :Gwrite<CR>
let g:lmaps.g.w = ['Gwrite', 'Write and stage the file']

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
nnoremap <silent> gD :call LanguageClient_textDocument_definition()<CR>
nnoremap <silent> <leader>ln :call LanguageClient_textDocument_rename()<CR>
let g:lmaps.l.n = ['call LanguageClient_textDocument_rename()', 'Rename symbol']
nnoremap <silent> <leader>lr :Denite references<CR>
let g:lmaps.l.r = ['Denite references', 'Find symbol references']
nnoremap <silent> <leader>lh :call LanguageClient_textDocument_hover()<CR>
let g:lmaps.l.h = ['call LanguageClient_textDocument_hover()', 'Hover on symbol']

" Formatting Bindings
nnoremap <silent> <leader>bf :Neoformat<CR>
let g:lmaps.b.f = ['Neoformat', 'Format buffer with Neoformat']

" Easy-Align bindings
" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

" Schlepp bindings
vmap <unique> <up>    <Plug>SchleppUp
vmap <unique> <down>  <Plug>SchleppDown
vmap <unique> <left>  <Plug>SchleppLeft
vmap <unique> <right> <Plug>SchleppRight

" Journal and note bindings
let g:lmaps.j = {'name': 'Journal'}
nnoremap <silent> <leader>jt :e ~/wiki/journal/<c-r>=strftime("%Y-%m-%d")<cr>.md<cr>
let g:lmaps.j.t = ['e ~/wiki/journal/<c-r>=strftime("%Y-%m-%d")<cr>.md', 'Edit journal entry for today']
nnoremap <silent> <leader>js :Denite file_rec:~/wiki/journal grep:~/wiki/journal:'':'^.+$'<cr>
let g:lmaps.j.s = ["Denite file_rec:~/wiki/journal grep:~/wiki/journal:'':'^.+$'", 'Search in journal entries']

let g:lmaps.n = {'name': 'Notes'}
nnoremap <silent> <leader>nn :Pad new<cr>
let g:lmaps.n.n = ['Pad new', 'Make a new note']
nnoremap <silent> <leader>nl :Pad ls<cr>
let g:lmaps.n.l = ['Pad ls', 'List notes']
nnoremap <silent> <leader>ns :Denite file_rec:~/wiki/notes grep:~/wiki/notes:'':'^.+$'<cr>
let g:lmaps.n.s = ["Denite file_rec:~/wiki/notes grep:~/wiki/notes:'':'^.+$'", 'Search notes']

let g:lmaps.p = {'name': 'Plans/Plan notes'}
nnoremap <silent> <leader>ps :Denite file_rec:~/wiki grep:~/wiki:'':'^.+$'<cr>
let g:lmaps.p.s = ["Denite file_rec:~/wiki/notes grep:~/wiki/notes:'':'^.+$'", 'Search plans/notes']
nnoremap <silent> <leader>pi :e ~/wiki/index.md<cr>
let g:lmaps.p.i = ['e ~/wiki/index.md', 'Go to plan index']

" Docs bindings
let g:lmaps.z = ['Zeavim', 'Search Zeal docs']

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

" Make Backspace and Enter more useful
nnoremap <BS> {
onoremap <BS> {
vnoremap <BS> {

nnoremap <expr> <CR> empty(&buftype) ? '}' : '<CR>'
onoremap <expr> <CR> empty(&buftype) ? '}' : '<CR>'
vnoremap <CR> }

" Deoplete bindings
"  <TAB>: completion.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ deoplete#manual_complete()
function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~ '\s'
endfunction

" <S-TAB>: completion back.
inoremap <expr><S-TAB>  pumvisible() ? "\<C-p>" : "\<C-h>"

" <C-h>, <BS>: close popup and delete backword char.
inoremap <expr><C-h> deoplete#smart_close_popup()."\<C-h>"
inoremap <expr><BS> deoplete#smart_close_popup()."\<C-h>"

" inoremap <expr><C-g> deoplete#undo_completion()
" <C-l>: redraw candidates
inoremap <expr><C-g>       deoplete#refresh()
inoremap <silent><expr><C-l> deoplete#complete_common_string()

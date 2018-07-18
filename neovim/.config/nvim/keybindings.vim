" Keybindings

" Set the leader keys
let g:mapleader = "\<space>"
let g:maplocalleader = ','

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
let g:lmaps.d = ['Sayonara', 'Close buffer and window']
nnoremap <silent> <leader>k :Sayonara!<CR>
let g:lmaps.k = ['Sayonara!', 'Close buffer, not window']
nnoremap <silent> <leader>a :bp<CR>
let g:lmaps.a = ['bp', 'Go to previous buffer']
nnoremap <silent> <leader>s :bn<CR> 
let g:lmaps.s = ['bn', 'Go to next buffer']
nnoremap <silent> <leader>bl :b#<CR>
let g:lmaps.b.l = ['b#', 'Go to last used buffer']
nnoremap <silent> <leader>bb :Buffers<CR>
nnoremap <silent> <tab> :Buffers<CR>
let g:lmaps.b.b = ['Buffers', 'Select a buffer']

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
nnoremap <silent> <leader>ff :Files<CR>
let g:lmaps.f.f = ['Files', 'Search for files in the current directory']
nnoremap <leader>fa :Files <C-r>=getcwd()<CR>
let g:lmaps.f.a = ['Files <C-r>=getcwd()<CR>', 'Search for files in an arbitrary directory']
nnoremap <silent> <leader>fg :GFiles<CR>
let g:lmaps.f.g = ['GFiles', 'Search for files in the current Git repo']
nnoremap <silent> <leader>fh :Helptags<CR>
let g:lmaps.f.h = ['Helptags', 'Search in help tags']
nnoremap <silent> <leader>fr :History<CR>
let g:lmaps.f.r = ['History', 'Search for recently edited files']
let g:lmaps.f.t = {'name': 'Tags'}
nnoremap <silent> <leader>ftt :Tags<CR>
let g:lmaps.f.t.t = ['Tags', 'Search in tags for the current directory']
nnoremap <silent> <leader>ftb :BTags<CR>
let g:lmaps.f.t.b = ['BTags', 'Search in tags for the current buffer']
nnoremap <silent> <leader>fi :Rg<CR>
let g:lmaps.f.i = ['Rg', 'Search for text in files in the current directory']
nnoremap <silent> <leader>fl :Lines<CR>
let g:lmaps.f.l = ['Lines', 'Search for text in currently open buffers']
nnoremap  <leader>fL :Locate
let g:lmaps.f.l = ['Locate', 'Search the system with locate']
let g:lmaps.f.s = {'name': 'Symbols'}
nnoremap <silent> <leader>fsb :call LanguageClient_textDocument_documentSymbol()<CR>
let g:lmaps.f.s.b = ['call LanguageClient_textDocument_documentSymbol()', 'Search symbols in the current buffer']
nnoremap <silent> <leader>fsp :call LanguageClient_workspace_symbol()<CR>
let g:lmaps.f.s.p = ['call LanguageClient_workspace_symbol()', 'Search symbols in the current project']
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
nnoremap <silent> <leader>gc :Gcommit<CR>
let g:lmaps.g.c = ['Gcommit', 'Create a git commit']
nnoremap <silent> <leader>gl :Gpull<CR>
let g:lmaps.g.l = ['Gpull', 'Run git pull']
nnoremap <silent> <leader>gp :Gpush<CR>
let g:lmaps.g.p = ['Gpush', 'Run git push']
nnoremap <silent> <leader>gd :Gdiff<CR>
let g:lmaps.g.d = ['Gdiff', 'Display git diff']
nnoremap <silent> <leader>gs :Gstatus<CR>
let g:lmaps.g.s = ['Gstatus', 'Display git status']
nnoremap <silent> <leader>gw :Gwrite<CR>
let g:lmaps.g.w = ['Gwrite', 'Write and stage the file']
nnoremap <silent> <leader>gi <Plug>(fzf-gitignore)<CR>
let g:lmaps.g.w = ['<Plug>(fzf-gitignore)', 'Write and stage the file']

" Compilation bindings
let g:lmaps.m = {'name': 'Build/Make'}
nnoremap <leader>mi :BuildIt<CR>
let g:lmaps.m.i = ['BuildIt', 'Run BuildIt for the current buffer']
nnoremap <leader>ms :BuildItStatus<CR>
let g:lmaps.m.s = ['BuildItStatus', 'Check the BuildIt status']
nnoremap <localleader>\, <silent> :Dispatch
let g:llmaps[','] = ['Dispatch', 'Run Dispatch']

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
nnoremap <silent> <leader>lr :call LanguageClient_textDocument_references()<CR>
let g:lmaps.l.r = ['call LanguageClient_textDocument_references()', 'Find symbol references']
nnoremap <silent> <leader>lh :call LanguageClient_textDocument_hover()<CR>
let g:lmaps.l.h = ['call LanguageClient_textDocument_hover()', 'Hover on symbol']
nnoremap <silent> <leader>la :call LanguageClient_textDocument_codeAction()<CR>
let g:lmaps.l.a = ['call LanguageClient_textDocument_codeAction()', 'Code actions at point']
nnoremap <silent> <leader>lf :call LanguageClient_textDocument_formatting()<CR>
let g:lmaps.l.f = ['call LanguageClient_textDocument_formatting()', 'Format document']

" Formatting Bindings
nnoremap <silent> <leader>bf :Neoformat<CR>
let g:lmaps.b.f = ['Neoformat', 'Format buffer with Neoformat']

" Easy-Align bindings
" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

" Schlepp bindings
vmap <up>    <Plug>SchleppUp
vmap <down>  <Plug>SchleppDown
vmap <left>  <Plug>SchleppLeft
vmap <right> <Plug>SchleppRight

" Journal and note bindings
let g:lmaps.j = {'name': 'Journal'}
nnoremap <silent> <leader>jt :e ~/wiki/journal/<c-r>=strftime("%Y-%m-%d")<cr>.md<cr>
let g:lmaps.j.t = ['e ~/wiki/journal/<c-r>=strftime("%Y-%m-%d")<cr>.md', 'Edit journal entry for today']
nnoremap <silent> <leader>js :Rg .* ~/wiki/journal<cr>
let g:lmaps.j.s = ['Rg .* ~/wiki/journal', 'Search in journal entries']

let g:lmaps.n = {'name': 'Notes'}
nnoremap <silent> <leader>nn :Pad new<cr>
let g:lmaps.n.n = ['Pad new', 'Make a new note']
nnoremap <silent> <leader>nl :Pad ls<cr>
let g:lmaps.n.l = ['Pad ls', 'List notes']
nnoremap <silent> <leader>ns :Files ~/wiki/notes<cr>
let g:lmaps.n.s = ['Files ~/wiki/notes', 'Search notes']

let g:lmaps.p = {'name': 'Plans/Plan notes'}
nnoremap <silent> <leader>ps :Files ~/wiki<cr>
let g:lmaps.p.s = ['Files ~/wiki', 'Search plans/notes']
nnoremap <silent> <leader>pi :e ~/wiki/index.md<cr>
let g:lmaps.p.i = ['e ~/wiki/index.md', 'Go to plan index']

" Leader guide bindings and settings
let g:allmaps = {}
let g:allmaps[' '] = g:lmaps
let g:allmaps[','] = g:llmaps
let g:allmaps[',']['name'] = '<localleader>'

nnoremap <silent> <leader> :<c-u>LeaderGuide '<Space>'<CR>
vnoremap <silent> <leader> :<c-u>LeaderGuideVisual '<Space>'<CR>
map <leader>. <Plug>leaderguide-global
nnoremap <localleader> :<c-u>LeaderGuide  ','<CR>
vnoremap <localleader> :<c-u>LeaderGuideVisual  ','<CR>
map <localleader>. <Plug>leaderguide-buffer

" 2-character Sneak (default)
nmap z <Plug>Sneak_s
nmap Z <Plug>Sneak_S

" visual-mode
xmap z <Plug>Sneak_s
xmap Z <Plug>Sneak_S

" operator-pending-mode
omap z <Plug>Sneak_s
omap Z <Plug>Sneak_S

" repeat motion
map ; <Plug>Sneak_;
map \ <Plug>Sneak_,

" 1-character enhanced 'f'
nmap f <Plug>Sneak_f
nmap F <Plug>Sneak_F

" visual-mode
xmap f <Plug>Sneak_f
xmap F <Plug>Sneak_F

" operator-pending-mode
omap f <Plug>Sneak_f
omap F <Plug>Sneak_F

" 1-character enhanced 't'
nmap t <Plug>Sneak_t
nmap T <Plug>Sneak_T

" visual-mode
xmap t <Plug>Sneak_t
xmap T <Plug>Sneak_T

" operator-pending-mode
omap t <Plug>Sneak_t
omap T <Plug>Sneak_T

" " Deoplete bindings
"
" inoremap <silent><expr> <TAB>
"       \ pumvisible() ? "\<C-n>" :
"       \ <SID>check_back_space() ? "\<TAB>" :
"       \ deoplete#manual_complete()
"
" function! s:check_back_space() abort
"   let l:col = col('.') - 1
"   return !l:col || getline('.')[l:col - 1]  =~? '\s'
" endfunction
"
" " <S-TAB>: completion back.
" inoremap <expr><S-TAB>  pumvisible() ? "\<C-p>" : "\<C-h>"
"
" " <C-h>, <BS>: close popup and delete backword char.
" inoremap <expr><C-h> deoplete#smart_close_popup()."\<C-h>"
" inoremap <expr><BS> deoplete#smart_close_popup()."\<C-h>"
"
" " inoremap <expr><C-g> deoplete#undo_completion()
" " <C-l>: redraw candidates
" inoremap <expr><C-g>       deoplete#refresh()
" inoremap <silent><expr><C-l> deoplete#complete_common_string()

inoremap <expr><Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr><S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
imap <c-u> <Plug>(ultisnips_expand)

inoremap <silent> <c-t> :Snippets<CR>

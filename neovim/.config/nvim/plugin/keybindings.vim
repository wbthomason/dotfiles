" Keybindings

" Get rid of the annoying F1 binding
map <F1> <nop>

" More convenient ESC
inoremap <silent> jj <ESC>

" Re-run commands
nnoremap <silent> <localleader>r @:
let g:llmaps.r = ['@:', 'Rerun last colon command']

" Exiting
nnoremap <silent> <leader>q :qa<cr>
let g:lmaps.q = ['qa', 'Quit']
nnoremap <silent> <leader>x :x!<cr>
let g:lmaps.x = ['x!', 'Write and quit']

" Buffer bindings
let g:lmaps.b = {'name': 'Buffers'}
nnoremap <silent> <leader>w :w<cr>
let g:lmaps.w = ['w', 'Write buffer']
nnoremap <silent><nowait> <leader>d :Sayonara<cr>
let g:lmaps.d = ['Sayonara', 'Close buffer and window']
nnoremap <silent> <leader>k :Sayonara!<cr>
let g:lmaps.k = ['Sayonara!', 'Close buffer, not window']
nnoremap <silent> <leader>bl :b#<cr>
let g:lmaps.b.l = ['b#', 'Go to last used buffer']
" nnoremap <silent>  - :Buffers<cr>
" nnoremap <silent> _ :FindIt<cr>

nnoremap <silent> - :Buffers<cr>
nnoremap <silent> _ :FindIt<cr>

" Error bindings
let g:lmaps.e = {'name': 'Errors'}
nnoremap <silent> <leader>eo :lopen<CR>
let g:lmaps.e.c = ['lopen', 'Open error list']
nnoremap <silent> <leader>ec :lclose<CR>
let g:lmaps.e.c = ['lclose', 'Close error list']

" Version control bindings
let g:lmaps.g = {'name': 'Git'}
nnoremap <silent> <leader>gl :Dispatch git pull<CR>
let g:lmaps.g.l = ['Dispatch git pull', 'Run git pull']
nnoremap <silent> <leader>gp :Dispatch git push<CR>
let g:lmaps.g.p = ['Dispatch git push', 'Run git push']
nnoremap <silent> <leader>gs :MagitOnly<CR>
let g:lmaps.g.s = ['MagitOnly', 'Display git status']

" Compilation bindings
nnoremap <localleader>\, <silent> :Dispatch
let g:llmaps[','] = ['Dispatch', 'Run Dispatch']

" REPL and Terminal bindings
tnoremap jj <C-\><C-n>
nnoremap <leader>r :IronRepl<CR>
let g:lmaps.r = ['IronRepl', 'Start the REPL for the current filetype']

" Formatting Bindings
nnoremap <silent> <leader>bf :Neoformat<CR>
let g:lmaps.b.f = ['Neoformat', 'Format buffer with Neoformat']

" Easy-Align bindings
" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap <silent> ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap <silent> ga <Plug>(EasyAlign)

" Schlepp bindings
vmap <up>    <Plug>SchleppUp
vmap <down>  <Plug>SchleppDown
vmap <left>  <Plug>SchleppLeft
vmap <right> <Plug>SchleppRight

" Journal and note bindings
let g:lmaps.j = {'name': 'Journal'}
nnoremap <silent> <leader>jt :e ~/wiki/journal/<c-r>=strftime("%Y%m%d")<cr>.org<cr>
let g:lmaps.j.t = ['e ~/wiki/journal/<c-r>=strftime("%Y%m%d")<cr>.org', 'Edit journal entry for today']
nnoremap <silent> <leader>js :Rg .* ~/wiki/journal<cr>
let g:lmaps.j.s = ['Rg .* ~/wiki/journal', 'Search in journal entries']

let g:lmaps.p = {'name': 'Plans/Plan notes'}
nnoremap <silent> <leader>ps :Files ~/wiki<cr>
let g:lmaps.p.s = ['Files ~/wiki', 'Search plans/notes']

" Leader guide bindings and settings
nnoremap <silent> <leader>      :<c-u>WhichKey '<Space>'<CR>
vnoremap <silent> <leader> :<c-u>WhichKeyVisual '<Space>'<CR>
nnoremap <silent> <localleader> :<c-u>WhichKey  ','<CR>
nnoremap <silent> <localleader> :<c-u>WhichKeyVisual  ','<CR>

" replace under cursor
nnoremap <leader>* :%s/\<<c-r><c-w>\>//g<left><left>

" Swap ` and ' for marks (` includes horizontal position)
for first in ['', 'g', '[', ']']
  for mode in ['n', 'x', 'o']
    exe mode . 'noremap ' . first . "' " . first . '`'
    exe mode . 'noremap ' . first . '` ' . first . "'"
  endfor
endfor

" Keybindings

" Get rid of the annoying F1 binding
map <F1> <nop>

" Re-run commands
nnoremap <silent> <localleader>r @:

" Easily run a build
nnoremap <silent> <localleader><localleader> :Make<cr>

" Exiting
nnoremap <silent> <leader>q :qa<cr>
nnoremap <silent> <leader>x :x!<cr>

" A little Emacs in my Neovim
nnoremap <silent><c-x><c-s> :w<cr>
inoremap <silent><c-x><c-s> <esc>:w<cr>a

" Buffer bindings
nnoremap <silent> <leader>w :w<cr>
nnoremap <silent><nowait> <leader>d :Sayonara<cr>
nnoremap <silent> <leader>k :Sayonara!<cr>
nnoremap <silent> <leader>l :b#<cr>
nnoremap <silent>  - :Clap buffers<cr>
nnoremap <silent> _ :Clap files --hidden -g "!.git/"<cr>

" Error bindings
nnoremap <silent> <leader>eo :lopen<CR>
nnoremap <silent> <leader>ec :lclose<CR>

" Version control bindings
nnoremap <silent> <leader>gl :Gpull<CR>
nnoremap <silent> <leader>gp :Gpush<CR>
nnoremap <silent> <leader>gs :Gstatus<CR>

" REPL and Terminal bindings
tnoremap jj <C-\><C-n>
nnoremap <leader>r :IronRepl<CR>

" Formatting Bindings
nnoremap <silent> <leader>f :Neoformat<CR>

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
nnoremap <silent> <leader>jt :e ~/wiki/journal/<c-r>=strftime("%Y%m%d")<cr>.org<cr>

" replace under cursor
nnoremap <leader>* :%s/\<<c-r><c-w>\>//g<left><left>

" Swap ` and ' for marks (` includes horizontal position)
for first in ['', 'g', '[', ']']
  for mode in ['n', 'x', 'o']
    exe mode . 'noremap ' . first . "' " . first . '`'
    exe mode . 'noremap ' . first . '` ' . first . "'"
  endfor
endfor

" Edit config
nnoremap <silent> <localleader>c :Clap files --hidden ~/dotfiles/neovim/.config/nvim<cr>

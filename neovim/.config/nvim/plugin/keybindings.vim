" Keybindings

" Get rid of the annoying F1 binding
map <f1> <nop>
nmap <f1> <nop>
imap <f1> <nop>

" Re-run commands
nnoremap <silent> <localleader>r @:

" Easily run a build
nnoremap <silent> <localleader><localleader> <cmd>Make<cr>

" Fuzzy-finding
nnoremap <c-p> <cmd>Clap<cr>

" Exiting
nnoremap <silent> <leader>q <cmd>qa<cr>
nnoremap <silent> <leader>x <cmd>x!<cr>
nnoremap <silent><nowait> <leader>d <cmd>Sayonara<cr>

" A little Emacs in my Neovim
nnoremap <silent><c-x><c-s> <cmd>w<cr>
inoremap <silent><c-x><c-s> <esc><cmd>w<cr>a

" Buffer bindings
nnoremap <silent> <leader>w <cmd>w<cr>
nnoremap <silent> <leader>k <cmd>Sayonara!<cr>
nnoremap <silent> <leader>l <cmd>b#<cr>
" nnoremap <silent>  - <cmd>Clap buffers<cr>
" nnoremap <silent> _ <cmd>Clap files<cr>
" nnoremap <silent> + <cmd>Clap gfiles<cr>
nnoremap <silent> - <cmd>Buffers<cr>
nnoremap <silent> _ <cmd>FzfPreviewFromResources buffer mru old git directory<cr>

" Hovering
nnoremap <silent> gh <cmd>lua require('hover').hover()<cr>

" Error bindings
nnoremap <silent> <leader>eo <cmd>lopen<CR>
nnoremap <silent> <leader>ec <cmd>lclose<CR>

" Version control bindings
nnoremap <silent> gl <cmd>Gpull<cr>
nnoremap <silent> gp <cmd>Gpush<cr>
nnoremap <silent> gs <cmd>Gstatus<cr>
nmap <silent> ]h <plug>(signify-next-hunk)
nmap <silent> [h <plug>(signify-prev-hunk)

" REPL and Terminal bindings
tnoremap jj <C-\><C-n>
nnoremap <leader>R <cmd>IronRepl<CR>

" Formatting Bindings
nnoremap <silent> <leader>f <cmd>Neoformat<CR>

" Easy-Align bindings
" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap <silent> ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap <silent> ga <Plug>(EasyAlign)

" replace under cursor
nnoremap <leader>* :%s/\<<c-r><c-w>\>//g<left><left>

" Swap ` and ' for marks (` includes horizontal position)
" for first in ['', 'g', '[', ']']
"   for mode in ['n', 'x', 'o']
"     exe mode . 'noremap ' . first . "' " . first . '`'
"     exe mode . 'noremap ' . first . '` ' . first . "'"
"   endfor
" endfor

" Folds
nnoremap <silent> <c-t> <cmd>normal! za<cr>

" Edit config
nnoremap <silent> <localleader>c <cmd>FzfPreviewDirectory ~/.config/nvim<cr>

" Yank to clipboard
nnoremap <silent> y+ <cmd>set opfunc=util#clipboard_yank<cr>g@
vnoremap <silent> y+ <cmd><C-U>call util#clipboard_yank(visualmode(), 1)<cr>

" Move between windows
nnoremap <c-h> <c-w>h
nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-l> <c-w>l

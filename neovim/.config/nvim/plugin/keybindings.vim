" Keybindings

" Get rid of the annoying F1 binding
imap <f1> <nop>

" Re-run commands
nnoremap <silent> <localleader>r @:

" Easily run a build
nnoremap <silent> <localleader><localleader> <cmd>Make<cr>

" Exiting
nnoremap <silent> <leader>q <cmd>qa<cr>
nnoremap <silent> <leader>x <cmd>x!<cr>
nnoremap <silent><nowait> <leader>d <cmd>Sayonara<cr>

" A little Emacs in my Neovim
nnoremap <silent><c-x><c-s> <cmd>w<cr>
inoremap <silent><c-x><c-s> <esc><cmd>w<cr>a

" Buffer bindings
nnoremap <silent> <leader>w <cmd>w<cr>
nnoremap <silent> - <cmd>lua require('telescope.builtin').buffers({show_all_buffers = true})<cr>
nnoremap <silent> _ <cmd>lua require('telescope.builtin').git_files()<cr>

" Hovering
nnoremap <silent> gh <cmd>lua require('hover').hover()<cr>

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
nnoremap <silent> <leader>f <Plug>(ale_fix)

" Easy-Align bindings
" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap <silent> ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap <silent> ga <Plug>(EasyAlign)

" replace under cursor
nnoremap <leader>* :%s/\<<c-r><c-w>\>//g<left><left>

" Folds
nnoremap <silent> <c-t> <cmd>normal! za<cr>

" Edit config
nnoremap <silent> <localleader>c <cmd>lua require('telescope.builtin').find_files({cwd='~/.config/nvim'})<cr>

" Yank to clipboard
nnoremap <silent> y+ <cmd>set opfunc=util#clipboard_yank<cr>g@
vnoremap <silent> y+ <cmd><C-U>call util#clipboard_yank(visualmode(), 1)<cr>

" Move between windows
nnoremap <c-h> <c-w>h
nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-l> <c-w>l

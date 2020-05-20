" Keybindings

" Get rid of the annoying F1 binding
map <f1> <nop>
nmap <f1> <nop>
imap <f1> <nop>

" Re-run commands
nnoremap <silent> <localleader>r @:

" Easily run a build
nnoremap <silent> <localleader><localleader> :Make<cr>

" Fuzzy-finding
nnoremap <c-p> :Clap<cr>

" Exiting
nnoremap <silent> <leader>q :qa<cr>
nnoremap <silent> <leader>x :x!<cr>
nnoremap <silent><nowait> <leader>d :Sayonara<cr>

" A little Emacs in my Neovim
nnoremap <silent><c-x><c-s> :w<cr>
inoremap <silent><c-x><c-s> <esc>:w<cr>a

" Buffer bindings
nnoremap <silent> <leader>w :w<cr>
nnoremap <silent> <leader>k :Sayonara!<cr>
nnoremap <silent> <leader>l :b#<cr>
nnoremap <silent>  - :Clap buffers<cr>
nnoremap <silent> _ :Clap files<cr>
nnoremap <silent> + :Clap gfiles<cr>

" Hovering
nnoremap <silent> gh :lua require('hover').hover()<cr>

" Error bindings
nnoremap <silent> <leader>eo :lopen<CR>
nnoremap <silent> <leader>ec :lclose<CR>

" Version control bindings
function! s:load_run_fugitive(cmd) abort
  if !exists('g:loaded_fugitive')
    packadd vim-fugitive
  endif

  execute a:cmd
endfunction
nnoremap <silent> <leader>gl <cmd>call <sid>load_run_fugitive('Gpull')<cr>
nnoremap <silent> <leader>gp <cmd>call <sid>load_run_fugitive('Gpush')<cr>
nnoremap <silent> <leader>gs <cmd>call <sid>load_run_fugitive('Gstatus')<cr>
nmap <silent> ]h <plug>(signify-next-hunk)
nmap <silent> [h <plug>(signify-prev-hunk)

" REPL and Terminal bindings
tnoremap jj <C-\><C-n>
nnoremap <leader>R :IronRepl<CR>

" Formatting Bindings
nnoremap <silent> <leader>f :Neoformat<CR>

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
nnoremap <silent> <c-t> :normal! za<cr>

" Edit config
nnoremap <silent> <localleader>c :Clap files --hidden ~/dotfiles/neovim/.config/nvim<cr>

" Yank to clipboard
nnoremap <silent> y+ :set opfunc=util#clipboard_yank<cr>g@
vnoremap <silent> y+ :<C-U>call util#clipboard_yank(visualmode(), 1)<cr>

" Move between windows
nnoremap <c-h> <c-w>h
nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-l> <c-w>l

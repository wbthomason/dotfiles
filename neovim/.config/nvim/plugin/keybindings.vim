" Keybindings

let s:in_vscode = exists('g:vscode')

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
inoremap <c-p> <esc>:Clap<cr>

" Comments
if s:in_vscode
  xunmap gc
  nunmap gc
  ounmap gc
  nunmap gcc 

  xmap gc  <Plug>VSCodeCommentary
  nmap gc  <Plug>VSCodeCommentary
  omap gc  <Plug>VSCodeCommentary
  nmap gcc <Plug>VSCodeCommentaryLine
endif

" Exiting
if s:in_vscode
  nnoremap <silent> <leader>q :Qall<cr>
  nnoremap <silent> <leader>x :Xall<cr>
  nnoremap <silent> <leader>d :Quit<cr>
else
  nnoremap <silent> <leader>q :qa<cr>
  nnoremap <silent> <leader>x :x!<cr>
  nnoremap <silent><nowait> <leader>d :Sayonara<cr>
endif

" A little Emacs in my Neovim
nnoremap <silent><c-x><c-s> :w<cr>
inoremap <silent><c-x><c-s> <esc>:w<cr>a

" Buffer bindings
if s:in_vscode
  nnoremap <silent> <leader>w :Write<cr>
  nnoremap <silent> - :call VSCodeNotify('workbench.action.quickOpenPreviousEditor')<cr>
  nnoremap <silent> _ :call VSCodeNotify('workbench.action.quickOpenPreviousEditor')<cr>
  nnoremap <silent> + :call VSCodeNotify('workbench.action.quickOpenPreviousEditor')<cr>
else
  nnoremap <silent> <leader>w :w<cr>
  nnoremap <silent> <leader>k :Sayonara!<cr>
  nnoremap <silent> <leader>l :b#<cr>
  nnoremap <silent>  - :Clap buffers<cr>
  nnoremap <silent> _ :Clap files<cr>
  nnoremap <silent> + :Clap gfiles<cr>
endif

" Hovering
if !s:in_vscode
  nnoremap <silent> gh :lua require('hover').hover()<cr>
endif

" Error bindings
if s:in_vscode
  nnoremap <silent> <leader>eo :call VSCodeNotify('workbench.actions.view.problems')<cr>
  nnoremap <silent> <leader>ec :call VSCodeNotify('workbench.action.closePanel')<cr>
else
  nnoremap <silent> <leader>eo :lopen<CR>
  nnoremap <silent> <leader>ec :lclose<CR>
endif

" Version control bindings
if s:in_vscode
  nnoremap <silent> <leader>gl :call VSCodeNotify('git.pull')<CR>
  nnoremap <silent> <leader>gp :call VSCodeNotify('git.push')<CR>
  nnoremap <silent> <leader>gs :call VSCodeNotify('gitlens.views.repositories:scm.focus')<CR>
else
  nnoremap <silent> <leader>gl :Gpull<CR>
  nnoremap <silent> <leader>gp :Gpush<CR>
  nnoremap <silent> <leader>gs :Gstatus<CR>
  nmap <silent> ]h <plug>(signify-next-hunk)
  nmap <silent> [h <plug>(signify-prev-hunk)
endif

" REPL and Terminal bindings
tnoremap jj <C-\><C-n>
if s:in_vscode
  nnoremap <leader>r :call VSCodeNotify('workbench.debug.action.toggleRepl')<cr>
else
  nnoremap <leader>r :IronRepl<CR>
endif

" Formatting Bindings
if s:in_vscode
  nnoremap <silent> <leader>f :call VSCodeNotify('editor.action.formatDocument')<cr>
else
  nnoremap <silent> <leader>f :Neoformat<CR>
endif

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

" Highlighted undo
" nmap u     <Plug>(highlightedundo-undo)
" nmap <C-r> <Plug>(highlightedundo-redo)
" nmap U     <Plug>(highlightedundo-Undo)
" nmap g-    <Plug>(highlightedundo-gminus)
" nmap g+    <Plug>(highlightedundo-gplus)

" Move between windows
nnoremap <c-h> <c-w>h
nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-l> <c-w>l

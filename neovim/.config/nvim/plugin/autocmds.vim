" Misc autocommands

augroup syntax_aucmds
  au!
  au Syntax * syn match extTodo "\<\(NOTE\|HACK\|BAD\|TODO\):\?" containedin=.*Comment.* | hi! link extTodo Todo
augroup END

augroup misc_aucmds
  au!
  au BufWinEnter * checktime
  au TextYankPost * silent! lua vim.highlight.on_yank()
augroup END

augroup collab_aucmds
  au!
  au VimEnter,BufEnter ~/projects/research/communion*/*.tex set formatoptions-=t | set wrap linebreak breakindent
augroup END

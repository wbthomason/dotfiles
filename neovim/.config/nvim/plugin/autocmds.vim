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

" General autocommands

augroup syntax_aucmds
  au!
  au Syntax * syn match extTodo "\<\(NOTE\|HACK\|BAD\|TODO\):\?" containedin=.*Comment.* | hi! link extTodo Todo
augroup END

augroup misc_aucmds
  au!
  au BufWinEnter * checktime
augroup END

if exists('g:vscode')
  augroup vscode_aucmds
    au!
    au VimEnter * set nospell
  augroup END
endif

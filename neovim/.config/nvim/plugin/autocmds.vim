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
else
  augroup collab_aucmds
    au!
    au VimEnter,BufEnter ~/projects/research/communion*/*.tex set formatoptions-=t | set wrap linebreak breakindent
  augroup END
endif

" General autocommands

augroup syntax_aucmds
  au!
  au Syntax * syn match extTodo "\<\(NOTE\|HACK\|BAD\|TODO\):\?" containedin=.*Comment.* | hi! link extTodo Todo
augroup END

augroup main_aucommands
  au!
  " au BufReadPost *
  "       \ if line("'\"") > 0 && line("'\"") <= line("$") |
  "       \ exe "normal! g`\"" |
  "       \ endif
  au BufWinEnter * checktime
  au VimEnter *
        \  if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
        \|   PlugInstall --sync | q
        \| endif
augroup END

function! LspMaybeHover(is_running) abort
  if a:is_running.result
    call LanguageClient#textDocument_hover()
  endif
endfunction

function! LspMaybeHighlight(is_running) abort
  if a:is_running.result
    call LanguageClient#textDocument_documentHighlight()
  endif
endfunction

augroup lsp_aucommands
  au!
  " au CursorHold * call LanguageClient#isAlive(function('LspMaybeHover'))
  " au CursorHold * call LanguageClient#isAlive(function('LspMaybeHighlight'))
  au CursorHold * silent call CocActionAsync('highlight')
  au User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
  au User CocDiagnosticChange call lightline#update_once()
augroup END

augroup extra_filetype_aucommands
  au!
  au BufRead,BufNewFile *.launch set filetype=roslaunch
  au BufNewFile,BufFilePre,BufRead *.scrbl set filetype=scribble
augroup END

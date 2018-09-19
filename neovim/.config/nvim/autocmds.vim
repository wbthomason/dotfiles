" General autocommands

augroup syntax_aucmds
  au!
  au Syntax * syn match extTodo "\<\(NOTE\|HACK\|BAD\|TODO\):\?" containedin=.*Comment.*
augroup END

augroup main_aucommands
  au!
  au BufReadPost *
        \ if line("'\"") > 0 && line("'\"") <= line("$") |
        \ exe "normal! g`\"" |
        \ endif
  au BufWinEnter * checktime
  au CompleteDone * pclose
  autocmd VimEnter *
        \  if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
        \|   PlugInstall --sync | q
        \| endif
augroup END

augroup lsp_aucommands
  au!
  au BufEnter * let b:LanguageClient_serverRunning = v:false 
  au User LanguageClientStopped let b:LanguageClient_serverRunning = v:false
  au User LanguageClientStarted let b:LanguageClient_serverRunning = v:true
  au CursorHold * if b:LanguageClient_serverRunning | call LanguageClient_textDocument_hover() | endif
  au CursorMoved * if b:LanguageClient_serverRunning | sil call LanguageClient#textDocument_documentHighlight() | endif
augroup END

augroup extra_filetype_aucommands
  au!
  au BufRead,BufNewFile *.launch set filetype=roslaunch
  au BufNewFile,BufFilePre,BufRead *.scrbl set filetype=scribble
augroup END

augroup ncm_aucommands
  au!
  au BufEnter * call ncm2#enable_for_buffer()
  au TextChangedI * call ncm2#auto_trigger()
  au InsertEnter * call ncm2#enable_for_buffer()
augroup END

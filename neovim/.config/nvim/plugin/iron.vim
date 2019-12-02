" Utilities for lazy-loading Iron

function! s:load_and_run(cmd) abort
  echom a:cmd
  delcommand IronRepl
  delcommand IronWatchCurrentFile
  delcommand IronSend
  packadd iron.nvim
  luafile $HOME/.config/nvim/iron.lua
  execute a:cmd
endfunction

command! IronRepl call <SID>load_and_run(':IronRepl')
command! -nargs=* IronWatchCurrentFile call <SID>load_and_run(':IronWatchCurrentFile ' . <q-args>)
command! -nargs=+ -bang IronSend call <SID>load_and_run(':IronSend' . (<bang>0 ? '!' : '') . ' ' . <q-args>)

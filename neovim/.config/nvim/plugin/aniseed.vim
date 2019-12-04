" Utilities for lazy-loading Aniseed

function! s:load_and_run(cmd) abort
  echom a:cmd
  delcommand AniseedEval
  delcommand AniseedEvalFile
  delcommand AniseedEvalRange
  packadd aniseed
  lua require("aniseed.mapping").init()
  execute a:cmd
endfunction

command! -nargs=+ AniseedEval call <SID>load_and_run(':AniseedEval ' . <q-args>)
command! -nargs=+ AniseedEvalFile call <SID>load_and_run(':AniseedEvalFile ' . <q-args>)
command! -range AniseedEvalRange call <SID>load_and_run(':' . <line1> . ',' . <line2> . 'AniseedEvalRange')

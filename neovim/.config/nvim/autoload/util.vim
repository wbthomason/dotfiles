scriptencoding utf-8
" Utility functions

function! util#in_vc_repo() abort
  let l:branch = gitbranch#name()
  return l:branch !=# ''
endfunction

function! util#vc_status() abort
  let l:mark = ''
  let l:branch = gitbranch#name()
  let l:changes = sy#repo#get_stats()
  let l:status = l:changes[0] > 0 ? '+' . l:changes[0] : ''
  let l:prefix = l:changes[0] > 0 ? ' ' : ''
  let l:status = l:changes[1] > 0 ? l:status . l:prefix . '~' . l:changes[1] : l:status
  let l:prefix = l:changes[1] > 0 ? ' ' : ''
  let l:status = l:changes[2] > 0 ? l:status . l:prefix . '-' . l:changes[2] : l:status
  return l:branch !=# '' ? l:status . ' ' . l:mark . ' ' . l:branch : ''
endfunction


function! util#syntax_stack() abort
  let l:s = synID(line('.'), col('.'), 1)                                       
  echo synIDattr(l:s, 'name') . ' -> ' . synIDattr(synIDtrans(l:s), 'name')
endfunc

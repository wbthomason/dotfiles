" Statusline functions
function! statusline#icon_filetype() abort
  return (&filetype !=# '' ? &filetype : 'no filetype') . ' ' . luaeval("require('utils').icons.lookup_filetype(_A)", &filetype) 
endfunction

let s:indicator_checking = "\uf110"
let s:indicator_warnings = "\uf071"
let s:indicator_errors = "\uf05e"
let s:indicator_ok = "\uf00c"

" The below adapted (slightly) from https://github.com/maximbaz/lightline-ale/blob/master/autoload/lightline/ale.vim

function! statusline#lint_warnings() abort
  if !statusline#linted()
    return ''
  endif
  let l:counts = ale#statusline#Count(bufnr(''))
  let l:all_errors = l:counts.error + l:counts.style_error
  let l:all_non_errors = l:counts.total - l:all_errors
  return l:all_non_errors == 0 ? '' : printf(s:indicator_warnings . '%d', all_non_errors)
endfunction

function! statusline#lint_errors() abort
  if !statusline#linted()
    return ''
  endif
  let l:counts = ale#statusline#Count(bufnr(''))
  let l:all_errors = l:counts.error + l:counts.style_error
  return l:all_errors == 0 ? '' : printf(s:indicator_errors . '%d', all_errors)
endfunction

function! statusline#lint_ok() abort
  if !statusline#linted()
    return ''
  endif
  let l:counts = ale#statusline#Count(bufnr(''))
  return l:counts.total == 0 ? s:indicator_ok : ''
endfunction

function! statusline#lint_checking() abort
  return ale#engine#IsCheckingBuffer(bufnr('')) ? s:indicator_checking : ''
endfunction


function! statusline#linted() abort
  return get(g:, 'ale_enabled', 0) == 1
        \ && getbufvar(bufnr(''), 'ale_linted', 0) > 0
        \ && ale#engine#IsCheckingBuffer(bufnr('')) == 0
endfunction

function! s:trim(str)
  if exists('*trim')
    return trim(a:str)
  endif
  return substitute(a:str, '\s\+$', '', '')
endfunction

function! statusline#force_update() abort
  set &ro = &ro
endfunction

" Statusline functions
function! statusline#icon_filetype() abort
  return (&filetype !=# '' ? &filetype : 'no filetype') . ' ' . luaeval("require('utils').icons.lookup_filetype(_A)", &filetype) 
endfunction

let s:indicator_checking = "\uf110"
let s:indicator_warnings = "\uf071"
let s:indicator_errors = "\uf05e"
let s:indicator_ok = "\uf00c"

" The below adapted (slightly) from https://github.com/maximbaz/lightline-ale/blob/master/autoload/lightline/ale.vim

let g:statusline_ale_warnings = v:false
function! statusline#lint_warnings() abort
  if !statusline#linted()
    return ''
  endif
  let l:counts = ale#statusline#Count(bufnr(''))
  let l:all_errors = l:counts.error + l:counts.style_error
  let l:all_non_errors = l:counts.total - l:all_errors
  let g:statusline_ale_warnings = l:all_non_errors != 0
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

function! statusline#vc_status() abort
  let l:mark = ''
  let l:branch = gitbranch#name()
  let l:changes = sy#repo#get_stats()
  let l:status = l:changes[0] > 0 ? '+' . l:changes[0] : ''
  let l:prefix = l:changes[0] > 0 ? ' ' : ''
  let l:status = l:changes[1] > 0 ? l:status . l:prefix . '~' . l:changes[1] : l:status
  let l:prefix = l:changes[1] > 0 ? ' ' : ''
  let l:status = l:changes[2] > 0 ? l:status . l:prefix . '-' . l:changes[2] : l:status
  let l:status = l:status == '' ? '' : l:status . ' '
  return l:branch !=# '' ? l:status . l:mark . ' ' . l:branch . ' ' : ''
endfunction

function! statusline#coc_status() abort
  let l:base_status = coc#status()
  return l:base_status != '' ? '  ' . l:base_status . ' ' : ''
endfunction

scriptencoding utf-8
"
" Statusline functions
function! statusline#icon() abort
  return luaeval("require('utils').icons.lookup_filetype(_A)", &filetype)
endfunction

function! statusline#filetype() abort
  return &filetype !=# '' ? &filetype : 'no filetype'
endfunction

let s:indicator_checking = "\uf110"
let s:indicator_warnings = "\uf071"
let s:indicator_errors = "\uf05e"
let s:indicator_ok = "\uf00c"

function! statusline#ale_warnings() abort
  let l:counts = ale#statusline#Count(bufnr(''))
  let l:all_errors = l:counts.error + l:counts.style_error
  let l:all_non_errors = l:counts.total - l:all_errors
  return l:all_non_errors == 0 ? '' : printf(s:indicator_warnings . '%d', all_non_errors)
endfunction

function! statusline#ale_errors() abort
  let l:counts = ale#statusline#Count(bufnr(''))
  let l:all_errors = l:counts.error + l:counts.style_error
  return l:all_errors == 0 ? '' : printf(s:indicator_errors . '%d', all_errors)
endfunction

function! statusline#ale_ok() abort
  let l:counts = ale#statusline#Count(bufnr(''))
  return l:counts.total == 0 ? s:indicator_ok : ''
endfunction

let s:spinner_frames = ['⣾', '⣽', '⣻', '⢿', '⡿', '⣟', '⣯', '⣷']

let s:ale_frame_idx = 0
function! statusline#ale_checking() abort
  let result = ''
  if ale#engine#IsCheckingBuffer(bufnr(''))
    let result = s:spinner_frames[s:ale_frame_idx % len(s:spinner_frames)]
    let s:ale_frame_idx = s:ale_frame_idx + 1
  else
    let s:ale_frame_idx = 0
  endif
  return result
endfunction

function! statusline#ale_enabled() abort
  return (get(g:, 'ale_enabled', 0) == 1 && getbufvar(bufnr(''), 'ale_linted', 0) > 0) || getbufvar(bufnr(''), 'ale_linted', 0) > 0
endfunction

function! statusline#ale() abort
  if !statusline#ale_enabled()
    return ''
  endif

  let l:icon = ' 🍺 '
  let l:checking = statusline#ale_checking()

  if l:checking !=# ''
    return l:icon . l:checking . ' '
  endif

  let l:ok = statusline#ale_ok()
  if l:ok !=# ''
    return l:icon . l:ok . ' '
  endif

  let l:warnings = statusline#ale_warnings()
  let l:errors = statusline#ale_errors()
  return l:icon . l:warnings . (l:warnings ==# '' ? '' : (l:errors ==# '' ? '' : ' ')) . l:errors . ' '
endfunction


function! statusline#gutentags_enabled() abort
  return exists('g:gutentags_enabled') && g:gutentags_enabled == 1 && gutentags#statusline() !=# ''
endfunction

function! statusline#gutentags()
  if !statusline#gutentags_enabled()
    return ''
  endif

  return gutentags#statusline('[', '] ')
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
  let l:status = l:status ==# '' ? '' : l:status . ' '
  return l:branch !=# '' ? l:status . l:mark . ' ' . l:branch . ' ' : ''
endfunction

function! statusline#coc() abort
  let l:base_status = coc#status()
  let l:status = ' 🇻 '
  if l:base_status !=# ''
    let l:status = l:status . l:base_status . ' '
  else
    let l:status = l:status . s:indicator_ok . ' '
  endif

  return l:status
endfunction

function! statusline#get_mode(mode) abort
  let l:currentmode={
        \'n' : 'Normal',
        \'no' : 'N·Operator Pending',
        \'v' : 'Visual',
        \'V' : 'V·Line',
        \'^V' : 'V·Block',
        \'s' : 'Select',
        \'S': 'S·Line',
        \'^S' : 'S·Block',
        \'i' : 'Insert',
        \'R' : 'Replace',
        \'Rv' : 'V·Replace',
        \'c' : 'Command',
        \'cv' : 'Vim Ex',
        \'ce' : 'Ex',
        \'r' : 'Prompt',
        \'rm' : 'More',
        \'r?' : 'Confirm',
        \'!' : 'Shell',
        \'t' : 'Terminal'
        \}
  return toupper(get(l:currentmode, a:mode, 'V-Block'))
endfunction

function! statusline#filename() abort
  let base_name = fnamemodify(bufname('%'), ':~:.')
  let space = min([60, float2nr(floor(0.6 * winwidth(0)))])
  if len(base_name) <= space
    return base_name
  endif

  return pathshorten(base_name)
endfunction

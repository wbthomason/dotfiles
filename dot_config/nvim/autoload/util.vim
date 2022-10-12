scriptencoding utf-8
" Utility functions

" Report the highlight groups active at the current point
function! util#syntax_stack() abort
  let l:s = synID(line('.'), col('.'), 1)
  echo synIDattr(l:s, 'name') . ' -> ' . synIDattr(synIDtrans(l:s), 'name')
endfunc

" Operator function to yank directly to the clipboard via the + register
function! util#clipboard_yank(type, ...) abort
  let sel_save = &selection
  let &selection = 'inclusive'
  if a:0 " Invoked from visual mode
    silent execute 'normal! "+y'
  else " Invoked with a motion
    silent execute 'normal! `[v`]"+y'
  endif

  let &selection = sel_save
endfunction

" Lazy-load a package on a command
function! util#load_and_run(cmd, start, end, bang, args, info) abort
  for old_cmd in a:info['delete']
    execute 'delcommand ' . old_cmd
  endfor

  if type(a:info['package']) == v:t_list
    for package in a:info['package']
      execute 'packadd ' . package
    endfor
  else
    execute 'packadd ' . a:info['package']
  endif

  if has_key(a:info, 'config')
    for config_cmd in a:info['config']
      execute config_cmd
    endfor
  endif

  execute printf('%s%s%s %s', (a:start == a:end ? '' : (a:start . ',' . a:end)), a:cmd, a:bang, a:args)
endfunction

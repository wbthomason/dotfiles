" Custom functions

function! Toggle_writer_mode()
  execute ':Goyo'
  execute ':Limelight!!'
endfunction

function! StartifyEntryFormat()
  return 'WebDevIconsGetFileTypeSymbol(absolute_path) ." ". entry_path'
endfunction

function! LinterStatus() abort
  let l:counts = ale#statusline#Count(bufnr(''))
  let l:errors = l:counts.error + l:counts.style_error
  let l:warnings = l:counts.warning + l:counts.style_warning
  return l:counts.total == 0 ? '⬥ ok' :
        \ printf('⚠ %d', l:warnings) . (l:errors == 0 ? '' :
        \ printf(' ⨉ %d', l:errors))
endfunction

function! ToggleColors()
  if &background == 'dark'
    set notermguicolors
    set background=light
    colorscheme monotonic-light
  else
    set termguicolors
    set background=dark
    colorscheme nazgul
  endif
endfunction

command ToggleColors call ToggleColors()

command WhatHighlight :call <SID>SynStack()<CR>
function! <SID>SynStack()
  if !exists("*synstack")
    return
  endif
  echo map(synstack(line('.'), col('.')), 'synIDattr(v:val,"name")')
endfunc

function! s:prepend_icon(candidates)
  let l:result = []
  for l:candidate in a:candidates
    let l:filename = fnamemodify(l:candidate, ':p:t')
    let l:icon = WebDevIconsGetFileTypeSymbol(l:filename, isdirectory(l:filename))
    call add(l:result, printf('%s %s', l:icon, l:candidate))
  endfor
  return l:result
endfunction

function! s:edit_file(item)
  let l:pos = stridx(a:item, ' ')
  let l:file_path = a:item[l:pos+1:-1]
  execute 'silent e' l:file_path
endfunction

command FindIt :call OmniFilter()
function! OmniFilter()
  let l:buffers = map(filter(range(1, bufnr('$')), 'buflisted(v:val)'), 'bufname(v:val)')
  let l:mru = filter(copy(v:oldfiles)[:20], "v:val !~# 'term:\\|magit:\\|fugitive:\\|NERD_tree\\|^/tmp/\\|.git/'")
  let s:file_list = extend(l:buffers, l:mru)
  let s:gfiles = ['']
  let s:files = ['']
  let s:done_vals = [v:false, v:false]
  let s:callbacks = {
    \ 'on_stdout': 'OnEvent',
    \ 'on_exit': 'OnExit'
    \ }

  let l:in_gitdir = finddir('.git', ';') !=# ''

  if l:in_gitdir
    let s:files_id = jobstart([ 'rg',  '--files' ], s:callbacks)
    let s:gfiles_id = jobstart([ 'git', 'ls-files'], s:callbacks)
  else
    let s:files_id = jobstart([ 'rg', '-L', '-i', '--no-ignore', '--files' ], s:callbacks)
    let s:gfiles_id = -5
    let s:done_vals[1] = v:true
  endif

  function! OnEvent(job_id, data, event)
    let l:list_ref = (a:job_id == s:files_id) ? s:files : s:gfiles
    let l:list_ref[-1] .= a:data[0]
    call extend(l:list_ref, a:data[1:])
  endfunction

  function! OnExit(job_id, data, event)
    if a:job_id == s:files_id
      let s:done_vals[0] = v:true
    else
      let s:done_vals[1] = v:true
    endif

    if s:done_vals[0] && s:done_vals[1]
      if s:gfiles_id == -5
        let s:gfiles = []
      endif

      call uniq(extend(extend(s:file_list, s:gfiles), s:files))
      call fzf#run(fzf#wrap({
        \ 'source': s:prepend_icon(s:file_list),
        \ 'sink':   function('s:edit_file')
        \ }))
      call feedkeys('G', 'n')
    endif
  endfunction
endfunction

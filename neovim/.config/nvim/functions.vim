" Custom functions

" function! Toggle_writer_mode()
"   execute ':Goyo'
"   execute ':Limelight!!'
" endfunction

function! StartifyEntryFormat()
  return "luaeval(\"require('utils').iconify(_A)\", entry_path)"
endfunction

" function! LinterStatus() abort
"   let l:counts = ale#statusline#Count(bufnr(''))
"   let l:errors = l:counts.error + l:counts.style_error
"   let l:warnings = l:counts.warning + l:counts.style_warning
"   return l:counts.total == 0 ? '⬥ ok' :
"         \ printf('⚠ %d', l:warnings) . (l:errors == 0 ? '' :
"         \ printf(' ⨉ %d', l:errors))
" endfunction

function! IconFileType() abort
  return (&filetype !=# '' ? &filetype : 'no filetype') . ' ' . luaeval("require('utils').icons.lookup_filetype(_A)", &filetype) 
endfunction

function! VCStatus() abort
  let l:mark = ''
  let l:branch = fugitive#head()
  let l:changes = sy#repo#get_stats()
  let l:status = l:changes[0] > 0 ? '+' . l:changes[0] : ''
  let l:prefix = l:changes[0] > 0 ? ' ' : ''
  let l:status = l:changes[1] > 0 ? l:status . l:prefix . '~' . l:changes[1] : l:status
  let l:prefix = l:changes[1] > 0 ? ' ' : ''
  let l:status = l:changes[2] > 0 ? l:status . l:prefix . '-' . l:changes[2] : l:status
  return l:branch !=# '' ? l:status . ' ' . l:mark . ' ' . l:branch : ''
endfunction

function! ToggleColors()
  if &background ==# 'dark'
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
  if !exists('*synstack')
    return
  endif
  echo map(synstack(line('.'), col('.')), 'synIDattr(v:val,"name")')
endfunc

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

  let l:gitdir = finddir('.git', ';')
  let l:in_gitdir = l:gitdir !=# ''

  if l:in_gitdir
    let s:files_id = jobstart([ 'rg',  '--files' ], s:callbacks)
    let s:gfiles_id = jobstart([
          \'git',
          \'ls-files',
          \fnamemodify(l:gitdir, ':h')],
          \ s:callbacks)
  else
    let s:files_id = jobstart([ 'rg', '-L', '-i', '--no-ignore', '--files' ], s:callbacks)
    let s:gfiles_id = -5
    let s:done_vals[1] = v:true
  endif

  function! s:edit_file(items)
    " let l:pos = stridx(a:item, ' ')
    " let l:file_path = a:item[l:pos+1:-1]
    " execute 'silent e' l:file_path
    let l:i = 1
    let l:ln = len(a:items)
    while l:i < l:ln
      let l:item = a:items[i]
      let l:parts = split(l:item, ' ')
      let l:file_path = get(l:parts, 1, '')
      let a:items[i] = l:file_path
      let l:i += 1
    endwhile

    call s:Sink(a:items)
  endfunction

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

      call extend(extend(s:file_list, s:gfiles), s:files)
      let s:file_list = luaeval("require('utils').filter_and_iconify(_A)", s:file_list)
      let l:opts = fzf#wrap({'options': ['-m', '--tiebreak=index']})
      let l:opts.source = s:file_list
      let s:Sink = l:opts['sink*']
      let l:opts['sink*'] = function('s:edit_file')
      " let l:opts.options = []
      call fzf#run(l:opts)
      call feedkeys('G', 'n')
    endif
  endfunction
endfunction

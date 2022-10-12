scriptencoding utf-8

function! fzf#floatingwin()
  let width = &columns / 2
  let height = &lines / 2
  let top = ((&lines - height) / 2) - 1
  let left = (&columns - width) / 2
  let opts = {'relative': 'editor', 'row': top, 'col': left, 'width': width, 'height': height, 'style': 'minimal'}

  let top = '╭' . repeat('─', width - 2) . '╮'
  let mid = '│' . repeat(' ', width - 2) . '│'
  let bot = '╰' . repeat('─', width - 2) . '╯'
  let lines = [top] + repeat([mid], height - 2) + [bot]
  let s:buf = nvim_create_buf(v:false, v:true)
  call nvim_buf_set_lines(s:buf, 0, -1, v:true, lines)
  call nvim_open_win(s:buf, v:true, opts)
  set winhl=Normal:Floating
  let opts.row += 1
  let opts.height -= 2
  let opts.col += 2
  let opts.width -= 4
  call nvim_open_win(nvim_create_buf(v:false, v:true), v:true, opts)
  augroup fzf_window_aucmds
    au!
    au BufWipeout <buffer> exe 'bw '.s:buf
  augroup END
endfunction

function! fzf#omnifilter()
  let l:buffers = map(filter(range(1, bufnr('$')), 'buflisted(v:val)'), 'bufname(v:val)')
  let l:mru = filter(copy(v:oldfiles)[:20], "v:val !~# 'term:\\|magit:\\|fugitive:\\|NERD_tree\\|^/tmp/\\|.git/'")
  let s:file_list = extend(l:buffers, l:mru)
  let s:gfiles = ['']
  let s:files = ['']
  let s:done_vals = [v:false, v:false]
  let s:callbacks = {
        \ 'on_stdout': 'Omni_On_Event',
        \ 'on_exit': 'Omni_On_Exit'
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
    let l:i = 1
    let l:ln = len(a:items)
    while l:i < l:ln
      let l:item = a:items[i]
      let l:parts = split(l:item, ' ')
      let l:file_path = get(l:parts, 1, '')
      let a:items[i] = l:file_path
      let l:i += 1
    endwhile

    call s:sink(a:items)
  endfunction

  function! Omni_On_Event(job_id, data, event)
    let l:list_ref = (a:job_id == s:files_id) ? s:files : s:gfiles
    let l:list_ref[-1] .= a:data[0]
    call extend(l:list_ref, a:data[1:])
  endfunction

  function! Omni_On_Exit(job_id, data, event)
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
      let l:opts = fzf#wrap({'options': ['-m', '--tiebreak=begin,length,index']})
      let l:opts.source = s:file_list
      let s:sink = l:opts['sink*']
      let l:opts['sink*'] = function('s:edit_file')
      " let l:opts.options = []
      call fzf#run(l:opts)
    endif
  endfunction
endfunction

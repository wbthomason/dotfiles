" Prosession
let g:prosession_tmux_title = 1
let g:prosession_on_startup = 0
let g:prosession_dir = expand('~/.cache/nvim/sessions')

function! s:complete_sessions(lead, cmd, curs)
  let fldr = fnamemodify(expand(a:lead), ':h')
  if !empty(a:lead) && fldr != '.' && isdirectory(fldr)
    let flist = glob(a:lead . '*', 0, 1)
  else
    let flead = empty(a:lead) ? '' : '*' . a:lead
    let flist = glob(fnamemodify(g:prosession_dir, ':p') . flead . '*.vim', 0, 1)
    let flist = map(flist, "fnamemodify(v:val, ':t:r')")
  endif
  let flist = map(flist, "substitute(v:val, '%', '/', 'g')")
  return flist  
endfunction

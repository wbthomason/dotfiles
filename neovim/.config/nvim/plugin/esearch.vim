let g:esearch = {
      \ 'adapter':          'rg',
      \ 'backend':          'nvim',
      \ 'out':              'win',
      \ 'batch_size':       1000,
      \ 'use':              ['visual', 'hlsearch', 'last'],
      \ 'default_mappings': 0,
      \}

let g:esearch#out#win#open = 'enew'
let g:esearch#cmdline#dir_icon = '🖿 '

function! s:load_esearch(cmd) abort
  delcommand Esearch
  packadd vim-esearch
  call esearch#out#win#map('s',       'split')
  call esearch#out#win#map('v',       'vsplit')
  call esearch#out#win#map('<Enter>', 'open')
  call esearch#out#win#map('e',       'open')

  "    Open silently (keep focus on the results window)
  call esearch#out#win#map('S', 'split-silent')
  call esearch#out#win#map('V', 'vsplit-silent')

  "    Move cursor with snapping
  call esearch#out#win#map('<C-n>', 'next')
  call esearch#out#win#map('<C-j>', 'next-file')
  call esearch#out#win#map('<C-p>', 'prev')
  call esearch#out#win#map('<C-k>', 'prev-file')

  call esearch#cmdline#map('<C-o><C-r>', 'toggle-regex')
  call esearch#cmdline#map('<C-o><C-s>', 'toggle-case')
  call esearch#cmdline#map('<C-o><C-w>', 'toggle-word')
  call esearch#cmdline#map('<C-o><C-h>', 'cmdline-help')
  command Esearch :call esearch#init()
  execute a:cmd
endfunction

command! Esearch :call s:load_esearch(':Esearch')

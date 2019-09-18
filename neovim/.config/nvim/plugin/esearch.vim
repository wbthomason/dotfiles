let g:esearch = {
      \ 'adapter':          'rg',
      \ 'backend':          'nvim',
      \ 'out':              'win',
      \ 'batch_size':       1000,
      \ 'use':              ['visual', 'hlsearch', 'last'],
      \ 'default_mappings': 0,
      \}

call esearch#out#win#map('t',       'tab')
call esearch#out#win#map('i',       'split')
call esearch#out#win#map('s',       'vsplit')
call esearch#out#win#map('<Enter>', 'open')
call esearch#out#win#map('o',       'open')

"    Open silently (keep focus on the results window)
call esearch#out#win#map('T', 'tab-silent')
call esearch#out#win#map('I', 'split-silent')
call esearch#out#win#map('S', 'vsplit-silent')

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

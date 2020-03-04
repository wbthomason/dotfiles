" Utilities for lazy-loading Iron
let s:iron_config = {
      \'delete': ['IronRepl', 'IronWatchCurrentFile', 'IronSend'],
      \'package': 'iron.nvim',
      \'config': ['luafile $HOME/.config/nvim/iron.lua']
      \}

command! IronRepl call util#load_and_run('IronRepl', 0, 0, '', '', s:iron_config)
command! -nargs=* IronWatchCurrentFile call util#load_and_run('IronWatchCurrentFile', 0, 0, '', <q-args>, s:iron_config)
command! -nargs=+ -bang IronSend call util#load_and_run('IronSend', 0, 0, <q-bang>, <q-args>, s:iron_config)

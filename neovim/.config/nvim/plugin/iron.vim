" Utilities for lazy-loading Iron
let s:iron_config = {
      \'delete': ['IronRepl', 'IronWatchCurrentFile', 'IronSend'],
      \'package': 'iron.nvim',
      \'config': ['luafile $HOME/.config/nvim/iron.lua']
      \}

command! IronRepl call util#load_and_run(':IronRepl', s:iron_config)
command! -nargs=* IronWatchCurrentFile call util#load_and_run(':IronWatchCurrentFile ' . <q-args>, s:iron_config)
command! -nargs=+ -bang IronSend call util#load_and_run(':IronSend' . (<bang>0 ? '!' : '') . ' ' . <q-args>, s:iron_config)

" Utilities for lazy-loading Dispatch
let s:dispatch_config = {
      \'delete': ['Dispatch', 'Make', 'Focus', 'Start'],
      \'package': 'vim-dispatch',
      \'config': []
      \}

command! -bang -nargs=* -range Dispatch call util#load_and_run('Dispatch', <line1>, <line2>, <q-bang>, <q-args>, s:dispatch_config)
command! -bang -nargs=* -range Make call util#load_and_run('Make', <line1>, <line2>, <q-bang>, <q-args>, s:dispatch_config)
command! -bang -nargs=* -range Focus call util#load_and_run('Focus', <line1>, <line2>, <q-bang>, <q-args>, s:dispatch_config)
command! -bang -nargs=* -range Start call util#load_and_run('Start', <line1>, <line2>, <q-bang>, <q-args>, s:dispatch_config)

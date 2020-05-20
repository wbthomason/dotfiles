let s:mundo_load_config = { 'delete': ['MundoToggle', 'MundoShow'], 'package': 'vim-mundo' }
command! -nargs=0 MundoToggle call util#load_and_run('MundoToggle', 0, 0, '', '', s:mundo_load_config)
command! -nargs=0 MundoShow call util#load_and_run('MundoShow', 0, 0, '', '', s:mundo_load_config)

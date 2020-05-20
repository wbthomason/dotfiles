let s:enmasse_load_config = { 'delete': ['EnMasse'], 'package': 'vim-enmasse' }
command! EnMasse call util#load_and_run('EnMasse', 0, 0, '', '', s:enmasse_load_config)

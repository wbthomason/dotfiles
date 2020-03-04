let g:grepper = get(g:, 'grepper', {})
let g:grepper.tools = ['rg', 'git', 'ag', 'ack', 'ack-grep', 'grep', 'findstr', 'pt', 'sift']
nnoremap <leader>s :Grepper<cr>

let s:grepper_config = {
      \'delete': ['Grepper'],
      \'package': 'vim-grepper',
      \'config': []
      \}

command! -bang -nargs=* -range Grepper call util#load_and_run('Grepper', <line1>, <line2>, <q-bang>, <q-args>, s:grepper_config)

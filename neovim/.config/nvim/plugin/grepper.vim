let g:grepper = get(g:, 'grepper', {})
let g:grepper.tools = ['rg', 'git', 'ag', 'ack', 'ack-grep', 'grep', 'findstr', 'pt', 'sift']
nnoremap <leader>s :Grepper<cr>

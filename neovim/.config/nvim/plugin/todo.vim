" Config for todo.txt files
augroup todo_txt_aucmds
  au!
  au filetype todo setlocal omnifunc=todo#Complete
  au filetype todo setlocal completeopt-=preview
augroup END

let g:Todo_txt_prefix_creation_date = 1

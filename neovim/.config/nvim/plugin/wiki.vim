" Config for wiki.vim

let g:wiki_root = '~/gdrive/wiki'
let g:wiki_filetypes = ['wiki', 'markdown']
let g:wiki_journal = {
      \ 'name': 'log',
      \ 'frequency': 'daily',
      \ 'date_format': {
      \   'daily' : '%Y-%m-%d',
      \   'weekly' : '%Y_w%V',
      \   'monthly' : '%Y_m%m',
      \ }
      \}
let g:wiki_mappings_use_defaults = 0
let g:wiki_list_todos = ['TODO', 'NEXT', 'IN-PROGRESS', 'DONE', 'PAUSED', 'CANCELLED']

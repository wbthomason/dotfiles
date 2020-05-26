let g:wiki_root = '~/gdrive/notes'
let g:wiki_zotero_root = '~/gdrive/papers'
let g:wiki_filetypes = ['md']
let g:wiki_journal = {
      \ 'name': 'log',
      \ 'frequency': 'daily',
      \ 'date_format': {
      \   'daily' : '%Y-%m-%d',
      \   'weekly' : '%Y_w%V',
      \   'monthly' : '%Y_m%m',
      \}
      \}
let g:wiki_list_todos = ['TODO', 'NEXT', 'IN-PROGRESS', 'DONE', 'PAUSED', 'CANCELLED']
let g:wiki_mappings_use_defaults = 'local'
let g:wiki_tags = { 'output': 'cursor' }

function! UpdateWikiTagsList() abort
  let old_a = @a
  redir @a
  silent call wiki#tags#list()
  redir END
  let wiki_tags = map(split(@a, '\W\+'), {_, val -> ':' . val . ':'})
  let @a = old_a
  let [lnum, col] = searchpos('## All Tags', 'cn')
  if lnum == 0 && col == 0
    call append(line('$'), ['', '## All Tags', ''])
  endif

  let [lnum, col] = searchpos('## All Tags', 'cn')
  foldopen!
  execute string(lnum + 1) . ',$d'
  let wiki_tags[0] = ''
  call append(lnum, wiki_tags)
  foldclose!
endfunction

command! UpdateWikiTags call UpdateWikiTagsList()

augroup wiki_load_aucmds
  au!
  au BufNewFile,BufReadPre ~/notes/**/*.md ++once packadd wiki.vim
augroup END

let s:wiki_load_config = { 
      \ 'delete': ['WikiJournal', 'WikiOpen'],
      \ 'package': 'wiki.vim',
      \}

command! WikiJournal call util#load_and_run('WikiJournal', 0, 0, '', '', s:wiki_load_config)
command! WikiOpen call util#load_and_run('WikiOpen', 0, 0, '', '', s:wiki_load_config)

" Open wiki files
nnoremap <silent> <localleader>w :Clap files ~/notes/wiki<cr>

function! Wiki_Open_Map(name) abort
  let l:name = wiki#get_root() . '/' . a:name
  if filereadable(l:name) || split(a:name, '/')[0] != 'meetings'
    return a:name
  endif

  return a:name . '_' . strftime('%Y%m%d')
endfunction

let g:wiki_map_create_page = 'Wiki_Open_Map'

function! Wiki_Link_Map(text) abort
  let l:lowercase = tolower(a:text)
  let l:no_spaces = substitute(l:lowercase, '\s\+', '_', 'g')
  let l:with_extension = printf('%s.%s', l:no_spaces, b:wiki.extension)
  let l:from_root = printf('%s/%s', wiki#get_root(), l:with_extension)
  let l:local_file = printf('%s/%s', expand('%:p:h'), l:with_extension)
  if !filereadable(l:local_file) && filereadable(l:from_root)
    return '/' . l:no_spaces
  endif

  return l:no_spaces
endfunction

let g:wiki_map_link_create = 'Wiki_Link_Map'

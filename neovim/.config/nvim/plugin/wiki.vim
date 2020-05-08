let g:wiki_root = '~/notes/wiki'
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


function! LoadWiki() abort
  packadd wiki.vim
  autocmd! wiki_load_aucmds
  augroup! wiki_load_aucmds
endfunction

augroup wiki_load_aucmds
  au!
  au BufNewFile,BufReadPre ~/notes/**/*.md call LoadWiki()
  au VimEnter * call LoadWiki()
augroup END

" Open wiki files
nnoremap <silent> <localleader>w :Clap files ~/notes/wiki<cr>
" Register tags provider
call clap#register('wiki_tags', g:clap#provider#wiki_tags#)


endfunction


endfunction


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

function! s:get_wiki_tags() abort
  let l:tags = wiki#tags#get_all()
  let l:results = []
  for [l:key, l:val] in items(l:tags)
    for [l:file, l:lnum, l:col] in l:val
      let l:results += [l:key . ': ' . l:file . ':' . l:lnum]
    endfor
  endfor

  return l:results
endfunction

function! s:accept_wiki_tag(line) abort
  " TODO: Support opening loclist with all files of same tag
  let [l:tag, l:file, l:lnum] = split(a:line, ':')
  execute 'edit ' . l:file
  execute l:lnum
endfunction

function! s:preview_wiki_file() abort
  let l:line = g:clap.display.getcurline()
  let [l:tag, l:file, l:lnum] = split(l:line, ':')
  echom "called for " . l:file
  call clap#preview#file_at(l:file, l:lnum)
endfunction

" Wiki tags with Clap
let g:clap_provider_wiki_tags = {
      \ 'source': funcref('s:get_wiki_tags'),
      \ 'sink': funcref('s:accept_wiki_tag'),
      \ 'source_type': 4,
      \ 'on_move': funcref('s:preview_wiki_file'),
      \ 'syntax': 'wiki'
      \}

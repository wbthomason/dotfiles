" Author: wbthomason <wil.thomason@gmail.com>
" Description: List and preview files with wiki.vim tags

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
  let [l:tag, l:file, l:lnum] = split(a:line, ':')
  if has_key(g:clap, 'open_action')
    execute g:clap.open_action l:file
  else
    execute 'edit' l:file
  endif
  silent call cursor(str2nr(l:lnum), 1)
  normal! ^zvzz
endfunction

function! s:preview_wiki_file() abort
  let l:line = g:clap.display.getcurline()
  let [l:tag, l:file, l:lnum] = split(l:line, ':')
  let l:file = trim(l:file)
  let l:lnum = str2nr(l:lnum)
  call clap#preview#file_at(l:file, l:lnum)
endfunction

function! s:loclist_all_tag_files() abort
  let l:line = g:clap.display.getcurline()
  call clap#_exit()
  let [l:tag, l:file, l:lnum] = split(l:line, ':')
  let l:tag = trim(l:tag)
  call wiki#tags#search('-output', 'loclist', l:tag)
endfunction

let g:clap#provider#wiki_tags# = {
      \ 'source': funcref('s:get_wiki_tags'),
      \ 'sink': funcref('s:accept_wiki_tag'),
      \ 'source_type': 4,
      \ 'on_move': funcref('s:preview_wiki_file'),
      \ 'syntax': 'markdown',
      \ 'support_open_action': v:true,
      \ 'action': {
        \ 'Open &vertically': { -> clap#selection#try_open('ctrl-v') },
        \ 'Open in &split': { -> clap#selection#try_open('ctrl-x') },
        \ 'Open &all with tag': function('s:loclist_all_tag_files')
        \}
      \}

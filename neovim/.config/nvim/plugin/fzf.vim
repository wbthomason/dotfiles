" Settings for fzf (and related plugins)
" Lame M-x approximation
nnoremap <silent> <leader><leader> :Command<cr>

let g:fzf_layout = { 'window': 'call fzf#floatingwin()' }
let g:fzf_gitignore_no_maps = 1
let g:fzf_colors =
      \ {'fg':     ['fg', 'NormalFloat'],
      \ 'bg':      ['bg', 'NormalFloat'],
      \ 'hl':      ['fg', 'Comment'],
      \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
      \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
      \ 'hl+':     ['fg', 'Statement'],
      \ 'info':    ['fg', 'PreProc'],
      \ 'border':  ['fg', 'Ignore'],
      \ 'prompt':  ['fg', 'Conditional'],
      \ 'pointer': ['fg', 'Exception'],
      \ 'marker':  ['fg', 'Keyword'],
      \ 'spinner': ['fg', 'Label'],
      \ 'header':  ['fg', 'Comment'] }

augroup fzf_aucmds
  au!
  au FileType fzf set nonumber norelativenumber
augroup END

command FindIt :call fzf#omnifilter()

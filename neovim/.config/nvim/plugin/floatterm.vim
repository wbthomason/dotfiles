augroup floaterm_aucmds
  au!
  autocmd User Startified setlocal buflisted
augroup END

let g:floaterm_position = 'center'
let g:floaterm_background = '#252525'
let g:floaterm_border_color = '#555555'
let g:floaterm_border_bgcolor = '#141414'
nnoremap <leader>t :FloatermToggle<cr>

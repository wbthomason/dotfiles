" let g:qs_highlight_on_keys = ['f', 'F', 't', 'T']
let g:qs_lazy_highlight = 1
highlight QuickScopePrimary guifg='#afdf5f' gui=underline ctermfg=155 cterm=underline
highlight QuickScopeSecondary guifg='#5fdfdf' gui=underline ctermfg=81 cterm=underline

augroup qs_load_aucommands
  au!
  " au InsertEnter * ++once call s:load_qs()
augroup END

function! s:load_qs() abort
  packadd quick-scope
  autocmd! qs_load_aucommands
  augroup! qs_load_aucommands
endfunction

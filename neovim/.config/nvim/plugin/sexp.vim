" Load vim-sexp and vim-sexp-mappings-for-regular-people
let g:sexp_enable_insert_mode_mappings = 0
augroup sexp_loaders
  au!
  execute 'autocmd FileType ' . g:sexp_filetypes . ' call s:load_sexps()'
augroup END

function! s:load_sexps() abort
  autocmd! sexp_loaders
  augroup! sexp_loaders
  packadd vim-sexp
  packadd vim-sexp-mappings-for-regular-people
  doautoall sexp_filetypes Filetype
  doautoall sexp_mappings_for_regular_people Filetype
endfunction

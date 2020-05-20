" Matchup
let g:matchup_matchparen_deferred = 1
let g:matchup_matchparen_deferred_show_delay = 100
let g:matchup_matchparen_hi_surround_always = 1
let g:matchup_override_vimtex = 1
let g:matchup_delim_start_plaintext = 0
let g:matchup_transmute_enabled = 0

augroup matchup_load_aucommands
  au!
  au VimEnter * ++once call s:load_matchup()
augroup END

function! s:load_matchup() abort
  packadd vim-matchup
  doautocmd matchup_filetype FileType *
endfunction

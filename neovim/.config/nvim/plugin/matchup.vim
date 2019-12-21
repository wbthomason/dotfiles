" Matchup
let g:matchup_matchparen_deferred = 1
let g:matchup_matchparen_hi_surround_always = 1

if !exists('g:vscode')
  augroup matchup_load_aucommands
    au!
    au VimEnter * call s:load_matchup()
  augroup END
endif

function! s:load_matchup() abort
  packadd vim-matchup
  autocmd! matchup_load_aucommands
  augroup! matchup_load_aucommands
endfunction

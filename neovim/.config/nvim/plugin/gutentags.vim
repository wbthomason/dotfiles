let g:gutentags_project_root = ['.wiki']
let g:gutentags_define_advanced_commands = 1

function! s:load_gutentags()
  packadd vim-gutentags
  autocmd! gutentags_load_aucommands
  augroup! gutentags_load_aucommands
  verbose call gutentags#setup_gutentags()
endfunction

augroup gutentags_load_aucommands
  au!
  au VimEnter,BufEnter ~/wiki/*.md call s:load_gutentags()
augroup END

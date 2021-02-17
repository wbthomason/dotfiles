scriptencoding utf-8
set noshowmode
set laststatus=2

" Setup the colors
function! s:setup_colors() abort
  hi StatuslineSeparator guifg=#3a3a3a gui=none guibg=none
  hi StatuslinePercentage guibg=#3a3a3a gui=none guifg=#dab997
  hi StatuslineNormal guibg=#3a3a3a gui=none guifg=#e9e9e9
  hi StatuslineVC guibg=#3a3a3a gui=none guifg=#a9a9a9
  hi StatuslineLintWarn guibg=#3a3a3a gui=none guifg=#ffcf00
  hi StatuslineLintChecking guibg=#3a3a3a gui=none guifg=#458588
  hi StatuslineLintError guibg=#3a3a3a gui=none guifg=#d75f5f
  hi StatuslineLintOk guibg=#3a3a3a gui=none guifg=#b8bb26
  hi StatuslineLint guibg=#e9e9e9 guifg=#3a3a3a
  hi StatuslineLineCol guibg=#3a3a3a gui=none guifg=#878787
  hi StatuslineTS guibg=#3a3a3a gui=none guifg=#878787
  hi StatuslineFiletype guibg=#3a3a3a gui=none guifg=#e9e9e9
endfunction

augroup statusline_colors
  au!
  au ColorScheme * call s:setup_colors()
augroup END

call s:setup_colors()

au VimEnter * ++once lua statusline = require('statusline')
au VimEnter * ++once lua vim.o.statusline = '%!v:lua.statusline.status()'

scriptencoding utf-8
set noshowmode
set laststatus=3

" Setup the colors
function! s:setup_colors() abort
  if g:colors_name ==# 'nazgul'
    let s:bg_color = '#333333'
    let s:fg_color = '#e9e9e9'
  else
    let s:bg_color = synIDattr(synIDtrans(hlID('Statusline')), 'bg#', 'gui') 
    let s:fg_color = synIDattr(synIDtrans(hlID('Statusline')), 'fg#', 'gui') 
  endif

  exec 'hi Statusline guifg=' . s:fg_color . ' guibg=' . s:bg_color . ' gui=none'
  exec 'hi StatuslineSeparator guifg=' . s:bg_color . ' gui=none guibg=none'
  exec 'hi StatuslinePercentage guibg=' . s:bg_color . ' gui=none guifg=#dab997'
  exec 'hi StatuslineNormal guibg=' . s:bg_color . ' gui=none guifg=' . s:fg_color
  exec 'hi StatuslineVC guibg=' . s:bg_color . ' gui=none guifg=#a9a9a9'
  exec 'hi StatuslineLintWarn guibg=' . s:bg_color . ' gui=none guifg=#ffcf00'
  exec 'hi StatuslineLintChecking guibg=' . s:bg_color . ' gui=none guifg=#458588'
  exec 'hi StatuslineLintError guibg=' . s:bg_color . ' gui=none guifg=#d75f5f'
  exec 'hi StatuslineLintOk guibg=' . s:bg_color . ' gui=none guifg=#b8bb26'
  exec 'hi StatuslineLint guibg=' . s:fg_color . ' guifg=' . s:bg_color
  exec 'hi StatuslineLineCol guibg=' . s:bg_color . ' gui=none guifg=#878787'
  exec 'hi StatuslineTS guibg=' . s:bg_color . ' gui=none guifg=#878787'
  exec 'hi StatuslineFiletype guibg=' . s:bg_color . ' gui=none guifg=' . s:fg_color
  exec 'hi StatuslineNormalAccent guibg=#d75f5f gui=bold guifg=' . s:fg_color
  exec 'hi StatuslineInsertAccent guifg=' . s:fg_color . ' gui=bold guibg=#dab997'
  exec 'hi StatuslineReplaceAccent guifg=' . s:fg_color . ' gui=bold guibg=#afaf00'
  exec 'hi StatuslineConfirmAccent guifg=' . s:fg_color . ' gui=bold guibg=#83adad'
  exec 'hi StatuslineTerminalAccent guifg=' . s:fg_color . ' gui=bold guibg=#6f6f6f'
  exec 'hi StatuslineMiscAccent guifg=' . s:fg_color . ' gui=bold guibg=#f485dd'
  exec 'hi StatuslineFilenameModified guifg=#d75f5f gui=bold guibg=' . s:bg_color
  exec 'hi StatuslineFilenameNoMod guifg=' . s:fg_color . ' gui=bold guibg=' . s:bg_color

endfunction

augroup statusline_colors
  au!
  au ColorScheme * call s:setup_colors()
augroup END

call s:setup_colors()

au VimEnter * ++once lua statusline = require('statusline')
au VimEnter * ++once lua vim.o.statusline = '%!v:lua.statusline.status()'

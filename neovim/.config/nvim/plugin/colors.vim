" Custom color tweaks

function! s:setup_colors() abort
  hi! RedHover guifg=#cc241d ctermfg=124 gui=NONE cterm=NONE
  hi! YellowHover guifg=#fabd2f ctermfg=214 gui=NONE cterm=NONE
  hi! OrangeHover guifg=#fd7d2f ctermfg=214 gui=NONE cterm=NONE
  hi! GreenHover guifg=#b8cc26 ctermfg=142 gui=NONE cterm=NONE
  hi! BlueHover guifg=#83a5cb ctermfg=109 gui=NONE cterm=NONE
  hi! AquaHover guifg=#8ec07c ctermfg=108 gui=NONE cterm=NONE
  hi! WhiteHover guifg=#ffffff ctermfg=108 gui=NONE cterm=NONE

  let sign_col_bg = synIDattr(synIDtrans(hlID('SignColumn')), 'bg#', 'gui') 
  let sign_col_bg = empty(sign_col_bg) ? synIDattr(synIDtrans(hlID('SignColumn')), 'bg#', 'cterm') : sign_col_bg
  exec 'hi RedSign guifg=#cc241d ctermfg=124 gui=NONE cterm=NONE guibg=' . sign_col_bg
  exec 'hi YellowSign guifg=#fabd2f ctermfg=214 gui=NONE cterm=NONE guibg=' . sign_col_bg
  exec 'hi GreenSign guifg=#b8cc26 ctermfg=142 gui=NONE cterm=NONE guibg=' . sign_col_bg
  exec 'hi BlueSign guifg=#83a5cb ctermfg=109 gui=NONE cterm=NONE guibg=' . sign_col_bg
  exec 'hi PurpleSign guifg=#a3a5eb ctermfg=109 gui=NONE cterm=NONE guibg=' . sign_col_bg
  exec 'hi AquaSign guifg=#8ec07c ctermfg=108 gui=NONE cterm=NONE guibg=' . sign_col_bg
  exec 'hi WhiteSign guifg=#ffffff gui=NONE cterm=NONE guibg=' . sign_col_bg

  hi! HintHighlight guifg=#83a5cb gui=undercurl guisp=#83a5cb

  hi! link DiagnosticError RedHover
  hi! link DiagnosticWarning YellowHover
  hi! link DiagnosticInformation WhiteHover
  hi! link DiagnosticHint HintSign
  hi! link LspReferenceText AquaHover
  hi! link LspReferenceRead BlueHover
  hi! link LspReferenceWrite GreenHover
  hi! link TSDefinition LspReferenceText
  hi! link TSDefinitionUsage LspReferenceWrite

  hi! DiagnosticUnderlineError guifg=#ff727b ctermfg=NONE guibg=NONE ctermbg=NONE guisp=#9d0006
  hi! DiagnosticUnderlineWarn guifg=#fabd2f ctermfg=NONE guibg=NONE ctermbg=NONE guisp=#b57614
  hi! DiagnosticUnderlineInfo guifg=#83a598 ctermfg=NONE guibg=NONE ctermbg=NONE 
  hi! DiagnosticUnderlineHint guifg=#83a5cb ctermfg=NONE guibg=NONE ctermbg=NONE guisp=#83a5cb

  hi! link SignifySignAdd GreenSign
  hi! link SignifySignChange BlueSign
  hi! link SignifySignDelete RedSign

  hi! link DirvishGitModified AquaSign
  hi! link DirvishGitStaged GreenSign
  hi! link DirvishGitRenamed AquaSign
  hi! link DirvishGitUnmerged RedSign
  hi! link DirvishGitUntracked YellowSign
  hi! link DirvishGitUntrackedDir OrangeHover

  hi! HoverDisplay guibg=#303030 guifg=#dddddd

  hi LspCxxHlGroupEnumConstant guifg=#818181
  hi LspCxxHlGroupNamespace guifg=#f0f0f0
  hi LspCxxHlGroupMemberVariable guifg=#ebebeb

  hi! link LspFloatWinBorder IndentBlanklineChar
endfunction

augroup colors_customization
  au!
  au ColorScheme * call s:setup_colors()
augroup END

call s:setup_colors()

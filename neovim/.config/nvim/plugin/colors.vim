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
  exec 'hi AquaSign guifg=#8ec07c ctermfg=108 gui=NONE cterm=NONE guibg=' . sign_col_bg
  exec 'hi WhiteSign guifg=#ffffff gui=NONE cterm=NONE guibg=' . sign_col_bg

  hi! link ALEErrorSign RedSign
  hi! link ALEWarningSign YellowSign
  hi! link ALEInfoSign WhiteSign

  hi! link ALEVirtualTextError RedHover
  hi! link ALEVirtualTextWarning YellowHover
  hi! link ALEVirtualTextInfo WhiteHover
  hi! link ALEVirtualTextStyleError OrangeHover
  hi! link ALEVirtualTextStyleWarning BlueHover

  hi! link CocHighlightText AquaHover
  hi! link CocHighlightRead BlueHover
  hi! link CocHighlightWrite GreenHover
  hi! link CocErrorSign ALEErrorSign
  hi! link CocWarningSign ALEWarningSign
  hi! link CocInfoSign ALEInfoSign
  hi! CocHintHighlight guifg=#83a5cb gui=undercurl guisp=#83a5cb
  hi! link CocHintSign BlueSign

  let matchGroupId = hlID(hlexists('ClapInput') != 0 ? 'ClapInput' : 'Visual')
  let input_bg = synIDattr(synIDtrans(matchGroupId), 'bg#', 'gui')
  let input_bg = input_bg !=# '' ? input_bg : synIDattr(synIDtrans(hlID('Visual')), 'bg#', 'gui')

  let matchGroupId = hlID(hlexists('ClapDisplay') != 0 ? 'ClapDisplay' : 'Pmenu')
  let display_bg = synIDattr(synIDtrans(matchGroupId), 'bg#', 'gui')
  let display_bg = display_bg !=# '' ? display_bg : synIDattr(synIDtrans(hlID('Pmenu')), 'bg#', 'gui')

  hi! ClapCurrentSelection guifg=#ffffff gui=bold
  exec 'hi! ClapInput guifg=#ffffff guibg=' . input_bg
  exec 'hi! ClapNoMatchesFound guifg=#fe4559 guibg=' . display_bg
  hi! ClapQuery guifg=#ffffff
  hi! link ClapSpinner ClapInput

  hi! ALEError guifg=#ff727b ctermfg=NONE guibg=NONE ctermbg=NONE gui=undercurl cterm=undercurl guisp=#9d0006
  hi! ALEWarning guifg=#fabd2f ctermfg=NONE guibg=NONE ctermbg=NONE gui=undercurl cterm=undercurl guisp=#b57614
  hi! ALEInfo guifg=#83a598 ctermfg=NONE guibg=NONE ctermbg=NONE gui=undercurl cterm=undercurl

  hi! link CocErrorHighlight ALEError
  hi! link CocWarningHighlight ALEWarning
  hi! link CocErrorFloat ALEError
  hi! link CocWarningFloat ALEWarning

  hi! link SignifySignAdd GreenSign
  hi! link SignifySignChange AquaSign
  hi! link SignifySignDelete RedSign

  hi! HoverDisplay guibg=#303030 guifg=#dddddd

  let s:idx = 1
  while s:idx < 9
    execute 'hi! link ClapMatches' . s:idx . ' ClapMatches'
    execute 'hi! link ClapFuzzyMatches' . s:idx . ' ClapMatches'
    let s:idx += 1
  endwhile

  while s:idx < 13
    execute 'hi! link ClapFuzzyMatches' . s:idx . ' ClapMatches'
    let s:idx += 1
  endwhile
  unlet s:idx

  hi LspCxxHlGroupEnumConstant guifg=#818181
  hi LspCxxHlGroupNamespace guifg=#f0f0f0
  hi LspCxxHlGroupMemberVariable guifg=#ebebeb
endfunction

augroup colors_customization
  au!
  au ColorScheme * call s:setup_colors()
augroup END

call s:setup_colors()

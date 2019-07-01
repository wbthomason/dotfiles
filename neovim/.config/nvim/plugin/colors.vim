" Custom color tweaks

hi RedSign guifg=#cc241d ctermfg=124 gui=NONE cterm=NONE
hi YellowSign guifg=#fabd2f ctermfg=214 gui=NONE cterm=NONE
hi GreenSign guifg=#b8cc26 ctermfg=142 gui=NONE cterm=NONE
hi BlueSign guifg=#83a5cb ctermfg=109 gui=NONE cterm=NONE
hi AquaSign guifg=#8ec07c ctermfg=108 gui=NONE cterm=NONE

hi RedHover guifg=#cc241d ctermfg=124 gui=NONE cterm=NONE
hi YellowHover guifg=#fabd2f ctermfg=214 gui=NONE cterm=NONE
hi OrangeHover guifg=#fd7d2f ctermfg=214 gui=NONE cterm=NONE
hi GreenHover guifg=#b8cc26 ctermfg=142 gui=NONE cterm=NONE
hi BlueHover guifg=#83a5cb ctermfg=109 gui=NONE cterm=NONE
hi AquaHover guifg=#8ec07c ctermfg=108 gui=NONE cterm=NONE
hi WhiteHover guifg=#ffffff ctermfg=108 gui=NONE cterm=NONE

hi! link ALEErrorSign RedSign
hi! link ALEWarningSign YellowSign
hi! link ALEInfoSign BlueSign

hi link ALEVirtualTextError RedHover
hi link ALEVirtualTextWarning YellowHover
hi link ALEVirtualTextInfo WhiteHover
hi link ALEVirtualTextStyleError OrangeHover
hi link ALEVirtualTextStyleWarning BlueHover

hi! link CocHighlightText AquaHover
hi! link CocHighlightRead BlueHover
hi! link CocHighlightWrite GreenHover
hi! link CocErrorSign ALEErrorSign
hi! link CocWarningSign ALEWarningSign
hi! link CocInfoSign ALEInfoSign

hi default link CocErrorHighlight ALEError
hi default link CocWarningHighlight ALEWarning

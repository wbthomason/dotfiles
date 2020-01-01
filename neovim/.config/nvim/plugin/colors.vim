" Custom color tweaks

hi RedHover guifg=#cc241d ctermfg=124 gui=NONE cterm=NONE
hi YellowHover guifg=#fabd2f ctermfg=214 gui=NONE cterm=NONE
hi OrangeHover guifg=#fd7d2f ctermfg=214 gui=NONE cterm=NONE
hi GreenHover guifg=#b8cc26 ctermfg=142 gui=NONE cterm=NONE
hi BlueHover guifg=#83a5cb ctermfg=109 gui=NONE cterm=NONE
hi AquaHover guifg=#8ec07c ctermfg=108 gui=NONE cterm=NONE
hi WhiteHover guifg=#ffffff ctermfg=108 gui=NONE cterm=NONE

hi RedSign guifg=#cc241d ctermfg=124 gui=NONE cterm=NONE
hi YellowSign guifg=#fabd2f ctermfg=214 gui=NONE cterm=NONE
hi GreenSign guifg=#b8cc26 ctermfg=142 gui=NONE cterm=NONE
hi BlueSign guifg=#83a5cb ctermfg=109 gui=NONE cterm=NONE
hi AquaSign guifg=#8ec07c ctermfg=108 gui=NONE cterm=NONE

hi! link ALEErrorSign RedSign
hi! link ALEWarningSign YellowSign
hi! link ALEInfoSign BlueSign

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

hi default link CocErrorHighlight ALEError
hi default link CocWarningHighlight ALEWarning
hi default link CocErrorFloat ALEError
hi default link CocWarningFloat ALEWarning

hi ClapCurrentSelection guifg=#ffffff gui=bold
hi ClapSelected guifg=#dddddd gui=bold,underline
hi ClapInput guifg=#ebdbb2 guibg=#404040
hi ClapDisplay guifg=#ebdbb2 guibg=#505050
hi ClapNoMatchesFound guifg=#fe4559
hi ClapQuery guifg=#ffffff
hi ClapSpinner guifg=#ebdbb2 guibg=#404040
hi ClapMatches guifg=#aaddaa

let s:idx = 1
while s:idx < 9
  execute 'hi link ClapMatches' . s:idx . ' ClapMatches'
  execute 'hi link ClapFuzzyMatches' . s:idx . ' ClapMatches'
  let s:idx += 1
endwhile

while s:idx < 13
  execute 'hi link ClapFuzzyMatches' . s:idx . ' ClapMatches'
  let s:idx += 1
endwhile
unlet s:idx

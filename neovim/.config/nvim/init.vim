scriptencoding utf-8

" Maps for which-key
let g:lmaps = {}
let g:llmaps = {}

" Set Python host program to speed up loading
" let g:loaded_python_provider = 1
" let g:python_host_skip_check = 1
" let g:python3_host_skip_check = 1
" let g:loaded_python3_provider = 1
let g:python_host_prog = '/usr/bin/python2'
let g:python3_host_prog = '/usr/bin/python'

let g:loaded_2html_plugin      = 1
let g:loaded_logiPat           = 1
let g:loaded_getscriptPlugin   = 1
let g:loaded_gzip              = 1
let g:loaded_man               = 1
let g:loaded_matchit           = 1
let g:loaded_matchparen        = 1
" let g:loaded_netrwFileHandlers = 1
" let g:loaded_netrwPlugin       = 1
" let g:loaded_netrwSettings     = 1
let g:loaded_rrhelper          = 1
let g:loaded_shada_plugin      = 1
let g:loaded_spellfile_plugin  = 1
let g:loaded_tarPlugin         = 1
let g:loaded_tutor_mode_plugin = 1
let g:loaded_vimballPlugin     = 1
let g:loaded_zipPlugin = 1

" Colorscheme
set termguicolors
set background=dark
colorscheme nazgul

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

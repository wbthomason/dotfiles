hi clear Normal
set bg&

hi clear
if exists("syntax_on")
  syntax reset
endif

let colors_name = "custom"
set t_Co=256

" 255 white
" 152 ice blue
" 210 muted orange

hi Normal guifg=#eeeeee guibg=None ctermfg=255 ctermbg=None
hi Comment guifg=#afd7d7 guibg=None ctermfg=152 ctermbg=None
hi Constant guifg=#bcbcbc guibg=None ctermfg=250 ctermbg=None

hi! link Identifier Normal
hi! link Statement Normal
hi! link Type Statement
hi! link PreProc Type
hi! link LineNr Normal
hi! link SpecialComment Comment

hi! link javaDocComment Comment

hi! link pythonEscape Constant

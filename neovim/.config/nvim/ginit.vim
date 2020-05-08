set guifont=Fira\ Code\ Retina
" set guifont=Iosevka\ Slab:h11
let g:neovide_cursor_animation_length = 0.0
let g:neovide_cursor_trail_length = 0.0

if exists('g:GuiLoaded')
  GuiFont 'Fira Code Retina:h11'
  " GuiLinespace 8
elseif exists('g:GtkGuiLoaded')
  call rpcnotify(1, 'Gui', 'Font', 'Fira Code Retina 11')
  call rpcnotify(1, 'Gui', 'Option', 'Tabline', 0)
  call rpcnotify(1, 'Gui', 'Option', 'Cmdline', 0)
elseif exists('g:fvim_loaded')
  FVimFontAntialias v:true
  FVimFontAutohint v:true
  FVimFontSubpixel v:true
  FVimFontHintLevel 'full'
  FVimFontAutoSnap v:true
  FVimFontNoBuiltinSymbols v:true
  nnoremap <silent> <C-ScrollWheelUp> :set guifont=+<CR>
  nnoremap <silent> <C-ScrollWheelDown> :set guifont=-<CR>
  FVimBackgroundComposition 'none'
endif

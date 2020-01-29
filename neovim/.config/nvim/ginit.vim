set guifont=FuraCode\ Nerd\ Font:h11
" set guifont=Iosevka\ Slab:h11

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
  FVimFontLcdRender v:true
  FVimFontHintLevel 'full'
  FVimFontAutoSnap v:true
  nnoremap <silent> <C-ScrollWheelUp> :set guifont=+<CR>
  nnoremap <silent> <C-ScrollWheelDown> :set guifont=-<CR>
  " FVimCursorSmoothMove v:true
endif

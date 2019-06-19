if exists('g:GuiLoaded')
  GuiFont 'Fira Code Retina:h11'
  " GuiLinespace 8
elseif exists('g:GtkGuiLoaded')
  call rpcnotify(1, 'Gui', 'Font', 'Fira Code Retina 11')
  call rpcnotify(1, 'Gui', 'Option', 'Tabline', 0)
  call rpcnotify(1, 'Gui', 'Option', 'Cmdline', 0)
endif

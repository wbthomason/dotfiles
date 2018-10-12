setlocal omnifunc=necoghc#omnifunc
augroup haskell_aucommands
  au!
  au BufWritePost *.hs InteroReload
augroup END

" General autocommands

augroup main_aucommands
  autocmd!
  au BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \ exe "normal! g`\"" |
    \ endif
  au BufWinEnter * checktime
  au CompleteDone * pclose
augroup END

augroup neomake_aucommands
  "au BufWritePost * Neomake
  "au BufRead * Neomake
augroup END

augroup markdown_aucommands
  au BufNewFile,BufFilePre,BufRead,BufEnter *.md set filetype=pandoc
  au BufNewFile,BufFilePre,BufRead *.md set makeprg=make\ %:t:r
  au BufNewFile,BufFilePre,BufRead,BufEnter *.pandoc set filetype=pandoc
augroup END

augroup haskell_aucommands
  au FileType haskell setlocal omnifunc=necoghc#omnifunc
  au CursorHold,CursorHoldI *.hs :InteroType
augroup END

augroup tex_aucommands
  au FileType tex setlocal spell
  au BufNewFile,BufFilePre,BufRead *.tex set makeprg=make
augroup END

augroup misc_lang_aucommands
  au BufNewFile,BufFilePre,BufRead *.rs,Cargo.toml set makeprg=cargo\ build
  au BufWritePost *.scala silent :EnTypeCheck
augroup END

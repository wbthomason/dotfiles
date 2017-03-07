" General autocommands

augroup main_aucommands
  autocmd!
  au BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \ exe "normal! g`\"" |
    \ endif
  au BufWinEnter * checktime
  "au BufWritePost * Neomake
  "au BufRead * Neomake
  au CompleteDone * pclose
  au BufNewFile,BufFilePre,BufRead,BufEnter *.md set filetype=pandoc
  au BufNewFile,BufFilePre,BufRead *.md set makeprg=make\ %:t:r
  au BufNewFile,BufFilePre,BufRead *.tex set makeprg=make
  au BufNewFile,BufFilePre,BufRead *.rs,Cargo.toml set makeprg=cargo\ build
  au FileType haskell set omnifunc=necoghc#omnifunc
augroup END


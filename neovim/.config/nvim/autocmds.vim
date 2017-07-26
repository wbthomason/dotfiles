" General autocommands

augroup main_aucommands
  au!
  au BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \ exe "normal! g`\"" |
    \ endif
  au BufWinEnter * checktime
  au CompleteDone * pclose
  au InsertEnter * call deoplete#enable()
augroup END

"augroup neomake_aucommands
  "au!
  "au BufWritePost * Neomake
  "au BufRead * Neomake
  "autocmd ColorScheme *
    "\ hi NeomakeErrorSign guifg=#bf616a |
    "\ hi NeomakeWarningSign guifg=#ebcb8b |
    "\ hi NeomakeInfoSign guifg=#eceff4 |
    "\ hi NeomakeMessageSign guifg=#88c0d0 |
"augroup END

augroup ale_colors
  au!
  autocmd ColorScheme *
        \ hi ALEErrorSign guifg=#ff727b |
        \ hi ALEWarningSign guifg=#ebcb8b |
        \ hi ALEInfoSign guifg=#eceff4 |
        \ hi ALEStyleErrorSign guifg=#bf616a |
        \ hi ALEStyleWarningSign guifg=#ebfb8b |
        \ hi ALEError guifg=#ff727b |
        \ hi ALEWarning guifg=#ebcb8b |
        \ hi ALEInfo guifg=#eceff4 |
        \ hi ALEStyleError guifg=#bf616a |
        \ hi ALEStyleWarning guifg=#ebfb8b |
augroup END

augroup markdown_aucommands
  au!
  au BufNewFile,BufFilePre,BufRead,BufEnter *.md set filetype=pandoc
  au BufNewFile,BufFilePre,BufRead *.md set makeprg=make\ %:t:r
  au BufNewFile,BufFilePre,BufRead,BufEnter *.pandoc set filetype=pandoc
augroup END

augroup haskell_aucommands
  au!
  au FileType haskell setlocal omnifunc=necoghc#omnifunc
  au CursorHold,CursorHoldI *.hs :InteroType
augroup END

augroup tex_aucommands
  au!
  au FileType tex setlocal spell
  au BufNewFile,BufFilePre,BufRead *.tex set makeprg=make
augroup END

augroup OCaml_aucmds
  au FileType ocaml call Setup_Ocaml()
augroup END

augroup misc_lang_aucommands
  au!
  au BufNewFile,BufFilePre,BufRead *.rs,Cargo.toml set makeprg=cargo\ build
  au BufWritePost *.scala silent :EnTypeCheck
augroup END


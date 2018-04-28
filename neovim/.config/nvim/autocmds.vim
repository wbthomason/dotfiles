" General autocommands

augroup main_aucommands
  au!
  au BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \ exe "normal! g`\"" |
    \ endif
  au BufWinEnter * checktime
  au CompleteDone * pclose
  au FileType qf setlocal wrap
augroup END

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
  au FileType pandoc set syntax=pandoc | 
        \ silent :ALEDisableBuffer | 
augroup END

augroup haskell_aucommands
  au!
  au FileType haskell setlocal omnifunc=necoghc#omnifunc
  au! BufWritePost *.hs InteroReload
augroup END

augroup tex_aucommands
  au!
  au FileType tex setlocal spell |
        \ silent :ALEDisableBuffer |
augroup END

augroup misc_lang_aucommands
  au!
  au BufNewFile,BufFilePre,BufRead *.rs,Cargo.toml set makeprg=cargo\ build
  au BufWritePost *.scala silent :EnTypeCheck
  au BufRead,BufNewFile *.launch set filetype=roslaunch
augroup END

augroup python_aucommands
  au FileType python setl nosmartindent
augroup END

augroup lisp_aucommands
  " Also Racket, Scheme, etc
  au filetype lisp,scheme,racket setlocal equalprg=scmindent
augroup END

augroup scribble_aucommands
  au BufNewFile,BufFilePre,BufRead *.scrbl set filetype=scribble
augroup END

augroup cmake_aucommands
  au!
  au FileType cmake setlocal commentstring=#\ %s
augroup END

augroup vimfiler_aucommands
  au!
  au FileType vimfiler nmap <buffer> i :VimFilerPrompt<CR>
augroup END

augroup deoplete_aucommands
  au!
  au InsertEnter * call deoplete#enable()
augroup END

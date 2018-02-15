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
        \ silent :ALEDisable | 
        \ call cm#disable_for_buffer()
augroup END

augroup haskell_aucommands
  au!
  au FileType haskell setlocal omnifunc=necoghc#omnifunc
  au! BufWritePost *.hs InteroReload
augroup END

augroup tex_aucommands
  au!
  au FileType tex setlocal spell
  au BufNewFile,BufFilePre,BufRead *.tex set makeprg=make
augroup END

augroup misc_lang_aucommands
  au!
  au BufNewFile,BufFilePre,BufRead *.rs,Cargo.toml set makeprg=cargo\ build
  au BufWritePost *.scala silent :EnTypeCheck
  au BufRead,BufNewFile *.launch set filetype=roslaunch
augroup END

augroup python_aucommands
  au FileType python setl nosmartindent | let g:cm_sources_override.neoinclude = {'enable': 0}
augroup END

augroup cmake_aucommands
  au!
  au FileType cmake setlocal commentstring=#\ %s
augroup END

augroup vimfiler_aucommands
  au!
  au FileType vimfiler nmap <buffer> i :VimFilerPrompt<CR>
augroup END

augroup ncm_aucommands
  au!
  au User CmSetup call cm#register_source({
          \ 'name' : 'vimtex',
          \ 'priority': 8,
          \ 'scoping': 1,
          \ 'scopes': ['tex'],
          \ 'abbreviation': 'tex',
          \ 'cm_refresh_patterns': g:vimtex#re#ncm,
          \ 'cm_refresh': {'omnifunc': 'vimtex#complete#omnifunc'},
          \ })
  au User CmSetup call cm#register_source({
          \ 'name' : 'pandoc',
          \ 'priority': 8,
          \ 'scoping': 0,
          \ 'scopes': ['pandoc'],
          \ 'abbreviation': 'pandoc',
          \ 'cm_refresh_patterns': '@',
          \ 'cm_refresh': {'omnifunc': 'pandoc#completion#Complete'},
          \ })
  au User CmSetup call cm#register_source({
          \ 'name' : 'neco-ghc',
          \ 'priority': 8,
          \ 'scoping': 1,
          \ 'scopes': ['haskell'],
          \ 'abbreviation': 'hs',
          \ 'cm_refresh_patterns': 'import\s+',
          \ 'cm_refresh': {'omnifunc': 'necoghc#omnifunc'},
          \ })
augroup END
" augroup denite_aucommands
"   au!
"   au VimResized,VimEnter * call denite#custom#option('default',
"         \'winheight', winheight(0) / 4)
" augroup end

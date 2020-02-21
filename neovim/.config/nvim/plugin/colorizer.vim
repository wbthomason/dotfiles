augroup colorizer_load_aucmds
  au! 
  au VimEnter * lua require('colorizer').setup { 'css'; 'javascript'; 'vim'; 'html'; }
augroup END

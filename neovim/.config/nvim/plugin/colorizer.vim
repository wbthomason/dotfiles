augroup colorizer_load_aucmds
  au! 
  au InsertEnter * ++once lua require('colorizer').setup { 'css'; 'javascript'; 'vim'; 'html'; }
augroup END

if exists('g:vscode')
  nnoremap gt :call VSCodeNotify('latex.forwardSearch')<cr>
else
  " setlocal spell
  nnoremap gt :CocCommand latex.ForwardSearch<CR>
endif

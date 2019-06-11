" coc config (as much as happens outside of coc-settings.json)
augroup coc_aucommands
  au!
  au CursorHold * silent call CocActionAsync('highlight')
  au User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
  au User CocDiagnosticChange call lightline#update_once()
augroup END

nnoremap <silent> gO :CocList outline<CR>
nnoremap <silent> gS :call CocAction("workspaceSymbols")<CR>

" Use `[c` and `]c` to navigate diagnostics
nmap <silent> [c <Plug>(coc-diagnostic-prev)
nmap <silent> ]c <Plug>(coc-diagnostic-next)

" Remap keys for gotos
nmap <silent> gD <Plug>(coc-definition)
nmap <silent> gT <Plug>(coc-type-definition)
nmap <silent> gI <Plug>(coc-implementation)
nmap <silent> gR <Plug>(coc-references)

nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if &filetype == 'vim'
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

let g:lmaps.l = {'name': 'LSP'}

nmap <silent> <leader>ln <Plug>(coc-rename)
vmap <silent> <leader>lf  <Plug>(coc-format-selected)
nmap <silent> <leader>lf  :call CocAction('format')<cr>
vmap <silent> <leader>la  <Plug>(coc-codeaction-selected)
nmap <silent> <leader>la  <Plug>(coc-codeaction-selected)
nmap <silent> <leader>ac  <Plug>(coc-codeaction)
nmap <silent> <leader>lqf  <Plug>(coc-fix-current)

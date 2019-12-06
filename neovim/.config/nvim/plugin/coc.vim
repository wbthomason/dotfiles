scriptencoding utf8
" coc config (as much as happens outside of coc-settings.json)

let g:coc_force_debug = 1
augroup coc_load_aucommands
  au!
  au VimEnter * call s:load_coc()
augroup END

function! s:load_coc() abort
  packadd coc.nvim
  call coc#add_extension('coc-python',
      \'coc-rls',
      \'coc-tsserver',
      \'coc-lists',
      \'coc-omni',
      \'coc-gocode',
      \'coc-gitignore',
      \'coc-texlab',
      \'coc-syntax',
      \'coc-snippets',
      \'coc-python',
      \'coc-highlight',
      \'coc-vimlsp')
    
  augroup coc_aucommands
    au!
    au CursorHold * silent call CocActionAsync('highlight')
    au User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
    au User CocDiagnosticChange call statusline#force_update()
  augroup END

  au! coc_load_aucommands VimEnter *
endfunction

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
  if &filetype ==# 'vim'
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

nmap <silent> <leader>cn <Plug>(coc-rename)
vmap <silent> <leader>cf  <Plug>(coc-format-selected)
nmap <silent> <leader>cf  :call CocAction('format')<cr>
vmap <silent> <leader>ca  <Plug>(coc-codeaction-selected)
nmap <silent> <leader>ca  <Plug>(coc-codeaction-selected)
nmap <silent> <leader>ac  <Plug>(coc-codeaction)
nmap <silent> <leader>cqf  <Plug>(coc-fix-current)

inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

inoremap <silent><expr> <c-tab> coc#refresh()

inoremap <silent> <cr> <C-R>=Handle_cr_coc()<cr>

function! Handle_cr_coc() abort
  return pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
endfunction

inoremap <expr> <TAB> pumvisible() ? "\<C-y>" : "\<TAB>"
let g:coc_snippet_next = '<TAB>'
let g:coc_snippet_prev = '<S-TAB>'
let g:coc_status_error_sign = ' 🗙 '
let g:coc_status_warning_sign = ' ⚠ '
" nmap <silent> <TAB> <Plug>(coc-range-select)
xmap <silent> <TAB> <Plug>(coc-range-select)
xmap <silent> <S-TAB> <Plug>(coc-range-select-backward)

xmap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap if <Plug>(coc-funcobj-i)
omap af <Plug>(coc-funcobj-a)

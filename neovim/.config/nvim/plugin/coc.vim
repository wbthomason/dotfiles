scriptencoding utf8
" coc config (as much as happens outside of coc-settings.json)

let g:coc_force_debug = 1
if !exists('g:vscode')
  augroup coc_load_aucommands
    au!
    au VimEnter * call s:load_coc()
  augroup END
endif

function! s:load_coc() abort
  packadd coc.nvim
  call coc#add_extension(
        \'coc-gitignore',
        \'coc-gocode',
        \'coc-highlight',
        \'coc-lists',
        \'coc-python',
        \'coc-python',
        \'coc-rust-analyzer',
        \'coc-snippets',
        \'coc-syntax',
        \'coc-texlab',
        \'coc-tsserver',
        \'coc-vimlsp'
        \)

  if exists("g:fvim_loaded")
    call coc#config('suggest.completionItemKindLabels', {
          \ "text": "t",
          \ "method": ":",
          \ "function": "f",
          \ "constructor": "c",
          \ "field": ".",
          \ "variable": "v",
          \ "class": "C",
          \ "interface": "I",
          \ "module": "M",
          \ "property": "p",
          \ "unit": "U",
          \ "value": "l",
          \ "enum": "E",
          \ "keyword": "k",
          \ "snippet": "s",
          \ "color": "K",
          \ "file": "F",
          \ "reference": "r",
          \ "folder": "d",
          \ "enumMember": "m",
          \ "constant": "0",
          \ "struct": "S",
          \ "event": "e",
          \ "operator": "o",
          \ "typeParameter": "T"
          \ })
  endif

  augroup coc_aucommands
    au!
    au CursorHold * silent call CocActionAsync('highlight')
    au User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
    au User CocDiagnosticChange redrawstatus!
    au FileType cpp,c,python,rust call s:map_funcobs()
    au FileType python let b:coc_root_patterns = ['pyproject.toml', '.git', '.env']
  augroup END

  autocmd! coc_load_aucommands
  augroup! coc_load_aucommands
  doautoall coc_aucommands FileType
endfunction

function! s:map_funcobs() abort
  xmap <buffer> if <Plug>(coc-funcobj-i)
  xmap <buffer> af <Plug>(coc-funcobj-a)
  omap <buffer> if <Plug>(coc-funcobj-i)
  omap <buffer> af <Plug>(coc-funcobj-a)
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

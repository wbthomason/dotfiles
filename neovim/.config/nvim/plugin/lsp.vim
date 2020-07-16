" This disables the fairly slow loading of server installation commands, which we don't really use
" anyway
let g:nvim_lsp = 1

function! s:load_lsp() abort
  packadd nvim-lsp
  packadd completion-nvim
  packadd vim-vsnip
  packadd vim-vsnip-integ
  packadd diagnostic-nvim
  lua << END
    require('completion').addCompletionSource('vimtex', require('vimtex').complete_item)
    require('completion').addCompletionSource('wiki', require('wiki').complete_item)
END

  execute 'luafile ' . stdpath('config') . '/lua/lsp.lua'
  execute 'luafile ' . stdpath('config') . '/lua/dap-config.lua'
  augroup lsp_use_aucmd
    au!
    au BufEnter * lua require('completion').on_attach()
  augroup END

  lua require('completion').on_attach()
  doautoall FileType
endfunction

augroup lsp_load_aucommands
  au InsertEnter * ++once call s:load_lsp()
augroup END

command! -nargs=0 LoadLsp call s:load_lsp()

let g:completion_trigger_on_delete = 1
" function! s:check_back_space() abort
"     let col = col('.') - 1
"     return !col || getline('.')[col - 1]  =~ '\s'
" endfunction
" inoremap <silent><expr> <TAB>
"   \ pumvisible() ? "\<C-n>" :
"   \ <SID>check_back_space() ? "\<TAB>" :
"   \ completion#trigger_completion()
" inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
" Set completeopt to have a better completion experience
set completeopt=menuone,noinsert,noselect

" Avoid showing message extra message when using completion
set shortmess+=c

let g:completion_max_items = 30

let g:completion_confirm_key = ''
inoremap <silent> <Plug>(ComplCREnd) <cr>
imap <silent><expr> <Plug>ComplCR pumvisible() ? complete_info()['selected'] != -1 ? "\<Plug>(completion_confirm_completion)"  : "\<c-e>\<CR>" :  "\<Plug>(ComplCREnd)"
imap <silent><cr> <Plug>ComplCR<Plug>CloserClose<Plug>DiscretionaryEnd

call sign_define('LspDiagnosticsErrorSign', {'text' : '🗙', 'texthl' : 'RedHover'})
call sign_define('LspDiagnosticsWarningSign', {'text' : '➤', 'texthl' : 'YellowHover'})
call sign_define('LspDiagnosticsInformationSign', {'text' : '🛈', 'texthl' : 'WhiteHover'})
call sign_define('LspDiagnosticsHintSign', {'text' : '❗', 'texthl' : 'CocHintHighlight'})

let g:completion_enable_snippet = 'vim-vsnip'
imap <expr> <Tab> pumvisible() ? "\<c-n>" : (vsnip#available(1) ? '<Plug>(vsnip-expand)' : "\<Tab>")
imap <expr> <C-j> vsnip#available(1) ? '<Plug>(vsnip-jump-next)' : "\<C-j>"
smap <expr> <C-j> vsnip#available(1) ? '<Plug>(vsnip-jump-next)' : "\<C-j>"
imap <expr> <C-k> vsnip#available(-1) ? '<Plug>(vsnip-jump-prev)' : "\<C-k>"
smap <expr> <C-k> vsnip#available(-1) ? '<Plug>(vsnip-jump-prev)' : "\<C-k>"
let g:completion_auto_change_source = 1
let g:completion_chain_complete_list = {
      \ 'default' : {
      \   'default': [
      \       {'complete_items': ['lsp']},
      \       {'mode': '<c-p>'},
      \       {'mode': '<c-n>'},
      \],
      \   'comment': [],
      \   'string' : [
      \       {'complete_items': ['path'], 'triggered_only': ['/']}]
      \},
      \ 'tex': {
      \     'default': [
      \     { 'complete_items': ['lsp'] },
      \     { 'complete_items': ['vimtex'] },
      \     { 'mode': '<c-p>' },
      \     { 'mode': '<c-n>' },
      \],
      \     'comment': [],
      \     'string' : [ {'complete_items': ['path']} ]
      \},
      \ 'markdown': {
      \     'default': [
      \     { 'complete_items': ['wiki'] },
      \     { 'mode': '<c-p>' },
      \     { 'mode': '<c-n>' },
      \]
      \},
      \ 'lisp': {
      \     'default': [
      \     { 'complete_items': ['vlime'] },
      \     { 'mode': '<c-p>' },
      \     { 'mode': '<c-n>' },
      \],
      \     'comment': [],
      \     'string' : [ {'complete_items': ['path']} ]
      \}
      \}

let g:completion_customize_lsp_label = {
      \ 'Function': "\uf794",
      \ 'Method': "\uf6a6",
      \ 'Variable': "\uf71b",
      \ 'Constant': "\uf8ff",
      \ 'Struct': "\ufb44",
      \ 'Class': "\uf0e8",
      \ 'Interface': "\ufa52",
      \ 'Text': "\ue612",
      \ 'Enum': "\uf435",
      \ 'EnumMember': "\uf02b",
      \ 'Module': "\uf668",
      \ 'Color': "\ue22b",
      \ 'Property': "\ufab6",
      \ 'Field': "\uf93d",
      \ 'Unit': "\uf475",
      \ 'File': "\uf471",
      \ 'Value': "\uf8a3",
      \ 'Event': "\ufacd",
      \ 'Folder': "\uf115",
      \ 'Keyword': "\uf893",
      \ 'Snippet': "\uf64d",
      \ 'Operator': "\uf915",
      \ 'Reference': "\uf87a",
      \ 'TypeParameter': "\uf278",
      \ 'Default': "\uf29c"
      \}

let g:diagnostic_insert_delay = 1

nnoremap <silent> <F5> :lua require'dap'.continue()<CR>
nnoremap <silent> <F10> :lua require'dap'.step_over()<CR>
nnoremap <silent> <F11> :lua require'dap'.step_into()<CR>
nnoremap <silent> <F12> :lua require'dap'.step_out()<CR>
nnoremap <silent> <leader>b :lua require'dap'.toggle_breakpoint()<CR>
nnoremap <silent> <leader>B :lua require'dap'.toggle_breakpoint(vim.fn.input('Breakpoint condition: '))<CR>
nnoremap <silent> <leader>R :lua require'dap'.repl.open()<CR>
nnoremap <silent> <leader>L :lua require'dap'.repl.run_last()<CR>

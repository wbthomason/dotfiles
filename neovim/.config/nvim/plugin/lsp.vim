" This disables the fairly slow loading of server installation commands, which we don't really use
" anyway
let g:nvim_lsp = 1

" let g:completion_confirm_key = ''
let g:completion_max_items = 30
let g:completion_trigger_on_delete = 1
let g:completion_enable_snippet = 'snippets.nvim'
" imap <expr> <Tab> pumvisible() ? "\<c-n>" : (vsnip#available(1) ? '<Plug>(vsnip-expand)' : "\<Tab>")
" imap <expr> <C-j> vsnip#available(1) ? '<Plug>(vsnip-jump-next)' : "\<C-j>"
" smap <expr> <C-j> vsnip#available(1) ? '<Plug>(vsnip-jump-next)' : "\<C-j>"
" imap <expr> <C-k> vsnip#available(-1) ? '<Plug>(vsnip-jump-prev)' : "\<C-k>"
" smap <expr> <C-k> vsnip#available(-1) ? '<Plug>(vsnip-jump-prev)' : "\<C-k>"

set completeopt=menuone,noinsert,noselect
set shortmess+=c

execute 'luafile ' . stdpath('config') . '/lua/lsp.lua'

imap <tab> <Plug>(completion_smart_tab)
imap <s-tab> <Plug>(completion_smart_s_tab)

call sign_define('LspDiagnosticsErrorSign', {'text' : '🗙', 'texthl' : 'RedHover'})
call sign_define('LspDiagnosticsWarningSign', {'text' : '➤', 'texthl' : 'YellowHover'})
call sign_define('LspDiagnosticsInformationSign', {'text' : '🛈', 'texthl' : 'WhiteHover'})
call sign_define('LspDiagnosticsHintSign', {'text' : '❗', 'texthl' : 'CocHintHighlight'})

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

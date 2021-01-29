" This disables the fairly slow loading of server installation commands, which we don't really use
" anyway
let g:nvim_lsp = 1
set completeopt=menuone,noinsert,noselect
set shortmess+=c

execute 'luafile ' . stdpath('config') . '/lua/lsp.lua'

inoremap <silent><expr> <c-c> compe#complete()
inoremap <silent><expr> <CR>      compe#confirm('<CR>')
inoremap <silent><expr> <C-e>     compe#close('<C-e>')

" imap <tab> <Plug>(completion_smart_tab)
" imap <s-tab> <Plug>(completion_smart_s_tab)

" call sign_define('LspDiagnosticsSignError', {'text' : '🗙', 'texthl' : 'RedHover'})
" call sign_define('LspDiagnosticsSignWarning', {'text' : '➤', 'texthl' : 'YellowHover'})
" call sign_define('LspDiagnosticsSignInformation', {'text' : '🛈', 'texthl' : 'WhiteHover'})
" call sign_define('LspDiagnosticsSignHint', {'text' : '❗', 'texthl' : 'CocHintHighlight'})

call sign_define("LspDiagnosticsSignError", {'text': "", 'numhl': "RedSign"})
call sign_define("LspDiagnosticsSignWarning", {'text': "", 'numhl': "YellowSign"})
call sign_define("LspDiagnosticsSignInformation", {'text': "", 'numhl': "WhiteSign"})
call sign_define("LspDiagnosticsSignHint", {'text': "", 'numhl': "BlueSign"})

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

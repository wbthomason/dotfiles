function! s:load_lsp() abort
  packadd nvim-lsp
  packadd vim-vsnip
  packadd vim-vsnip-integ
  packadd completion-nvim
  packadd diagnostic-nvim
  execute 'luafile ' . stdpath('config') . '/lua/lsp.lua'
  augroup lsp_use_aucmd
    au!
    au BufEnter * lua require('completion').on_attach()
  augroup END

  lua require('completion').on_attach()
  doautoall FileType
endfunction

augroup lsp_load_aucommands
  au!
  au VimEnter * ++once call s:load_lsp()
augroup END

" Use <Tab> and <S-Tab> to navigate through popup menu
function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~ '\s'
endfunction

inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ completion#trigger_completion()
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

" Set completeopt to have a better completion experience
set completeopt=menuone,noinsert,noselect

" Avoid showing message extra message when using completion
set shortmess+=c

let g:completion_max_items = 20
let g:completion_confirm_key = "\<c-y>"
inoremap <silent><expr> <cr> <sid>handle_cr()

function! s:handle_cr() abort
  return pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
endfunction

call sign_define('LspDiagnosticsErrorSign', {'text' : "🗙", 'texthl' : 'RedHover'})
call sign_define('LspDiagnosticsWarningSign', {'text' : "➤", 'texthl' : 'YellowHover'})
call sign_define('LspDiagnosticsInformationSign', {'text' : "🛈", 'texthl' : 'WhiteHover'})
call sign_define('LspDiagnosticsHintSign', {'text' : "❗", 'texthl' : 'CocHintHighlight'})

let g:completion_auto_change_source = 0
let g:completion_chain_complete_list = {
      \ 'default' : {
      \   'default': [
      \       {'complete_items': ['lsp', 'snippet']},
      \       {'mode': '<c-p>'},
      \       {'mode': '<c-n>'},
      \],
      \   'comment': [],
      \   'string' : [
      \     {'complete_items': ['path']}
      \   ]
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

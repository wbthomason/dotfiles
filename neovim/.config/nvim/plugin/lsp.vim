finish
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

augroup completion_nvim_trigger_character_setup
    au!
    autocmd BufEnter * let g:completion_trigger_character = ['.']
    autocmd BufEnter *.c,*.cpp,*.h,*.cc,*.hh let g:completion_trigger_character = ['.', '::', '->']
augroup end

let g:LspDiagnosticsErrorSign = '🗙'
let g:LspDiagnosticsWarningSign = '➤'
let g:LspDiagnosticsInformationSign = 'ℹ'
let g:LspDiagnosticsHintSign = '❗'

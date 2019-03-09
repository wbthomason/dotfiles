inoremap <buffer> <silent> " <c-r>=neosnippet#anonymous("\\`\\`${1}''${0}")<cr>
setlocal spell
" silent :ALEDisableBuffer
" call ncm2#register_source({
"         \ 'name': 'vimtex',
"         \ 'priority': 8,
"         \ 'scope': ['tex'],
"         \ 'mark': 'tex',
"         \ 'word_pattern': '\w+',
"         \ 'complete_pattern': g:vimtex#re#ncm2,
"         \ 'on_complete': ['ncm2#on_complete#omni', 'vimtex#complete#omnifunc'],
"         \ })

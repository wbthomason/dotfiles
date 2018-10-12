inoremap <buffer> <silent> " "<c-r>=UltiSnips#Anon("\\`\\`$1''$0", '"', '', 'i')<cr>
setlocal spell
silent :ALEDisableBuffer
call ncm2#register_source({
        \ 'name': 'vimtex',
        \ 'priority': 8,
        \ 'scope': ['tex'],
        \ 'mark': 'tex',
        \ 'word_pattern': '\w+',
        \ 'complete_pattern': g:vimtex#re#ncm2,
        \ 'on_complete': ['ncm2#on_complete#omni', 'vimtex#complete#omnifunc'],
        \ })

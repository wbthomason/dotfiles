" Lightline
let g:lightline = {}
let g:lightline.colorscheme = 'Tomorrow_Night_Eighties'
let g:lightline.enable = {'tabline': 0}
let g:lightline.active = {
      \ 'left': [ [ 'mode', 'paste' ],
      \           [ 'vcstatus', 'filename', 'modified', 'readonly' ] ],
      \ 'right': [ [ 'cocstatus' ],
      \            [ 'linter_checking', 'linter_errors', 'linter_warnings', 'linter_ok' ],
      \            [ 'gutentags'],
      \            [ 'percent' ],
      \            [ 'filetype' ] ]}

let g:lightline.component_function = {
      \ 'vcstatus': 'VCStatus',
      \ 'filetype': 'IconFileType',
      \ 'cocstatus': 'coc#status'}

let g:lightline.component = {
      \ 'lineinfo': ' %3l:%-2v',
      \ 'readonly': "%{&readonly ? '' : ''}"}

let g:lightline.component_expand = {
      \  'linter_checking': 'lightline#ale#checking',
      \  'linter_warnings': 'lightline#ale#warnings',
      \  'linter_errors': 'lightline#ale#errors',
      \  'linter_ok': 'lightline#ale#ok',
      \  'gutentags': 'gutentags#statusline',
      \ }

let g:lightline.component_type = {
      \     'linter_checking': 'left',
      \     'linter_warnings': 'warning',
      \     'linter_errors': 'error',
      \     'linter_ok': 'left',
      \ }

let g:lightline#ale#indicator_checking = "\uf110"
let g:lightline#ale#indicator_warnings = "\uf071"
let g:lightline#ale#indicator_errors = "\uf05e"
let g:lightline#ale#indicator_ok = "\uf00c"

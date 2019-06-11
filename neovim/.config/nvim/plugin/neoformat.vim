" Neoformat
let g:neoformat_python_yapf = {
      \ 'exe': 'yapf',
      \ 'stdin': 1,
      \}
let g:neoformat_ocaml_ocamlformat = {
      \ 'exe': 'ocamlformat',
      \ 'args': ['--inplace', '-m 100'],
      \ 'replace': 1,
      \ }
let g:neoformat_cpp_clangformat = {
      \ 'exe': 'clang-format',
      \ 'stdin': 1,
      \ 'args': ['--style=file', '--assume-filename=code.cc'],
      \ }

let g:neoformat_basic_format_trim = 1

let g:dirvish_git_indicators = {
  \ 'Modified'  : '＊',
  \ 'Staged'    : '＋',
  \ 'Untracked' : '·',
  \ 'Renamed'   : '➜',
  \ 'Unmerged'  : '═',
  \ 'Ignored'   : '☒',
  \ 'Unknown'   : '?'
  \ }

command! -nargs=? -complete=dir Explore Dirvish <args>
command! -nargs=? -complete=dir Sexplore belowright split | silent Dirvish <args>
command! -nargs=? -complete=dir Vexplore leftabove vsplit | silent Dirvish <args>

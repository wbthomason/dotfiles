" Custom functions

function! Toggle_writer_mode()
  execute ":Goyo"
  execute ":Limelight!!"
endfunction

function! StartifyEntryFormat()
  return 'WebDevIconsGetFileTypeSymbol(absolute_path) ." ". entry_path'
endfunction

function! LinterStatus() abort
  let l:counts = ale#statusline#Count(bufnr(''))

  let l:errors = l:counts.error + l:counts.style_error
  let l:warnings = l:counts.warning + l:counts.style_warning
  return l:counts.total == 0 ? '⬥ ok' : 
        \ printf('⚠ %d', l:warnings) . (l:errors == 0 ? '' :
        \ printf(' ⨉ %d', l:errors))
endfunction

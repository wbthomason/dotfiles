" Custom functions

function! Toggle_writer_mode()
  execute ":Goyo"
  execute ":Limelight!!"
endfunction

function! Writer_checks_on()
  execute ":GrammarousCheck"
  execute ":DittoOn"
endfunction

function! Writer_checks_off()
  execute ":GrammarousReset"
  execute ":DittoOff"
endfunction

" Stolen from tracyone/t-vim

function! s:get_input() abort
  let col = col( '.' )
  let line = getline( '.' )
  if col - 1 < len( line )
    return matchstr( line, '^.*\%' . col . 'c' )
  endif
  return line
endfunction

function! Vim_complete( findstart, base ) abort
  let line_prefix = s:get_input()
  if a:findstart
    let ret = necovim#get_complete_position( line_prefix )
    if ret < 0
      return col( '.' ) " default to current
    endif
    return ret
  else
    return necovim#gather_candidates( line_prefix . a:base, a:base )
  endif
endfunction

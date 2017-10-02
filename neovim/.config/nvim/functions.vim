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

function! TrimGuideDisplay()
  let g:leaderGuide#displayname =
        \ substitute(g:leaderGuide#displayname, '\c<cr>$', '', '')
  let g:leaderGuide#displayname = 
        \ substitute(g:leaderGuide#displayname, '^<Plug>', '', '')
endfunction

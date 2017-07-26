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

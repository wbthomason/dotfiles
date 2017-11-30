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


" These take time, so we only want to run them if we're editing OCaml
function! Setup_Ocaml()
  let g:opamshare = substitute(system('opam config var share'),'\n$','','''')
  execute 'set rtp+=' . g:opamshare . '/merlin/vim'
  execute 'helptags ' . g:opamshare . '/merlin/vim/doc'
  execute 'set rtp^=' . g:opamshare . '/ocp-indent/vim'
  execute 'set rtp+=' . g:opamshare . '/ocp-index/vim'
endfunction

" Stolen from https://github.com/saaguero/dotvim
function! LoadUltiSnips()
  let l:curpos = getcurpos()
  execute plug#load('ultisnips')
  call cursor(l:curpos[1], l:curpos[2])
  call UltiSnips#ExpandSnippet()
  return ""
endfunction

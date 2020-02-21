scriptencoding utf-8
if exists('g:loaded_statusline')
  finish
endif

if exists('g:vscode')
  set statusline=
  let g:loaded_statusline = v:true
  finish
endif

set noshowmode
set laststatus=2
function! UpdateColors(mode) abort
  " Normal mode
  if a:mode ==# 'n'
    hi StatuslineAccent guibg=#d75f5f gui=bold guifg=#e9e9e9
    " Insert mode
  elseif a:mode ==# 'i'
    hi StatuslineAccent guifg=#e9e9e9 gui=bold guibg=#dab997
    " Replace mode
  elseif a:mode ==# 'R'
    hi StatuslineAccent guifg=#e9e9e9 gui=bold guibg=#afaf00
    " Command mode
  elseif a:mode ==# 'c'
    hi StatuslineAccent guifg=#e9e9e9 gui=bold guibg=#83adad
    " Terminal mode
  elseif a:mode ==# 't'
    hi StatuslineAccent guifg=#e9e9e9 gui=bold guibg=#6f6f6f
    " Visual mode
  else
    hi StatuslineAccent guifg=#e9e9e9 gui=bold guibg=#f485dd
  endif

  if &modified
    hi StatuslineFilename guifg=#d75f5f gui=bold guibg=#3a3a3a
  else
    hi StatuslineFilename guifg=#e9e9e9 gui=bold guibg=#3a3a3a
  endif
  " Return empty string so as not to display anything in the statusline
  return ''
endfunction

function! SetModifiedSymbol(modified) abort
  if a:modified == 1
    hi StatuslineModified guibg=#3a3a3a gui=bold guifg=#d75f5f
    return '  ●'
  else
    hi StatuslineModified guibg=#3a3a3a gui=bold guifg=#afaf00
    return ''
  endif
endfunction

set statusline=%{UpdateColors(mode())}

" Left side items
set statusline+=%#StatuslineAccent#\ %{statusline#get_mode(mode())}\ 

" Filetype icon
set statusline+=%#StatuslineFiletype#\ %{statusline#icon()}

" Modified status
set statusline+=%#StatuslineModified#%{SetModifiedSymbol(&modified)}

" Filename
set statusline+=%#StatuslineFilename#\ %.50f\ %<

" Paste and RO
set statusline+=%#StatuslineFilename#%{&paste?'PASTE\ ':''}
set statusline+=%{&paste&&&readonly?'\ ':''}%r%{&readonly?'\ ':''}

" Line and Column
set statusline+=%#StatuslineLineCol#(Ln\ %l/%L,\ %#StatuslineLineCol#Col\ %c)\ %<

" Right side items
set statusline+=%=

" Filetype
" set statusline+=%#StatuslineFiletype#%{statusline#filetype()}\ \ 

" Current scroll percentage and total lines of the file
" set statusline+=%#StatuslinePercentage#%p%%\ (%L)\ \

" VCS
set statusline+=%#StatuslineVC#%{statusline#vc_status()}\ 

" ALE status
set statusline+=%{statusline#ale_enabled()?'\ ':''}%(%#StatuslineLint#%{statusline#ale()}%)

" coc
set statusline+=%{g:coc_enabled?'':'\ '}%(%#StatuslineLint#%{statusline#coc()}%)

" Setup the colors
function! s:setup_colors() abort
  hi StatusLine          guifg=#d485ad     guibg=NONE     gui=NONE
  hi StatusLineNC        guifg=#d75f5f     guibg=NONE     gui=bold

  hi StatuslineSeparator guifg=#3a3a3a gui=none guibg=none

  hi StatuslineFiletype guifg=#d9d9d9 gui=none guibg=#3a3a3a

  hi StatuslinePercentage guibg=#3a3a3a gui=none guifg=#dab997

  hi StatuslineNormal guibg=#3a3a3a gui=none guifg=#e9e9e9
  hi StatuslineVC guibg=#3a3a3a gui=none guifg=#a9a9a9

  hi StatuslineLintWarn guibg=#3a3a3a gui=none guifg=#ffcf00
  hi StatuslineLintChecking guibg=#3a3a3a gui=none guifg=#458588
  hi StatuslineLintError guibg=#3a3a3a gui=none guifg=#d75f5f
  hi StatuslineLintOk guibg=#3a3a3a gui=none guifg=#b8bb26
  hi StatuslineLint guibg=#e9e9e9 guifg=#3a3a3a

  hi StatuslineLineCol guibg=#3a3a3a gui=none guifg=#878787

  hi StatuslineFiletype guibg=#3a3a3a gui=none guifg=#e9e9e9

endfunction

augroup statusline_colors
  au!
  au ColorScheme * call s:setup_colors()
augroup END

call s:setup_colors()

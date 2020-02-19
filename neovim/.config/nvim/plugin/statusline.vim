scriptencoding utf-8
if exists('g:loaded_statusline')
  finish
endif

if exists('g:vscode')
  set statusline=
  let g:loaded_statusline = v:true
  finish
endif

" Adapted and modified from https://github.com/elenapan/dotfiles/blob/master/config/nvim/statusline.vim

set noshowmode

set laststatus=2
function! RedrawModeColors(mode)
  " Normal mode
  if a:mode ==# 'n'
    hi StatuslineAccent guifg=#d75f5f gui=NONE guibg=NONE
    hi StatuslineFilename guifg=#e9e9e9 gui=bold guibg=#3a3a3a
    hi StatuslineAccentBody guibg=#d75f5f gui=bold guifg=#e9e9e9
  " Insert mode
  elseif a:mode ==# 'i'
    hi StatuslineAccent guifg=#dab997 gui=NONE guibg=NONE
    hi StatuslineFilename guifg=#dab997 gui=bold guibg=#3a3a3a
    hi StatuslineAccentBody guifg=#e9e9e9 gui=bold guibg=#dab997
  " Replace mode
  elseif a:mode ==# 'R'
    hi StatuslineAccent guifg=#afaf00 gui=NONE guibg=NONE
    hi StatuslineFilename guifg=#afaf00 gui=bold guibg=#3a3a3a
    hi StatuslineAccentBody guifg=#e9e9e9 gui=bold guibg=#afaf00
  " Visual mode
  elseif a:mode ==? 'v' || a:mode ==# '^V'
    hi StatuslineAccent guifg=#f485dd gui=NONE guibg=NONE
    hi StatuslineFilename guifg=#f485dd gui=bold guibg=#3a3a3a
    hi StatuslineAccentBody guifg=#e9e9e9 gui=bold guibg=#f485dd
  " Command mode
  elseif a:mode ==# 'c'
    hi StatuslineAccent guifg=#83adad gui=NONE guibg=NONE
    hi StatuslineFilename guifg=#83adad gui=bold guibg=#3a3a3a
    hi StatuslineAccentBody guifg=#e9e9e9 gui=bold guibg=#83adad
  " Terminal mode
  elseif a:mode ==# 't'
    hi StatuslineAccent guifg=#6f6f6f gui=NONE guibg=NONE
    hi StatuslineFilename guifg=#e9e9e9 gui=bold guibg=#3a3a3a
    hi StatuslineAccentBody guifg=#e9e9e9 gui=bold guibg=#6f6f6f
  endif
  " Return empty string so as not to display anything in the statusline
  return ''
endfunction

function! SetModifiedSymbol(modified)
    if a:modified == 1
        hi StatuslineModifiedBody guibg=#3a3a3a gui=bold guifg=#d75f5f
        return ' ●'
    else
        hi StatuslineModifiedBody guibg=#3a3a3a gui=bold guifg=#afaf00
        return ' ✔'
    endif
endfunction

set statusline=%{RedrawModeColors(mode())}

" Left side items
set statusline+=%#StatuslineAccentBody#\ %{util#get_mode(mode())}\ 

" Filename
set statusline+=%#StatuslineFilename#\ \ %.60f\ 

" Modified status
set statusline+=%#StatuslineModifiedBody#%{SetModifiedSymbol(&modified)}\ \ 

" Paste and RO
set statusline+=%#StatuslineFilename#%{&paste?'PASTE\ ':''}
set statusline+=%{&paste&&&readonly?'\ ':''}%r%{&readonly?'\ ':''}

" VCS
set statusline+=%#StatuslineVC#%{statusline#vc_status()}

" Right side items
set statusline+=%=

" Filetype
set statusline+=%#StatuslineFiletypeBody#%{statusline#icon_filetype()}\ \ 

" Line and Column
set statusline+=%#StatuslineLineColBody#(Ln\ %l/%L,\ %#StatuslineLineColBody#Col\ %c)\ 

" Current scroll percentage and total lines of the file
" set statusline+=%#StatuslinePercentageBody#%p%%\ (%L)\ \ 

" ALE status
set statusline+=%{statusline#has_ale()?'\ ':''}%(%#StatuslineLint#%{statusline#ale_status()}%)

" coc
set statusline+=%{coc#status()==''?'':'\ '}%(%#StatuslineLint#%{statusline#coc_status()}%)

" Setup the colors
function! s:setup_colors() abort
  hi StatusLine          guifg=#d485ad     guibg=NONE     gui=NONE
  hi StatusLineNC        guifg=#d75f5f     guibg=NONE     gui=bold

  hi StatuslineSeparator guifg=#3a3a3a gui=none guibg=none

  hi StatuslineFiletypeBody guifg=#d9d9d9 gui=none guibg=#3a3a3a

  hi StatuslinePercentageBody guibg=#3a3a3a gui=none guifg=#dab997

  hi StatuslineNormalBody guibg=#3a3a3a gui=none guifg=#e9e9e9
  hi StatuslineVC guibg=#3a3a3a gui=none guifg=#a9a9a9

  hi StatuslineLintWarn guibg=#3a3a3a gui=none guifg=#ffcf00
  hi StatuslineLintChecking guibg=#3a3a3a gui=none guifg=#458588
  hi StatuslineLintError guibg=#3a3a3a gui=none guifg=#d75f5f
  hi StatuslineLintOk guibg=#3a3a3a gui=none guifg=#b8bb26
  hi StatuslineLint guibg=#e9e9e9 guifg=#3a3a3a

  hi StatuslineLineColBody guibg=#3a3a3a gui=none guifg=#878787

endfunction

augroup statusline_colors
  au!
  au ColorScheme * call s:setup_colors()
augroup END

call s:setup_colors()

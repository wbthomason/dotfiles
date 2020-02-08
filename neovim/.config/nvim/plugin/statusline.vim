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
    hi StatuslineFilename guifg=#e9e9e9 gui=none guibg=#3a3a3a
    hi StatuslineAccentBody guibg=#d75f5f gui=NONE guifg=#e9e9e9
  " Insert mode
  elseif a:mode ==# 'i'
    hi StatuslineAccent guifg=#dab997 gui=NONE guibg=NONE
    hi StatuslineFilename guifg=#dab997 gui=none guibg=#3a3a3a
    hi StatuslineAccentBody guifg=#e9e9e9 gui=NONE guibg=#dab997
  " Replace mode
  elseif a:mode ==# 'R'
    hi StatuslineAccent guifg=#afaf00 gui=NONE guibg=NONE
    hi StatuslineFilename guifg=#afaf00 gui=none guibg=#3a3a3a
    hi StatuslineAccentBody guifg=#e9e9e9 gui=NONE guibg=#afaf00
  " Visual mode
  elseif a:mode ==? 'v' || a:mode ==# '^V'
    hi StatuslineAccent guifg=#f485dd gui=NONE guibg=NONE
    hi StatuslineFilename guifg=#f485dd gui=none guibg=#3a3a3a
    hi StatuslineAccentBody guifg=#e9e9e9 gui=NONE guibg=#f485dd
  " Command mode
  elseif a:mode ==# 'c'
    hi StatuslineAccent guifg=#83adad gui=NONE guibg=NONE
    hi StatuslineFilename guifg=#83adad gui=none guibg=#3a3a3a
    hi StatuslineAccentBody guifg=#e9e9e9 gui=NONE guibg=#83adad
  " Terminal mode
  elseif a:mode ==# 't'
    hi StatuslineAccent guifg=#6f6f6f gui=NONE guibg=NONE
    hi StatuslineFilename guifg=#e9e9e9 gui=none guibg=#3a3a3a
    hi StatuslineAccentBody guifg=#e9e9e9 gui=NONE guibg=#6f6f6f
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
set statusline+=%#StatuslineAccent#
set statusline+=%#StatuslineAccentBody#⏺\ 

" Filename
set statusline+=%#StatuslineFilename#\ %.60f

" Modified status
set statusline+=%#StatuslineModifiedBody#%{SetModifiedSymbol(&modified)}
set statusline+=%#StatuslineSeparator#\ 

" Paste and RO
set statusline+=%#StatuslineSeparator#%{&paste\|\|&readonly?'':''}
set statusline+=%#StatuslineFilename#%{&paste?'PASTE':''}
set statusline+=%{&paste&&&readonly?'\ ':''}%r
set statusline+=%#StatuslineSeparator#%{&paste\|\|&readonly?'\ ':''}

" VCS
set statusline+=%#StatuslineSeparator#%{util#in_vc_repo()?'':''}
set statusline+=%#StatuslineNormalBody#%{util#vc_status()}
set statusline+=%#StatuslineSeparator#%{util#in_vc_repo()?'':''}

" Right side items
set statusline+=%=

" Line and Column
set statusline+=%#StatuslineSeparator#
set statusline+=%#StatuslineLineColBody#(%l,%#StatuslineLineColBody#%c)
set statusline+=%#StatuslineSeparator#\ 

" Current scroll percentage and total lines of the file
set statusline+=%#StatuslineSeparator#
set statusline+=%#StatuslinePercentageBody#%p%%\ (%L)
set statusline+=%#StatuslineSeparator#\ 

" Filetype
set statusline+=%#StatuslineSeparator#
set statusline+=%#StatuslineFiletypeBody#%{statusline#icon_filetype()}
set statusline+=%#StatuslineSeparator#\ 

" ALE status
" set statusline+=%(%#StatuslineSeparator#%{g:ale_enabled?'':''}%#StatuslineLintChecking#%{statusline#lint_checking()}%#StatuslineLintWarn#%{statusline#lint_warnings()}%#StatuslineLintError#%{g:statusline_ale_warnings?'\ ':''}%{statusline#lint_errors()}%#StatuslineLintOk#%{statusline#lint_ok()}%#StatuslineSeparator#%{g:ale_enabled?'':''}%{statusline#linted()\|\|statusline#lint_checking()!=#''?'\ ':''}%)
"
" coc
set statusline+=%(%#StatuslineSeparator#%#StatuslineNormalBody#%{coc#status()}%#StatuslineSeparator#%)

" Setup the colors
function! s:setup_colors() abort
  hi StatusLine          guifg=#d485ad     guibg=NONE     gui=NONE
  hi StatusLineNC        guifg=#d75f5f     guibg=NONE     gui=bold

  hi StatuslineSeparator guifg=#3a3a3a gui=none guibg=none

  hi StatuslineFiletypeBody guifg=#d485ad gui=none guibg=#3a3a3a

  hi StatuslinePercentageBody guibg=#3a3a3a gui=none guifg=#dab997

  hi StatuslineNormalBody guibg=#3a3a3a gui=none guifg=#e9e9e9

  hi StatuslineLintWarn guibg=#3a3a3a gui=none guifg=#ffcf00
  hi StatuslineLintChecking guibg=#3a3a3a gui=none guifg=#458588
  hi StatuslineLintError guibg=#3a3a3a gui=none guifg=#d75f5f
  hi StatuslineLintOk guibg=#3a3a3a gui=none guifg=#b8bb26

  hi StatuslineLineCol guifg=#3a3a3a gui=NONE guibg=#3a3a3a
  hi StatuslineLineColBody guibg=#3a3a3a gui=none guifg=#979797
endfunction

augroup statusline_colors
  au!
  au ColorScheme * call s:setup_colors()
augroup END

call s:setup_colors()

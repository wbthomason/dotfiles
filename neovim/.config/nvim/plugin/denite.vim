call denite#custom#var('file/rec', 'command', ['rg', '--files', '--glob', '!.git'])
call denite#custom#source('file/rec', 'matchers', ['matcher/cpsm'])
call denite#custom#source('file_mru', 'matchers', ['matcher/fuzzy', 'matcher/project_files'])
call denite#custom#var('grep', 'command', ['rg'])
call denite#custom#var('grep', 'default_opts', ['-i', '--vimgrep', '--no-heading'])
call denite#custom#var('grep', 'recursive_opts', [])
call denite#custom#var('grep', 'pattern_opt', ['--regexp'])
call denite#custom#var('grep', 'separator', ['--'])
call denite#custom#var('grep', 'final_opts', [])
call denite#custom#alias('source', 'file/rec/git', 'file/rec')
call denite#custom#var('file/rec/git', 'command', ['git', 'ls-files', '-co', '--exclude-standard'])
call denite#custom#option('_', 'split', 'floating')
call denite#custom#option('_', 'prompt', '⟫')
call denite#custom#option('_', 'start_filter', v:true)

augroup denite_aucmds
  au!
  au FileType denite call s:denite_settings()
  au FileType denite-filter call s:denite_filter_settings()
augroup END

function! s:denite_settings() abort
  nnoremap <silent><buffer><expr> <cr> denite#do_map('do_action')
  nnoremap <silent><buffer><expr> d denite#do_map('do_action', 'delete')
  nnoremap <silent><buffer><expr> p denite#do_map('do_action', 'preview')
  nnoremap <silent><buffer><expr> q denite#do_map('quit')
  nnoremap <silent><buffer><expr> i denite#do_map('open_filter_buffer')
  nnoremap <silent><buffer><expr> <Space> denite#do_map('toggle_select').'j'
  nnoremap <silent><buffer><expr> <esc> denite#do_map('quit')
endfunction

function s:denite_filter_settings() abort
  imap <silent><buffer> <c-c> <Plug>(denite_filter_quit)
  imap <silent><buffer><expr> <esc> denite#do_map('quit')
  imap <silent><buffer><expr> <cr> denite#do_map('do_action')
endfunction

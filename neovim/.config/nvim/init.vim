scriptencoding utf-8
" Config file path
let g:config_path = '~/.config/nvim/'

" Function for sourcing config modules
function! ConfigInc(module)
  execute 'source ' . fnameescape(g:config_path) . fnameescape(a:module)
endfunction

" Set Python host program to speed up loading
" let g:loaded_python_provider = 1
" let g:python_host_skip_check = 1
" let g:python3_host_skip_check = 1
" let g:loaded_python3_provider = 1
let g:python_host_prog = '/usr/bin/python2'
let g:python3_host_prog = '/usr/bin/python'

let g:loaded_2html_plugin      = 1
let g:loaded_logiPat           = 1
let g:loaded_getscriptPlugin   = 1
let g:loaded_gzip              = 1
let g:loaded_man               = 1
let g:loaded_matchit           = 1
let g:loaded_matchparen        = 1
let g:loaded_netrwFileHandlers = 1
let g:loaded_netrwPlugin       = 1
let g:loaded_netrwSettings     = 1
let g:loaded_rrhelper          = 1
let g:loaded_shada_plugin      = 1
let g:loaded_spellfile_plugin  = 1
let g:loaded_tarPlugin         = 1
let g:loaded_tutor_mode_plugin = 1
let g:loaded_vimballPlugin     = 1
let g:loaded_zipPlugin = 1

" Functions
call ConfigInc('functions.vim')

" Autocommands
call ConfigInc('autocmds.vim')

" Polyglot
let g:polyglot_disabled = ['latex', 'tex', 'markdown', 'pandoc']

" Haskell
let g:haskell_enable_quantification = 1   " to enable highlighting of `forall`
let g:haskell_enable_recursivedo = 1      " to enable highlighting of `mdo` and `rec`
let g:haskell_enable_arrowsyntax = 1      " to enable highlighting of `proc`
let g:haskell_enable_pattern_synonyms = 1 " to enable highlighting of `pattern`
let g:haskell_enable_typeroles = 1        " to enable highlighting of type roles
let g:haskell_enable_static_pointers = 1  " to enable highlighting of `static`
let g:haskell_backpack = 1                " to enable highlighting of backpack keywords
let g:haskellmode_completion_ghc = 0
" let g:necoghc_enable_detailed_browse = 1
" let g:necoghc_use_stack = 1

" Intero
let g:intero_type_on_hover = 1

" Bufferline
" let g:bufferline_echo = 0
" let g:bufferline_show_bufnr = 0
" let g:bufferline_rotate = 2
" let g:bufferline_pathshorten = 1

" IndentLine
" let g:indentLine_color_term = 239
" let g:indentLine_color_gui = '#09AA08'
let g:indentLine_char = '│'
let g:indentLine_faster = 1

" Pandoc
let g:pandoc#syntax#conceal#use = 1
let g:pandoc#after#modules#enabled = []
let g:pandoc#formatting#mode = 'haA'
let g:pandoc#formatting#textwidth = 100
let g:pandoc#modules#disabled = [ 'commands', 'templates', 'formatting']
let g:pandoc#completion#bib#use_preview = 0
let g:pandoc#biblio#use_bibtool = 1
let g:pandoc#completion#bib#mode = 'citeproc'

" Org-mode
let g:org_aggressive_conceal = 1
let g:org_heading_shade_leading_stars = 1
let g:org_indent = 1

" Vimtex
let g:tex_flavor = 'latex'
let g:tex_conceal = ''
let g:vimtex_compiler_progname = 'nvr'
let g:vimtex_complete_recursive_bib = 1
let g:vimtex_complete_enabled = 1
let g:vimtex_quickfix_method = 'pplatex'
let g:vimtex_quickfix_mode = 0
let g:matchup_override_vimtex = 1

" Undotree
" if has('persistent_undo')
"   set undodir=~/.undodir/
"   set undofile
" endif

" Tagbar
let g:tagbar_type_haskell = {
      \ 'ctagsbin'  : 'hasktags',
      \ 'ctagsargs' : '-x -c -o-',
      \ 'kinds'     : [
      \  'm:modules:0:1',
      \  'd:data: 0:1',
      \  'd_gadt: data gadt:0:1',
      \  't:type names:0:1',
      \  'nt:new types:0:1',
      \  'c:classes:0:1',
      \  'cons:constructors:1:1',
      \  'c_gadt:constructor gadt:1:1',
      \  'c_a:constructor accessors:1:1',
      \  'ft:function types:1:1',
      \  'fi:function implementations:0:1',
      \  'o:others:0:1'
      \ ],
      \ 'sro'        : '.',
      \ 'kind2scope' : {
      \ 'm' : 'module',
      \ 'c' : 'class',
      \ 'd' : 'data',
      \ 't' : 'type'
      \ },
      \ 'scope2kind' : {
      \ 'module' : 'm',
      \ 'class'  : 'c',
      \ 'data'   : 'd',
      \ 'type'   : 't'
      \ }
      \ }

let g:tagbar_type_rust = {
      \ 'ctagstype' : 'rust',
      \ 'kinds' : [
      \'T:types,type definitions',
      \'f:functions,function definitions',
      \'g:enum,enumeration names',
      \'s:structure names',
      \'m:modules,module names',
      \'c:consts,static constants',
      \'t:traits,traits',
      \'i:impls,trait implementations',
      \]
      \}

let g:tagbar_type_elixir = {
      \ 'ctagstype' : 'elixir',
      \ 'kinds' : [
      \ 'f:functions',
      \ 'functions:functions',
      \ 'c:callbacks',
      \ 'd:delegates',
      \ 'e:exceptions',
      \ 'i:implementations',
      \ 'a:macros',
      \ 'o:operators',
      \ 'm:modules',
      \ 'p:protocols',
      \ 'r:records'
      \ ]
      \ }

" Signify
let g:signify_vcs_list = ['git']
let g:signify_sign_change = '＊'
let g:signify_sign_add = '＋'
let g:signify_sign_delete = '～'
let g:signify_sign_delete_first_line = g:signify_sign_delete
let g:signify_sign_show_count = 0

" Goyo
let g:goyo_width = 110

" Startify
let g:startify_list_order = [
      \ ['Update'], 'commands',
      \ ['Recent Files in Directory'], 'dir',
      \ ['Recent Files'], 'files',
      \ ['Bookmarks'], 'bookmarks',
      \ ['Sessions'], 'sessions']
let g:startify_commands = [
      \ {'u': ['Update plugins', ':PlugUpdate']},
      \ {'g': ['Upgrade Plug', ':PlugUpgrade']},
      \ {'c': ['Clean plugins', ':PlugClean']},
      \ {'t': ['Time startup', ':StartupTime']},
      \ {'s': ['Start Prosession', ':Prosession .']}]
let g:startify_session_persistence = 1
let g:startify_change_to_vcs_root = 1
let g:startify_custom_header = []

" Vim-rooter
" Add language-builder patterns before source control for nested projects
let g:rooter_patterns = ['.catkin_workspace', 'Cargo.toml', 'stack.yaml', '*.cabal', 'Makefile', '.git', '.git/', '_darcs/', '.hg/', '.bzr/', '.svn/']
let g:rooter_resolve_links = 1
let g:rooter_silent_chdir = 1
let g:rooter_manual_only = 1
let g:rooter_change_directory_for_non_project_files = 'current'

" Prosession
let g:prosession_tmux_title = 1
let g:prosession_on_startup = 0

" Neoformat
let g:neoformat_python_yapf = {
      \ 'exe': 'yapf',
      \ 'stdin': 1,
      \ 'args': ["--style='{based_on_style: chromium, indent_width: 2, column_limit: 100}'"]
      \}
let g:neoformat_ocaml_ocamlformat = {
      \ 'exe': 'ocamlformat',
      \ 'args': ['--inplace', '-m 100'],
      \ 'replace': 1,
      \ }
let g:neoformat_cpp_clangformat = {
      \ 'exe': 'clang-format',
      \ 'stdin': 1,
      \ 'args': ['--style=file', '--assume-filename=code.cc'],
      \ }

let g:neoformat_basic_format_trim = 1

" IncSearch
let g:incsearch#auto_nohlsearch = 1

" Clever-F
let g:clever_f_fix_key_direction = 1

" Echodoc
let g:echodoc#enable_at_startup = 1

" Vaffle
let g:vaffle_show_hidden_files = 1
let g:vaffle_force_delete = 1

" " FZF
" let g:fzf_layout = { 'window': '10split enew' }
let g:fzf_gitignore_no_maps = 1
let g:fzf_colors =
      \ { 'fg':      ['fg', 'Normal'],
      \ 'bg':      ['bg', 'Normal'],
      \ 'hl':      ['fg', 'Comment'],
      \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
      \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
      \ 'hl+':     ['fg', 'Statement'],
      \ 'info':    ['fg', 'PreProc'],
      \ 'border':  ['fg', 'Ignore'],
      \ 'prompt':  ['fg', 'Conditional'],
      \ 'pointer': ['fg', 'Exception'],
      \ 'marker':  ['fg', 'Keyword'],
      \ 'spinner': ['fg', 'Label'],
      \ 'header':  ['fg', 'Comment'] }

" function! Fzf_dev()
"   let l:fzf_files_options = '--preview "rougify {2..-1} | head -'.&lines.'"'
"
"   function! s:files()
"     let l:files = split(system($FZF_DEFAULT_COMMAND), '\n')
"     return s:prepend_icon(l:files)
"   endfunction
"
"   function! s:prepend_icon(candidates)
"     let l:result = []
"     for l:candidate in a:candidates
"       let l:filename = fnamemodify(l:candidate, ':p:t')
"       let l:icon = WebDevIconsGetFileTypeSymbol(l:filename, isdirectory(l:filename))
"       call add(l:result, printf('%s %s', l:icon, l:candidate))
"     endfor
"
"     return l:result
"   endfunction
"
"   function! s:edit_file(item)
"     let l:pos = stridx(a:item, ' ')
"     let l:file_path = a:item[pos+1:-1]
"     execute 'silent e' l:file_path
"   endfunction
"
"   call fzf#run({
"         \ 'source': <sid>files(),
"         \ 'sink':   function('s:edit_file'),
"         \ 'options': '-m ' . l:fzf_files_options,
"         \ 'down':    '40%' })
" endfunction

" Projectile
" let g:projectile#enable_devicons = 1
" let g:projectile#search_prog = 'rg'

" Snippets
let g:UltiSnipsExpandTrigger       = '<Plug>(ultisnips_expand)'
let g:UltiSnipsJumpForwardTrigger  = '<tab>'
let g:UltiSnipsJumpBackwardTrigger = '<s-tab>'
let g:UltiSnipsRemoveSelectModeMappings = 0

" Vim-pad
let g:pad#dir = '~/wiki/notes'
let g:pad#default_format = 'pandoc'
let g:pad#open_in_split = 0
let g:pad#search_backend = 'rg'
let g:pad#set_mappings = 0

" Vlime
let g:vlime_cl_use_terminal = v:true
let g:vlime_force_default_keys = v:true

" Airline
let g:airline_theme = 'minimalist'
let g:airline_powerline_fonts = 1
let g:airline_highlighting_cache = 1
let g:airline#extensions#hunks#non_zero_only = 1
" let g:airline#extensions#bufferline#overwrite_variables = 1
let g:airline_extensions = ['vimagit', 'vimtex', 'hunks', 'branch']
let g:airline#extensions#default#layout = [
    \ [ 'a', 'b', 'c' ],
    \ [ 'x', 'z', 'error', 'warning' ]
    \ ]

" Sneak
let g:sneak#s_next = 1
let g:sneak#label = 1

" Commands
call ConfigInc('commands.vim')

" Keybindings
call ConfigInc('keybindings.vim')

" Add in plugins
call ConfigInc('plugins.vim')
"
" Colorscheme
set termguicolors
set background=dark
let &t_Cs = "\e[4:3m"
let &t_Ce = "\e[4:0m"
colorscheme nazgul
hi! link extTodo Todo

" set background=light
" colorscheme solarized8_flat

" ALE
let g:ale_sign_error = '🗙'
let g:ale_sign_warning = '➤'
let g:ale_lint_on_save = 1
let g:ale_lint_on_text_changed = 'never'
let g:ale_lint_on_insert_leave = 1
let g:ale_lint_on_enter = 1
let g:ale_echo_msg_error_str = 'E'
let g:ale_echo_msg_warning_str = 'W'
let g:ale_echo_msg_format = '[%linter%] %s [%severity%]'
let g:ale_linters = {
      \ 'haskell': ['hdevtools', 'hlint'],
      \ 'cpp': ['ccls', 'clangcheck', 'clangtidy', 'cppcheck', 'cpplint', 'flawfinder']
      \}
let g:ale_warn_about_trailing_whitespace = 1
let g:ale_set_highlights = 1
let g:ale_cpp_cpplint_options = '--linelength=100'
let g:ale_cpp_clang_options = '-std=c++17 -Wall'
let g:ale_linter_aliases = {'pandoc': ['markdown']}
let g:ale_cpp_clangtidy_checks = ['*', '-fuchsia-default-arguments']
let g:ale_max_signs = -1
let g:ale_set_signs = 1
let g:ale_set_balloons = 1
let g:ale_sign_column_always = 1

" LanguageClient
let g:LanguageClient_hasSnippetSupport = 1
let g:LanguageClient_serverCommands = {
      \ 'rust': ['rls'],
      \ 'python': ['pyls'],
      \ 'javascript': ['javascript-typescript-stdio'],
      \ 'typescript': ['javascript-typescript-stdio'],
      \ 'ocaml': ['ocaml-language-server', '--stdio'],
      \ 'go': ['~/go/bin/go-langserver'],
      \ 'haskell': ['hie-wrapper'],
      \ 'lua': ['lua-lsp'],
      \ 'cpp': ['ccls', '--log-file=/tmp/cc.log'],
      \ 'c': ['ccls', '--log-file=/tmp/cc.log'],
      \ 'lisp': ['cl-lsp']
      \ }
let g:LanguageClient_autoStart = 1
let g:LanguageClient_loadSettings = 1
let g:LanguageClient_diagnosticsDisplay = {
      \ 1: {
      \     'name': 'Error',
      \     'texthl': 'ALEError',
      \     'signText': '🗙',
      \     'signTexthl': 'ALEErrorSign'
      \ },
      \ 2: {
      \     'name': 'Warning',
      \     'texthl': 'ALEWarning',
      \     'signText': '➤',
      \     'signTexthl': 'ALEWarningSign'
      \ },
      \ 3: {
      \     'name': 'Information',
      \     'texthl': 'LanguageClientInformation',
      \     'signText': 'i',
      \     'signTexthl': 'SignInformation'
      \ },
      \ 4: {
      \     'name': 'Hint',
      \     'texthl': 'LanguageClientHint',
      \     'signText': '.',
      \     'signTexthl': 'SignHint'
      \ }
      \ }

let g:LanguageClient_documentHighlightDisplay = {
      \  1: {
      \      'name': 'Text',
      \      'texthl': 'BlueSign',
      \  },
      \  2: {
      \      'name': 'Read',
      \      'texthl': 'AquaSign',
      \  },
      \  3: {
      \      'name': 'Write',
      \      'texthl': 'GreenSign',
      \  },
      \ }

let g:LanguageClient_signColumnAlwaysOn = 1
let g:LanguageClient_diagnosticsEnable = 0
let g:LanguageClient_diagnosticsList = 'Location'
let g:LanguageClient_serverRunning = v:false
let g:LanguageClient_hoverPreview = 'Never'
let g:LanguageClient_completionPreferTextEdit = 1

" Denite
" let g:denite_source_session_path = '~/.vim/session'
" call denite#custom#option('_', {
"       \ 'prompt': '❯',
"       \ 'updatetime': 1,
"       \ 'reversed': 1,
"       \ })
"
" " call denite#custom#source('_', 'sorters', ['sorter/sublime'])
"
" call denite#custom#var('file/rec', 'command',
"       \ ['rg', '--files', '--glob', '!.git'])
" " call denite#custom#var('file/rec', 'command',
" " 	\ ['ag', '--follow', '--nocolor', '--nogroup', '-g', ''])
" call denite#custom#source('file/rec', 'matchers', ['matcher/cpsm'])
"
" call denite#custom#source('file/old', 'matchers', ['matcher/hide_hidden_files', 'matcher/cpsm'])
"
" call denite#custom#var('grep', 'command', ['rg'])
" call denite#custom#var('grep', 'default/opts',
"       \ ['--vimgrep', '--no-heading'])
" call denite#custom#var('grep', 'recursive/opts', [])
" call denite#custom#var('grep', 'pattern/opt', ['--regexp'])
" call denite#custom#var('grep', 'separator', ['--'])
" call denite#custom#var('grep', 'final/opts', [])
"
" call denite#custom#alias('source', 'file/rec/git', 'file/rec')
" call denite#custom#var('file/rec/git', 'command', ['git', 'ls-files', '-co', '--exclude-standard'])
" call denite#custom#source('file/rec/git', 'matchers', ['matcher/cpsm'])
"
" call denite#custom#map(
"       \ 'insert',
"       \ '<Down>',
"       \ '<denite:move_to_next_line>',
"       \ 'noremap'
"       \)
"
" call denite#custom#map(
"       \ 'insert',
"       \ '<Up>',
"       \ '<denite:move_to_previous_line>',
"       \ 'noremap'
"       \)

" Leader guide

call which_key#register('<Space>', 'g:lmaps')
call which_key#register(',', 'g:llmaps')
" call leaderGuide#register_prefix_descriptions('', 'g:allmaps')
" call leaderGuide#register_prefix_descriptions(',', 'g:llmaps')

" ALE integration config
call airline#parts#define_function('ALE', 'LinterStatus')
let g:airline_section_error = airline#section#create_right(['ALE'])

" NCM2
let g:ncm2_pyclang#library_path = '/usr/lib/libclang.so'
let g:ncm2#popup_delay = 100
inoremap <expr><Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr><S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
inoremap <c-u> <Plug>(ultisnips_expand)
inoremap <silent> <expr> <c-u> ncm2_ultisnips#expand_or("\<CR>", 'n')
call ncm2#override_source('tagprefix', {'priority': 2})
call ncm2#override_source('pyclang', {'priority': 8})


" General settings
call ConfigInc('settings.vim')

if &background ==# 'dark'
  " let g:rainbow_conf.guifgs = ['#eeeeee', '#c6c6c6', '#aaaaaa', '#888888']
  let g:rainbow_conf = {
	\	'guifgs': ['#ff1111', '#ff3030', '#ff6a6a', '#cd5555', '#8b3a3a'],
	\	'operators': '_,_',
	\	'parentheses': ['start=/(/ end=/)/ fold', 'start=/\[/ end=/\]/ fold', 'start=/{/ end=/}/ fold'],
	\	'separately': {
	\		'*': {},
    \		'ocaml': {
    \			'parentheses': ['start=/(\*\@!/ end=/)/ fold contains=@colorableGroup'],
    \		},
	\		'tex': {
	\			'parentheses': ['start=/(/ end=/)/', 'start=/\[/ end=/\]/'],
	\		},
	\		'vim': {
	\			'parentheses': ['start=/(/ end=/)/', 'start=/\[/ end=/\]/', 'start=/{/ end=/}/ fold', 'start=/(/ end=/)/ containedin=vimFuncBody', 'start=/\[/ end=/\]/ containedin=vimFuncBody', 'start=/{/ end=/}/ fold containedin=vimFuncBody'],
	\		},
	\		'html': {
	\			'parentheses': ['start=/\v\<((area|base|br|col|embed|hr|img|input|keygen|link|menuitem|meta|param|source|track|wbr)[ >])@!\z([-_:a-zA-Z0-9]+)(\s+[-_:a-zA-Z0-9]+(\=("[^"]*"|'."'".'[^'."'".']*'."'".'|[^ '."'".'"><=`]*))?)*\>/ end=#</\z1># fold'],
	\		},
	\		'css': 0,
	\	}
	\}
endif

let g:capture_templates = {
      \ 'journal': {'file': '~/wiki/journal/journal.md',
      \             'pattern': '^# ',
      \             'new_snip': '# `!v strftime("%A, %F")`## `!v strftime("%R")` - ${1:title}${0:entry}',
      \             'extend_pattern': '.\+$',
      \             'extend_snip': '## `!v strftime("%R")` - ${1:title}${0:entry}'},
      \ 'research': {'file': '~/wiki/research.md',
      \             'pattern': '^# ',
      \             'new_snip': '# `!v strftime("%A, %F")`## `!v strftime("%R")` - ${1:title}${0:entry}',
      \             'extend_pattern': '.\+$',
      \             'extend_snip': '## `!v strftime("%R")` - ${1:title}${0:entry}'},
      \ 'advisor': {'file': '~/wiki/ross_meetings.md',
      \             'pattern': '^# ',
      \             'new_snip': '# `!v strftime("%A, %F")`${0:meeting_notes}'},
      \}

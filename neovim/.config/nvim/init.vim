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
let g:polyglot_disabled = ['latex', 'tex', 'markdown', 'pandoc', 'org']

" Golden Ratio
" let g:golden_ratio_exclude_nonmodifiable = 1

" Vimagit
let g:magit_default_fold_level = 0
let g:magit_default_show_all_files = 0
let g:magit_auto_foldopen = 0

" Intero
let g:intero_type_on_hover = 1

" IndentLine
let g:indentLine_char = '│'
let g:indentLine_faster = 1

" Matchup
let g:matchup_matchparen_deferred = 1
let g:matchup_matchparen_hi_surround_always = 1

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

" Vim-qf
let g:qf_auto_open_quickfix = 0

" Rust
let g:rust_recommended_style = 0

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
" let g:goyo_width = 110

" Startify
let g:startify_list_order = [
      \ ['Update'], 'commands',
      \ ['Recent Files in Directory'], 'dir',
      \ ['Recent Files'], 'files',
      \ ['Bookmarks'], 'bookmarks',
      \ ['Sessions'], 'sessions']
let g:startify_commands = [
      \ {'u': ['Update plugins', ':PackUpdate']},
      \ {'c': ['Clean plugins', ':PackClean']},
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

" Clever-F
let g:clever_f_fix_key_direction = 1

" Echodoc
let g:echodoc#enable_at_startup = 1
let g:echodoc#type = 'signature'

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

" Vim-pad
let g:pad#dir = '~/wiki/notes'
let g:pad#default_format = 'org'
let g:pad#open_in_split = 0
let g:pad#search_backend = 'rg'
let g:pad#set_mappings = 0

" Vlime
let g:vlime_cl_use_terminal = v:true
let g:vlime_force_default_keys = v:true

" Lightline
let g:lightline = {}
let g:lightline.colorscheme = 'Tomorrow_Night_Eighties'
let g:lightline.enable = {'tabline': 0}
let g:lightline.active = {
      \ 'left': [ [ 'mode', 'paste' ],
      \           [ 'vcstatus', 'filename', 'modified', 'readonly' ] ],
      \ 'right': [ [ 'cocstatus' ],
      \            [ 'linter_checking', 'linter_errors', 'linter_warnings', 'linter_ok' ],
      \            [ 'gutentags'],
      \            [ 'percent' ],
      \            [ 'filetype' ] ]}

let g:lightline.component_function = {
      \ 'vcstatus': 'VCStatus',
      \ 'filetype': 'IconFileType',
      \ 'cocstatus': 'coc#status'}

let g:lightline.component = {
      \ 'lineinfo': ' %3l:%-2v',
      \ 'readonly': "%{&readonly ? '' : ''}"}

let g:lightline.component_expand = {
      \  'linter_checking': 'lightline#ale#checking',
      \  'linter_warnings': 'lightline#ale#warnings',
      \  'linter_errors': 'lightline#ale#errors',
      \  'linter_ok': 'lightline#ale#ok',
      \  'gutentags': 'gutentags#statusline',
      \ }

let g:lightline.component_type = {
      \     'linter_checking': 'left',
      \     'linter_warnings': 'warning',
      \     'linter_errors': 'error',
      \     'linter_ok': 'left',
      \ }

let g:lightline#ale#indicator_checking = "\uf110"
let g:lightline#ale#indicator_warnings = "\uf071"
let g:lightline#ale#indicator_errors = "\uf05e"
let g:lightline#ale#indicator_ok = "\uf00c"

" Sneak
let g:sneak#s_next = 1
let g:sneak#label = 1

" Commands
call ConfigInc('commands.vim')

" Keybindings
call ConfigInc('keybindings.vim')

" Add in plugins
call ConfigInc('plugins.vim')

" Colorscheme
set termguicolors
set background=dark
" colorscheme base16-gruvbox-dark-soft
colorscheme nazgul

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
      \ 'cpp': ['clangtidy', 'cppcheck', 'cpplint', 'flawfinder'],
      \ 'rust': ['rustfmt']
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
let g:ale_virtualtext_cursor = 1

" LanguageClient
" let g:LanguageClient_hasSnippetSupport = 1
" let g:LanguageClient_serverCommands = {
"       \ 'rust': ['rls'],
"       \ 'python': ['pyls'],
"       \ 'javascript': ['javascript-typescript-stdio'],
"       \ 'typescript': ['javascript-typescript-stdio'],
"       \ 'ocaml': ['ocaml-language-server', '--stdio'],
"       \ 'go': ['~/go/bin/go-langserver'],
"       \ 'haskell': ['hie-wrapper'],
"       \ 'lua': ['java', '-cp', '~/projects/EmmyLua-LanguageServer/EmmyLua-LS/build/libs/EmmyLua-LS.jar', 'com.tang.vscode.MainKt'],
"       \ 'cpp': ['ccls', '--log-file=/tmp/cc.log'],
"       \ 'c': ['ccls', '--log-file=/tmp/cc.log'],
"       \ }
" let g:LanguageClient_autoStart = 1
" let g:LanguageClient_loadSettings = 1
" let g:LanguageClient_settingsPath = '/home/wil/.config/nvim/settings.json'
" let g:LanguageClient_diagnosticsDisplay = {
"       \ 1: {
"       \     'name': 'Error',
"       \     'texthl': 'ALEError',
"       \     'signText': '🗙',
"       \     'signTexthl': 'ALEErrorSign'
"       \ },
"       \ 2: {
"       \     'name': 'Warning',
"       \     'texthl': 'ALEWarning',
"       \     'signText': '➤',
"       \     'signTexthl': 'ALEWarningSign'
"       \ },
"       \ 3: {
"       \     'name': 'Information',
"       \     'texthl': 'LanguageClientInformation',
"       \     'signText': 'i',
"       \     'signTexthl': 'SignInformation'
"       \ },
"       \ 4: {
"       \     'name': 'Hint',
"       \     'texthl': 'LanguageClientHint',
"       \     'signText': '.',
"       \     'signTexthl': 'SignHint'
"       \ }
"       \ }
"
" let g:LanguageClient_documentHighlightDisplay = {
"       \  1: {
"       \      'name': 'Text',
"       \      'texthl': 'BlueHover',
"       \  },
"       \  2: {
"       \      'name': 'Read',
"       \      'texthl': 'AquaHover',
"       \  },
"       \  3: {
"       \      'name': 'Write',
"       \      'texthl': 'GreenHover',
"       \  },
"       \ }
"
" let g:LanguageClient_signColumnAlwaysOn = 1
" let g:LanguageClient_diagnosticsEnable = 1
" let g:LanguageClient_diagnosticsList = 'Location'
" let g:LanguageClient_hoverPreview = 'Never'
" let g:LanguageClient_completionPreferTextEdit = 1

" Which-key
packadd vim-which-key
call which_key#register('<Space>', 'g:lmaps')
call which_key#register(',', 'g:llmaps')

" Coc
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> for trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()

" Use <cr> for confirm completion, `<C-g>u` means break undo chain at current position.
" Coc only does snippet and additional edit on confirm.
inoremap <silent> <cr> <C-R>=Handle_cr_coc()<cr>

function! Handle_cr_coc() abort
  return pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
endfunction

inoremap <expr> <TAB> pumvisible() ? "\<C-y>" : "\<TAB>"
let g:coc_snippet_next = '<TAB>'
let g:coc_snippet_prev = '<S-TAB>'
imap <C-k>     <Plug>(neosnippet_expand_or_jump)
smap <C-k>     <Plug>(neosnippet_expand_or_jump)
xmap <C-k>     <Plug>(neosnippet_expand_target)
let g:coc_status_error_sign = ' 🗙 '
let g:coc_status_warning_sign = ' ⚠ '

" General settings
call ConfigInc('settings.vim')

let g:rainbow_active = 1
let g:rainbow_conf = {
      \ 'guifgs': ['#bdae93', '#d5c4a1', '#ebdbb2', '#fbf1c7', '#fb4934', '#fe8019', '#fabd2f', '#b8bb26', '#8ec07c', '#83a598', '#d3869b', '#d65d0e'],
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

" Gutentags
let g:gutentags_ctags_exclude = ['.ccls-cache']
let g:gutentags_file_list_command = 'rg --files'

hi RedHover guifg=#cc241d ctermfg=124 gui=NONE cterm=NONE
hi YellowHover guifg=#fabd2f ctermfg=214 gui=NONE cterm=NONE
hi OrangeHover guifg=#fd7d2f ctermfg=214 gui=NONE cterm=NONE
hi GreenHover guifg=#b8cc26 ctermfg=142 gui=NONE cterm=NONE
hi BlueHover guifg=#83a5cb ctermfg=109 gui=NONE cterm=NONE
hi AquaHover guifg=#8ec07c ctermfg=108 gui=NONE cterm=NONE
hi WhiteHover guifg=#ffffff ctermfg=108 gui=NONE cterm=NONE

hi RedSign guifg=#cc241d ctermfg=124 gui=NONE cterm=NONE
hi YellowSign guifg=#fabd2f ctermfg=214 gui=NONE cterm=NONE
hi GreenSign guifg=#b8cc26 ctermfg=142 gui=NONE cterm=NONE
hi BlueSign guifg=#83a5cb ctermfg=109 gui=NONE cterm=NONE
hi AquaSign guifg=#8ec07c ctermfg=108 gui=NONE cterm=NONE

hi! link ALEErrorSign RedSign
hi! link ALEWarningSign YellowSign
hi! link ALEInfoSign BlueSign

hi! link ALEVirtualTextError RedHover
hi! link ALEVirtualTextWarning YellowHover
hi! link ALEVirtualTextInfo WhiteHover
hi! link ALEVirtualTextStyleError OrangeHover
hi! link ALEVirtualTextStyleWarning BlueHover

hi! link CocHighlightText AquaHover
hi! link CocHighlightRead BlueHover
hi! link CocHighlightWrite GreenHover
hi! link CocErrorSign ALEErrorSign
hi! link CocWarningSign ALEWarningSign
hi! link CocInfoSign ALEInfoSign

hi default link CocErrorHighlight ALEError
hi default link CocWarningHighlight ALEWarning

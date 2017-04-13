" Use full Vim features
if &compatible
  set nocompatible               " Be iMproved
endif

" Config file path
let g:config_path = "~/.config/nvim/"

" Function for sourcing config modules
function! ConfigInc(module)
  execute 'source ' . fnameescape(g:config_path) . fnameescape(a:module)
endfunction

" Add in plugins
call ConfigInc('plugins.vim')

" Functions
call ConfigInc('functions.vim')

" Autocommands
" Main Autocommands
call ConfigInc('autocmds.vim')

" Keybindings
call ConfigInc('keybindings.vim')

" Haskell settings
let g:haskell_enable_quantification = 1   " to enable highlighting of `forall`
let g:haskell_enable_recursivedo = 1      " to enable highlighting of `mdo` and `rec`
let g:haskell_enable_arrowsyntax = 1      " to enable highlighting of `proc`
let g:haskell_enable_pattern_synonyms = 1 " to enable highlighting of `pattern`
let g:haskell_enable_typeroles = 1        " to enable highlighting of type roles
let g:haskell_enable_static_pointers = 1  " to enable highlighting of `static`
let g:haskell_backpack = 1                " to enable highlighting of backpack keywords

" Eclim settings
let g:EclimCompletionMethod = 'omnifunc'

" Rainbow parens settings
let g:rainbow_active = 1

" Airline settings
let g:airline_powerline_fonts = 1
" Enable the list of buffers
let g:airline#extensions#tabline#enabled = 1
" Show just the filename
let g:airline#extensions#tabline#fnamemod = ':t'

" Dein settings
let g:dein#enable_notification = 1
let g:dein#install_message_type = 'echo'

" Opam/OCaml settings
let g:opamshare = substitute(system('opam config var share'),'\n$','','''')
execute "set rtp+=" . g:opamshare . "/merlin/vim"
execute "helptags " . g:opamshare . "/merlin/vim/doc"
execute "set rtp^=" . g:opamshare . "/ocp-indent/vim"

" Ale settings
let g:ale_sign_error = '✖'
let g:ale_sign_warning = '➤'
let g:ale_statusline_format = ['⨉ %d', '⚠ %d', '⬥ ok']
let g:ale_lint_on_save = 1
let g:ale_lint_on_text_changed = 0
let g:ale_lint_on_enter = 1
let g:ale_echo_msg_error_str = 'E'
let g:ale_echo_msg_warning_str = 'W'
let g:ale_echo_msg_format = '[%linter%] %s [%severity%]'
call airline#parts#define_function('ALE', 'ALEGetStatusLine')
call airline#parts#define_condition('ALE', 'exists("*ALEGetStatusLine")')
let g:airline_section_error = airline#section#create_right(['ALE'])
let g:ale_linters = {'haskell': ['hdevtools', 'hlint']}


augroup ale_colors
  au!
  autocmd ColorScheme *
        \ hi ALEErrorSign guifg=#ff727b |
        \ hi ALEWarningSign guifg=#ebcb8b |
augroup END

" IndentLine settings
let g:indentLine_color_term = 239
let g:indentLine_color_gui = '#09AA08'
let g:indentLine_char = '│'

" Pandoc settings
let g:pandoc#syntax#conceal#use = 1
let g:pandoc#after#modules#enabled = ['ultisnips', 'unite']
let g:pandoc#formatting#mode = 'haA'
let g:pandoc#formatting#textwidth = 100
let g:pandoc#completion#bib#use_preview = 1
let g:pandoc#command#autoexec_on_writes = 0
let g:pandoc#command#autoexec_command = 'make'
let g:pandoc#modules#disabled = ["folding"]

" Vimtex settings
let g:tex_flavor = "latex"

" Ultisnips settings
let g:UltiSnipsExpandTrigger = '<c-j>'
let g:UltiSnipsJumpForwardTrigger = '<c-j>'
let g:UltiSnipsJumpBackwardTrigger = '<c-k>'

" Syntastic settings
let g:syntastic_ocaml_checkers=['merlin','caml4po']

" YouCompleteMe settings
"let g:ycm_semantic_triggers =  {
      "\   'c' : ['->', '.'],
      "\   'objc' : ['->', '.'],
      "\   'ocaml' : ['.', '#'],
      "\   'cpp,objcpp' : ['->', '.', '::'],
      "\   'perl' : ['->'],
      "\   'php' : ['->', '::'],
      "\   'cs,javascript,d,python,perl6,scala,vb,elixir,go' : ['.'],
      "\   'java,jsp' : ['.'],
      "\   'vim' : ['re![_a-zA-Z]+[_\w]*\.'],
      "\   'ruby' : ['.', '::'],
      "\   'lua' : ['.', ':'],
      "\   'erlang' : [':'],
      "\   'tex': [
      "\ 're!\\[A-Za-z]*cite[A-Za-z]*(\[[^]]*\]){0,2}{[^}]*',
      "\ 're!\\[A-Za-z]*ref({[^}]*|range{([^,{}]*(}{)?))',
      "\ 're!\\hyperref\[[^]]*',
      "\ 're!\\includegraphics\*?(\[[^]]*\]){0,2}{[^}]*',
      "\ 're!\\(include(only)?|input){[^}]*',
      "\ 're!\\\a*(gls|Gls|GLS)(pl)?\a*(\s*\[[^]]*\]){0,2}\s*\{[^}]*',
      "\ 're!\\includepdf(\s*\[[^]]*\])?\s*\{[^}]*',
      "\ 're!\\includestandalone(\s*\[[^]]*\])?\s*\{[^}]*',
      "\ ],
      "\  'markdown': ['@'],
      "\  'pandoc': ['@'],
      "\  'haskell': ['.']
      "\ }
"let g:ycm_confirm_extra_conf = 0
"let g:ycm_filetype_blacklist = {
      "\ 'tagbar' : 1,
      "\ 'qf' : 1,
      "\ 'notes' : 1,
      "\ 'unite' : 1,
      "\ 'text' : 1,
      "\ 'infolog' : 1,
      "\ 'mail' : 1
      "\}

" VimFiler settings
let g:vimfiler_as_default_explorer = 1

" Undotree settings
if has("persistent_undo")
  set undodir=~/.undodir/
  set undofile
endif

" Tagbar settings
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

" Racer settings
let g:racer_cmd = "/usr/bin/racer"

" Gitgutter settings
set updatetime=500
let g:gitgutter_sign_modified = '•'
let g:gitgutter_sign_added = '＋'
highlight GitGutterAdd guifg = '#A3E28B'

" Vimwiki settings
let g:vimwiki_ext2syntax = {'.md': 'markdown'}
let g:vimwiki_list = [{'path': '$HOME/journal', 'syntax': 'markdown', 'ext': '.md'}]
let g:vimwiki_global_ext = 0

" Goyo settings
let g:goyo_width = 110

" Colorscheme
let base16colorspace=256
colorscheme base16-tomorrow-night
" Highlighting tweaks
" TODO: There's something wrong with this highlight rule; it only works after the second sourcing of
" a config file...
" Update: Maybe not anymore?
augroup color_tweaks
  autocmd!
  autocmd ColorScheme * highlight Todo cterm=bold ctermfg=0 ctermbg=3 gui=bold guifg=#3B4252 guibg=#EBCB8B | highlight Comment cterm=bold ctermfg=8 ctermbg=NONE gui=bold guifg=#D8DEE9 guibg=NONE | highlight LineNr guifg=#777777
augroup END

" Slime settings
let g:slime_target = "tmux"

" FZF settings
let g:fzf_colors =
      \ { 'fg':      ['fg', 'Normal'],
      \ 'bg':      ['bg', 'Normal'],
      \ 'hl':      ['fg', 'Comment'],
      \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
      \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
      \ 'hl+':     ['fg', 'Statement'],
      \ 'info':    ['fg', 'PreProc'],
      \ 'prompt':  ['fg', 'Conditional'],
      \ 'pointer': ['fg', 'Exception'],
      \ 'marker':  ['fg', 'Keyword'],
      \ 'spinner': ['fg', 'Label'],
      \ 'header':  ['fg', 'Comment'] }

let g:fzf_buffers_jump = 1
let g:fzf_commits_log_options = '--graph --color=always --format="%C(auto)%h%d %s %C(black)%C(bold)%cr"'

command! FZFMru call fzf#run({
      \ 'source':  reverse(s:all_files()),
      \ 'sink':    'edit',
      \ 'options': '-m -x +s',
      \ 'down':    '40%' })

function! s:all_files()
  return extend(
        \ filter(copy(v:oldfiles),
        \        "v:val !~ 'fugitive:\\|NERD_tree\\|^/tmp/\\|.git/'"),
        \ map(filter(range(1, bufnr('$')), 'buflisted(v:val)'), 'bufname(v:val)'))
endfunction

" Denite settings
let s:menus = {}
let s:menus.git = {
      \ 'description' : '            interface to vim-fugitive'
      \}
let s:menus.git.command_candidates = [
      \['▷ tig                                                         gt',
      \'normal ,gt'],
      \['▷ git status       (Fugitive)                                 gs',
      \'Gstatus'],
      \['▷ git diff         (Fugitive)                                 gd',
      \'Gdiff'],
      \['▷ git commit       (Fugitive)                                 gc',
      \'Gcommit'],
      \['▷ git log          (Fugitive)                                 gl',
      \'exe "silent Glog | Denite unite:quickfix"'],
      \['▷ git blame        (Fugitive)                                 gb',
      \'Gblame'],
      \['▷ git stage        (Fugitive)                                 gw',
      \'Gwrite'],
      \['▷ git checkout     (Fugitive)                                 go',
      \'Gread'],
      \['▷ git rm           (Fugitive)                                 gr',
      \'Gremove'],
      \['▷ git mv           (Fugitive)                                 gm',
      \'exe "Gmove " input("destination: ")'],
      \['▷ git push         (Fugitive, buffer output)                   gp',
      \'Git! push'],
      \['▷ git pull         (Fugitive, buffer output)                   gP',
      \'Git! pull'],
      \['▷ git prompt       (Fugitive, buffer output)                   gi',
      \'exe "Git! " input("command git: ")'],
      \['▷ git cd           (Fugitive)',
      \'Gcd'],
      \]
call denite#custom#var('menu', 'menus', s:menus)

" Startify settings
let g:startify_list_order = [
      \ ['Update'], 'commands', 
      \ ['Recent Files in Directory'], 'dir', 
      \ ['Recent Files'], 'files', 
      \ ['Bookmarks'], 'bookmarks', 
      \ ['Sessions'], 'sessions']
let g:startify_commands = [{'u': ['Update plugins', ':PlugUpdate']}, {'g': ['Upgrade Plug.vim', ':PlugUpgrade']}]
let g:startify_session_persistence = 1
let g:startify_change_to_vcs_root = 1
let g:startify_custom_header = []

" Vim-rooter settings
" Add language-builder patterns before source control for nested projects
"let g:rooter_patterns = ['Cargo.toml', 'stack.yaml', '*.cabal', 'Makefile', '.git', '.git/', '_darcs/', '.hg/', '.bzr/', '.svn/']
let g:rooter_resolve_links = 1
let g:rooter_silent_chdir = 1
let g:rooter_manual_only = 1
let g:rooter_change_directory_for_non_project_files = 'current'

" NERDCommenter settings
let g:NERDCompactSexyComs = 1
let g:NERDCommentEmptyLines = 1
let g:NERDTrimTrailingWhitespace = 1

" Deoplete settings
let g:deoplete#enable_at_startup = 1
let g:deoplete#auto_complete_delay = 20
let g:deoplete#omni#input_patterns = {
      \    'ocaml': '[^ ,;\t\[()\]]',
      \}
" YCM Semantic trigger regexes for use with deoplete if necessary
"let g:ycm_semantic_triggers =  {
      "\   'c' : ['->', '.'],
      "\   'objc' : ['->', '.'],
      "\   'ocaml' : ['.', '#'],
      "\   'cpp,objcpp' : ['->', '.', '::'],
      "\   'perl' : ['->'],
      "\   'php' : ['->', '::'],
      "\   'cs,javascript,d,python,perl6,scala,vb,elixir,go' : ['.'],
      "\   'java,jsp' : ['.'],
      "\   'vim' : ['re![_a-zA-Z]+[_\w]*\.'],
      "\   'ruby' : ['.', '::'],
      "\   'lua' : ['.', ':'],
      "\   'erlang' : [':'],
      "\   'tex': [
      "\ 're!\\[A-Za-z]*cite[A-Za-z]*(\[[^]]*\]){0,2}{[^}]*',
      "\ 're!\\[A-Za-z]*ref({[^}]*|range{([^,{}]*(}{)?))',
      "\ 're!\\hyperref\[[^]]*',
      "\ 're!\\includegraphics\*?(\[[^]]*\]){0,2}{[^}]*',
      "\ 're!\\(include(only)?|input){[^}]*',
      "\ 're!\\\a*(gls|Gls|GLS)(pl)?\a*(\s*\[[^]]*\]){0,2}\s*\{[^}]*',
      "\ 're!\\includepdf(\s*\[[^]]*\])?\s*\{[^}]*',
      "\ 're!\\includestandalone(\s*\[[^]]*\])?\s*\{[^}]*',
      "\ ],
      "\  'markdown': ['@'],
      "\  'pandoc': ['@'],
      "\  'haskell': ['.']
      "\ }
"let g:ycm_semantic_triggers =  {
      "\   'c' : ['->', '.'],
      "\   'objc' : ['->', '.'],
      "\   'ocaml' : ['.', '#'],
      "\   'cpp,objcpp' : ['->', '.', '::'],
      "\   'perl' : ['->'],
      "\   'php' : ['->', '::'],
      "\   'cs,javascript,d,python,perl6,scala,vb,elixir,go' : ['.'],
      "\   'java,jsp' : ['.'],
      "\   'vim' : ['re![_a-zA-Z]+[_\w]*\.'],
      "\   'ruby' : ['.', '::'],
      "\   'lua' : ['.', ':'],
      "\   'erlang' : [':'],
      "\   'tex': [
      "\ 're!\\[A-Za-z]*cite[A-Za-z]*(\[[^]]*\]){0,2}{[^}]*',
      "\ 're!\\[A-Za-z]*ref({[^}]*|range{([^,{}]*(}{)?))',
      "\ 're!\\hyperref\[[^]]*',
      "\ 're!\\includegraphics\*?(\[[^]]*\]){0,2}{[^}]*',
      "\ 're!\\(include(only)?|input){[^}]*',
      "\ 're!\\\a*(gls|Gls|GLS)(pl)?\a*(\s*\[[^]]*\]){0,2}\s*\{[^}]*',
      "\ 're!\\includepdf(\s*\[[^]]*\])?\s*\{[^}]*',
      "\ 're!\\includestandalone(\s*\[[^]]*\])?\s*\{[^}]*',
      "\ ],
      "\  'markdown': ['@'],
      "\  'pandoc': ['@'],
      "\  'haskell': ['.']
      "\ }

" BuildIt settings

" General settings
call ConfigInc('settings.vim')

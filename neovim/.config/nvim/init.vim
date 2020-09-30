scriptencoding utf-8

let g:mapleader      = "\<space>"
let g:maplocalleader = ','

let g:loaded_python_provider = 0
let g:python_host_prog       = '/usr/bin/python2'
let g:python3_host_prog      = '/usr/bin/python'
let g:node_host_prog         = '/usr/bin/neovim-node-host'

let g:loaded_2html_plugin      = 1
let loaded_gzip                = 1
let g:loaded_man               = 1
let loaded_matchit             = 1
let loaded_matchparen          = 1
let g:loaded_shada_plugin      = 1
let loaded_spellfile_plugin    = 1
let g:loaded_tarPlugin         = 1
let g:loaded_tutor_mode_plugin = 1
let g:loaded_vimballPlugin     = 1
let g:loaded_zipPlugin         = 1
let g:loaded_netrwPlugin       = 1

" Polyglot requires the following before loading
" Disable unused languages; has a slight (~1ms) performance impact
let g:polyglot_disabled = [
      \ 'acpiasl',
      \ 'ansible',
      \ 'apiblueprint',
      \ 'applescript',
      \ 'arduino',
      \ 'asciidoc',
      \ 'autohotkey',
      \ 'blade',
      \ 'caddyfile',
      \ 'carp',
      \ 'cjsx',
      \ 'clojure',
      \ 'coffee-script',
      \ 'cql',
      \ 'cryptol',
      \ 'crystal',
      \ 'cucumber',
      \ 'cue',
      \ 'dart',
      \ 'dhall',
      \ 'dlang',
      \ 'emberscript',
      \ 'emblem',
      \ 'ferm',
      \ 'fish',
      \ 'flatbuffers',
      \ 'glsl',
      \ 'gmpl',
      \ 'gnuplot',
      \ 'gradle',
      \ 'graphql',
      \ 'groovy-indent',
      \ 'groovy',
      \ 'haml',
      \ 'haproxy',
      \ 'haxe',
      \ 'hcl',
      \ 'helm',
      \ 'hive',
      \ 'ion',
      \ 'jasmine',
      \ 'jenkins',
      \ 'jinja',
      \ 'jst',
      \ 'jsx',
      \ 'latex',
      \ 'lua',
      \ 'tex',
      \ 'less',
      \ 'lilypond',
      \ 'livescript',
      \ 'log',
      \ 'mako',
      \ 'mdx',
      \ 'nginx',
      \ 'objc',
      \ 'opencl',
      \ 'pgsql',
      \ 'plantuml',
      \ 'pony',
      \ 'powershell',
      \ 'protobuf',
      \ 'pug',
      \ 'puppet',
      \ 'purescript',
      \ 'qmake',
      \ 'qml',
      \ 'r-lang',
      \ 'ragel',
      \ 'raml',
      \ 'rspec',
      \ 'rst',
      \ 'sbt',
      \ 'slim',
      \ 'slime',
      \ 'smt2',
      \ 'solidity',
      \ 'stylus',
      \ 'svelte',
      \ 'svg-indent',
      \ 'svg',
      \ 'terraform',
      \ 'textile',
      \ 'thrift',
      \ 'tomdoc',
      \ 'tptp',
      \ 'twig',
      \ 'v',
      \ 'vala',
      \ 'vbnet',
      \ 'vcl',
      \ 'vifm',
      \ 'vm',
      \ 'vue',
      \ 'xdc',
      \ 'xls',
      \ 'yard',
      \ 'zephir',
      \]
packadd vim-polyglot

set termguicolors
set background=dark
colorscheme nazgul

augroup start_screen
  au!
  autocmd VimEnter * ++once lua require('start').start()
augroup END

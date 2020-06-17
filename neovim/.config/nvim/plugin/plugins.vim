execute 'luafile ' . stdpath('config') . '/lua/plugins.lua'
let g:sexp_filetypes = join(['clojure', 'lisp', 'scheme', 'racket', 'jbuild', 'fennel', 'pddl'], ',')
command! PackerInstall packadd packer.nvim | lua require('plugins').install()
command! PackerUpdate packadd packer.nvim | lua require('plugins').update()
command! PackerSync packadd packer.nvim | lua require('plugins').sync()
command! PackerClean packadd packer.nvim | lua require('plugins').clean()
command! PackerCompile packadd packer.nvim | lua require('plugins').compile('~/.config/nvim/plugin/packer_load.vim')

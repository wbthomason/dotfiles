require 'impatient'
local g = vim.g
local cmd = vim.cmd
local o, wo, bo = vim.o, vim.wo, vim.bo
local utils = require 'config.utils'
local opt = utils.opt
local autocmd = utils.autocmd
local map = utils.map

-- Leader/local leader
g.mapleader = [[ ]]
g.maplocalleader = [[,]]

-- Skip some remote provider loading
g.loaded_python_provider = 0
g.python_host_prog = '/usr/bin/python2'
g.python3_host_prog = '/usr/bin/python'
g.node_host_prog = '/usr/bin/neovim-node-host'

-- Disable some built-in plugins we don't want
local disabled_built_ins = {
  'gzip',
  'man',
  'matchit',
  'matchparen',
  'shada_plugin',
  'tarPlugin',
  'tar',
  'zipPlugin',
  'zip',
  'netrwPlugin',
}

for i = 1, 10 do
  g['loaded_' .. disabled_built_ins[i]] = 1
end

-- Settings
local buffer = { o, bo }
local window = { o, wo }
opt('textwidth', 100, buffer)
opt('scrolloff', 7)
opt('wildignore', '*.o,*~,*.pyc')
opt('wildmode', 'longest,full')
opt('whichwrap', vim.o.whichwrap .. '<,>,h,l')
opt('inccommand', 'nosplit')
opt('lazyredraw', true)
opt('showmatch', true)
opt('ignorecase', true)
opt('smartcase', true)
opt('tabstop', 2, buffer)
opt('softtabstop', 0, buffer)
opt('expandtab', true, buffer)
opt('shiftwidth', 2, buffer)
opt('number', true, window)
opt('relativenumber', true, window)
opt('smartindent', true, buffer)
opt('laststatus', 3)
opt('showmode', false)
opt('shada', [['20,<50,s10,h,/100]])
opt('hidden', true)
opt('shortmess', o.shortmess .. 'c')
opt('joinspaces', false)
opt('guicursor', [[n-v-c:block,i-ci-ve:ver25,r-cr:hor20,o:hor50]])
opt('updatetime', 500)
opt('conceallevel', 2, window)
opt('concealcursor', 'nc', window)
opt('previewheight', 5)
opt('undofile', true, buffer)
opt('synmaxcol', 500, buffer)
opt('display', 'msgsep')
opt('cursorline', true, window)
opt('modeline', false, buffer)
opt('mouse', 'nivh')
opt('signcolumn', 'yes:1', window)

-- Colorscheme
opt('termguicolors', true)
opt('background', 'dark')
-- cmd [[colorscheme gruvbox-material]]
cmd [[colorscheme nazgul]]

-- Autocommands
autocmd('start_screen', [[VimEnter * ++once lua require('start').start()]], true)
autocmd(
  'syntax_aucmds',
  [[Syntax * syn match extTodo "\<\(NOTE\|HACK\|BAD\|TODO\):\?" containedin=.*Comment.* | hi! link extTodo Todo]],
  true
)
autocmd('misc_aucmds', {
  [[BufWinEnter * checktime]],
  [[TextYankPost * silent! lua vim.highlight.on_yank()]],
  [[FileType qf set nobuflisted ]],
}, true)

-- Commands
cmd [[command! WhatHighlight :call util#syntax_stack()]]
cmd [[command! PackerInstall packadd packer.nvim | lua require('plugins').install()]]
cmd [[command! PackerUpdate packadd packer.nvim | lua require('plugins').update()]]
cmd [[command! PackerSync packadd packer.nvim | lua require('plugins').sync()]]
cmd [[command! PackerClean packadd packer.nvim | lua require('plugins').clean()]]
cmd [[command! PackerCompile packadd packer.nvim | lua require('plugins').compile()]]

-- Keybindings
local silent = { silent = true }
-- Disable annoying F1 binding
map('', '<f1>', '<cmd>FloatermToggle<cr>')

-- Run a build
map('n', '<localleader><localleader>', '<cmd>Make<cr>', silent)

-- Quit, close buffers, etc.
map('n', '<leader>q', '<cmd>qa<cr>', silent)
map('n', '<leader>x', '<cmd>x!<cr>', silent)
map('n', '<leader>d', '<cmd>Sayonara<cr>', { silent = true, nowait = true })

-- A little Emacs in my Neovim
map('i', '<c-s>', '<esc><cmd>w<cr>a', silent)

-- Save buffer
map('n', '<leader>w', '<cmd>w<cr>', { silent = true })

-- Version control
map('n', 'gs', '<cmd>Neogit<cr>', silent)

-- Esc in the terminal
map('t', 'jj', [[<C-\><C-n>]])

-- Yank to clipboard
map({ 'n', 'v' }, 'y+', '<cmd>set opfunc=util#clipboard_yank<cr>g@', silent)

-- Window movement
map('n', '<c-h>', '<c-w>h')
map('n', '<c-j>', '<c-w>j')
map('n', '<c-k>', '<c-w>k')
map('n', '<c-l>', '<c-w>l')

-- Tab movement
map('n', '<c-Left>', '<cmd>tabpre<cr>')
map('n', '<c-Right>', '<cmd>tabnext<cr>')

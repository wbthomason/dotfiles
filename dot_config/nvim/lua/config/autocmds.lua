-- Autocommands
local autocmd = vim.api.nvim_create_autocmd
local augroup = vim.api.nvim_create_augroup
autocmd('VimEnter', {
  group = augroup('start_screen', { clear = true }),
  once = true,
  callback = function()
    require('start').start()
  end,
})
local misc_aucmds = augroup('misc_aucmds', { clear = true })
autocmd('BufWinEnter', { group = misc_aucmds, command = 'checktime' })
autocmd('TextYankPost', {
  group = misc_aucmds,
  callback = function()
    vim.highlight.on_yank()
  end,
})
autocmd('FileType', { group = misc_aucmds, pattern = 'qf', command = 'set nobuflisted' })
vim.cmd [[silent! autocmd! FileExplorer *]]
autocmd('BufEnter', {
  pattern = '*',
  callback = function(args)
    local file_info = vim.loop.fs_stat(args.file)
    if file_info and file_info.type == 'directory' then
      require('neo-tree').setup {}
      vim.cmd('Neotree position=current ' .. args.file)
    end
  end,
})
autocmd('BufReadPre', {
  group = misc_aucmds,
  callback = function()
    require 'config.lsp'
  end,
  once = true,
})

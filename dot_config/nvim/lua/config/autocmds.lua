-- Autocommands
local autocmd = vim.api.nvim_create_autocmd
local augroup = vim.api.nvim_create_augroup
autocmd('VimEnter', {
  group = augroup('start_screen', { clear = true }),
  once = true,
  callback = function()
    if vim.fn.argc() ~= 0 or vim.o.insertmode or not vim.o.modifiable then
      vim.api.nvim_exec_autocmds('User', { pattern = 'ActuallyEditing' })
      return
    end
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
      require 'neo-tree'
      return true
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

autocmd('BufReadPost', {
  group = misc_aucmds,
  once = true,
  callback = function()
    autocmd({
      'WinScrolled',
      'WinResized',
      'BufWinEnter',
      'CursorHold',
      'InsertLeave',
      'BufModifiedSet',
    }, {
      group = vim.api.nvim_create_augroup('barbecue.updater', {}),
      callback = function()
        require('barbecue.ui').update()
      end,
    })
  end,
})

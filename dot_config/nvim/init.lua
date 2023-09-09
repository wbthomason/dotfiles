-- -- Monkeypatch for slow watchfiles
-- local ok, wf = pcall(require, 'vim.lsp._watchfiles')
-- if ok then
--   -- disable lsp watcher. Too slow on linux
--   wf._watchfunc = function()
--     return function() end
--   end
-- end

local FSWATCH_EVENTS = {
  Created = 1,
  Updated = 2,
  Removed = 3,
  -- Renamed
  OwnerModified = 2,
  AttributeModified = 2,
  MovedFrom = 1,
  MovedTo = 3,
  -- IsFile
  -- IsDir
  -- IsSymLink
  -- Link
  -- Overflow
}

--- @param data string
--- @param opts table
--- @param callback fun(path: string, event: integer)
local function fswatch_output_handler(data, opts, callback)
  local d = vim.split(data, '%s+')
  local cpath = d[1]

  for i = 2, #d do
    if d[i] == 'IsDir' or d[i] == 'IsSymLink' or d[i] == 'PlatformSpecific' then
      return
    end
  end

  if opts.include_pattern and opts.include_pattern:match(cpath) == nil then
    return
  end

  if opts.exclude_pattern and opts.exclude_pattern:match(cpath) ~= nil then
    return
  end

  for i = 2, #d do
    local e = FSWATCH_EVENTS[d[i]]
    if e then
      callback(cpath, e)
    end
  end
end

local function fswatch(path, opts, callback)
  local obj = vim.system({
    'fswatch',
    '--recursive',
    '--event-flags',
    '--exclude',
    '/.git/',
    path,
  }, {
    stdout = function(err, data)
      if err ~= nil then
        vim.notify('[fswatch] ' .. err, vim.log.levels.ERROR)
      end

      if data ~= nil then
        for line in vim.gsplit(data, '\n', { plain = true, trimempty = true }) do
          fswatch_output_handler(line, opts, callback)
        end
      end
    end,
  })

  return function()
    obj:kill(2)
  end
end

if vim.fn.executable 'fswatch' == 1 then
  require('vim.lsp._watchfiles')._watchfunc = fswatch
end

-- Leader/local leader - lazy.nvim needs these set first
vim.g.mapleader = [[ ]]
vim.g.maplocalleader = [[,]]

local lazypath = vim.fn.stdpath 'data' .. '/lazy/lazy.nvim'
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system {
    'git',
    'clone',
    '--filter=blob:none',
    '--single-branch',
    'https://github.com/folke/lazy.nvim.git',
    lazypath,
  }
end

vim.loader.enable()
vim.opt.runtimepath:prepend(lazypath)

vim.g.loaded_python3_provider = 0
vim.g.loaded_ruby_provider = 0
vim.g.loaded_perl_provider = 0
vim.g.loaded_node_provider = 0
require('lazy').setup('plugins', {
  defaults = { lazy = true },
  performance = {
    cache = { enabled = true },
    rtp = {
      disabled_plugins = {
        'gzip',
        'matchit',
        'matchparen',
        'netrwPlugin',
        'rplugin',
        'tarPlugin',
        'tohtml',
        'tutor',
        'zipPlugin',
      },
    },
  },
})

require 'config.options'
require 'config.autocmds'
require 'config.keymaps'
require 'config.colorscheme'

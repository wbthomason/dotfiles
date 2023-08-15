local dap = require 'dap'

-- Debugpy
dap.adapters.python = {
  type = 'executable',
  command = 'python',
  args = { '-m', 'debugpy.adapter' },
}

dap.configurations.python = {
  {
    type = 'python',
    request = 'launch',
    name = 'Launch file',
    program = '${file}',
    pythonPath = function()
      local venv_path = vim.fn.getenv 'VIRTUAL_ENVIRONMENT'
      if venv_path ~= vim.NIL and venv_path ~= '' then
        return venv_path .. '/bin/python'
      else
        return '/usr/bin/python'
      end
    end,
  },
}

-- Neovim Lua
dap.adapters.nlua = function(callback, config)
  callback { type = 'server', host = config.host, port = config.port }
end

dap.configurations.lua = {
  {
    type = 'nlua',
    request = 'attach',
    name = 'Attach to running Neovim instance',
    host = function()
      local value = vim.fn.input 'Host [127.0.0.1]: '
      if value ~= '' then
        return value
      end
      return '127.0.0.1'
    end,
    port = function()
      local val = tonumber(vim.fn.input 'Port: ')
      assert(val, 'Please provide a port number')
      return val
    end,
  },
}

-- lldb
if not dap.adapters['codelldb'] then
  require('dap').adapters['codelldb'] = {
    type = 'server',
    host = 'localhost',
    port = '${port}',
    executable = {
      command = 'codelldb',
      args = {
        '--port',
        '${port}',
      },
    },
  }
end
for _, lang in ipairs { 'c', 'cpp' } do
  dap.configurations[lang] = {
    {
      type = 'codelldb',
      request = 'launch',
      name = 'Launch file',
      program = function()
        return vim.fn.input('Path to executable: ', vim.fn.getcwd() .. '/', 'file')
      end,
      cwd = '${workspaceFolder}',
    },
    {
      type = 'codelldb',
      request = 'attach',
      name = 'Attach to process',
      processId = require('dap.utils').pick_process,
      cwd = '${workspaceFolder}',
    },
  }
end

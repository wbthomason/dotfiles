local dap = require('dap')
local function get_python()
    local python_path = '/usr/bin/python'
    local virtualenv = vim.fn.getenv('VIRTUAL_ENV')
    if virtualenv ~= vim.NIL and virtualenv ~= '' then
        python_path = virtualenv .. '/bin/python'
    end

    return python_path
end

dap.adapters.python = {
    type = 'executable',
    command = get_python(),
    args = {'-m', 'debugpy.adapter'}
}

dap.configurations.python = {
    {
        type = 'python',
        request = 'launch',
        name = "Launch file",
        program = "${file}",
        pythonPath = get_python()
    }
}

dap.adapters.cpp = {
    attach = {pidProperty = "pid", pidSelect = "ask"},
    command = 'lldb-vscode', -- my binary was called 'lldb-vscode-11'
    env = {LLDB_LAUNCH_FLAG_LAUNCH_IN_TTY = "YES"},
    name = "lldb"
}

local M = {}
local last_gdb_config

M.start_c_debugger = function(args, mi_mode, mi_debugger_path)
    if args and #args > 0 then
        last_gdb_config = {
            type = "cpp",
            name = args[1],
            request = "launch",
            program = table.remove(args, 1),
            args = args,
            cwd = vim.fn.getcwd(),
            environment = {},
            externalConsole = true,
            MIMode = mi_mode or "gdb",
            MIDebuggerPath = mi_debugger_path
        }
    end

    if not last_gdb_config then
        print(
            'No binary to debug set! Use ":DebugC <binary> <args>" or ":DebugRust <binary> <args>"')
        return
    end

    dap.launch(dap.adapters.cpp, last_gdb_config)
    dap.repl.open()
end

vim.cmd [[
    command! -complete=file -nargs=* DebugC lua require "dap-config".start_c_debugger({<f-args>}, "gdb")
]]

vim.cmd [[
    command! -complete=file -nargs=* DebugRust lua require "dap-config".start_c_debugger({<f-args>}, "gdb", "rust-gdb")
]]

return M

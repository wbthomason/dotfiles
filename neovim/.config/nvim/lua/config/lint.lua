local lint = require('lint')
local linters = lint.linters

local cppcheck_pattern = [[([^:]*):(%d*):(%d*): %[([^%]\]*)%] ([^:]*): (.*)]]
linters.cppcheck = {
  cmd = 'cppcheck',
  stdin = false,
  args = {
    '--enable=warning,style,performance,information', '--language=c++',
    '--project=build/compile_commands.json', '--inline-suppr', '--quiet',
    '--cppcheck-build-dir=build',
    '--template={file}:{line}:{column}: [{id}] {severity}: {message}'
  },
  stream = 'stderr',
  parser = function(output, bufnr)
    local buffer_path = vim.api.nvim_buf_get_name(bufnr)
    local diagnostics = {}
    for line in vim.gsplit(output, '\n') do
      for file, lineno, colno, id, severity, msg in string.gmatch(line, cppcheck_pattern) do
        if file == buffer_path then
          local diagnostic = {}
          diagnostic.range = {}
          local line_num = tonumber(lineno)
          local col_num = tonumber(colno)
          local range = {line = line_num - 1, character = col_num - 1}
          diagnostic.range.start = range
          diagnostic.range['end'] = range
          if severity == 'style' or severity == 'information' then
            diagnostic.severity = 3
          elseif severity == 'warning' or severity == 'performance' then
            diagnostic.severity = 2
          elseif severity == 'error' then
            diagnostic.severity = 1
          end

          diagnostic.source = 'cppcheck(' .. id .. ')'
          diagnostic.message = msg
          diagnostics[#diagnostics + 1] = diagnostic
        end
      end
    end

    return diagnostics
  end
}

lint.linters_by_ft = {
  markdown = {'languagetool', 'vale'},
  sh = {'shellcheck'},
  zsh = {'shellcheck'},
  bash = {'shellcheck'},
  cpp = {'cppcheck'}
}

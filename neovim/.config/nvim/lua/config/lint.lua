local lint = require('lint')
local linters = lint.linters

linters.clangcheck = {
  cmd = 'clang-check',
  stdin = false,
  args = {'--analyze'},
  stream = 'stderr'
}

lint.linters_by_ft = {
  markdown = {'languagetool', 'vale'},
  sh = {'shellcheck'},
  zsh = {'shellcheck'},
  bash = {'shellcheck'},
  cpp = {'clangcheck', 'clangtidy', 'cppcheck', 'cpplint'}
}

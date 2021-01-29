require('compe').setup {
  enabled = true,
  debug = false,
  min_length = 1,
  preselect = 'always',
  source = {path = true, buffer = true, nvim_lsp = true, nvim_lua = true, vsnip = false}
}

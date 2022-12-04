-- Skip some remote provider loading
vim.g.loaded_python3_provider = 0
vim.g.loaded_node_provider = 0
vim.g.loaded_perl_provider = 0
vim.g.loaded_ruby_provider = 0

-- Disable some built-in plugins we don't want
local disabled_built_ins = {
  'loaded_gzip',
  'loaded_man',
  'loaded_matchit',
  'loaded_matchparen',
  'loaded_shada_plugin',
  'loaded_tarPlugin',
  'loaded_tar',
  'loaded_zipPlugin',
  'loaded_zip',
  'loaded_netrwPlugin',
}

for i = 1, 10 do
  vim.g[disabled_built_ins[i]] = 1
end

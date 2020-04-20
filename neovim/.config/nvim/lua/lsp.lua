local nvim_lsp = require('nvim_lsp')

local function on_attach(_, bufnr)
  local opts = { noremap=true, silent=true }
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gd', '<cmd>lua vim.lsp.buf.definition()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>e', '<cmd>lua vim.lsp.util.show_line_diagnostics()<CR>', opts)
end

nvim_lsp.vimls.setup { on_attach = on_attach }

nvim_lsp.clangd.setup {
  cmd = { "clangd",
    "--background-index",
    "--clang-tidy",
    "--completion-style=bundled",
    "--header-insertion=iwyu",
    "--suggest-missing-includes"
  },
  on_attach = on_attach
}

nvim_lsp.bashls.setup { on_attach = on_attach }

nvim_lsp.html.setup { on_attach = on_attach }

nvim_lsp.julials.setup { on_attach = on_attach }

nvim_lsp.ocamllsp.setup { on_attach = on_attach }

nvim_lsp.pyls_ms.setup {
  cmd = { "mspyls" },
  on_attach = on_attach
}

nvim_lsp.rust_analyzer.setup { on_attach = on_attach }

nvim_lsp.sumneko_lua.setup {
  cmd = { "lua-language-server" },
  settings = {
    Lua = {
      diagnostics = {
        globals = { "vim" }
      }
    }
  },
  on_attach = on_attach
}

nvim_lsp.texlab.setup { on_attach = on_attach }

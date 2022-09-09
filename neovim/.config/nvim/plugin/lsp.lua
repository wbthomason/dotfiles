require('lua-dev').setup { lspconfig = { cmd = { 'lua-language-server' }, prefer_null_ls = true } }

local lspconfig = require 'lspconfig'
local trouble = require 'trouble'
local null_ls = require 'null-ls'
-- local lightbulb = require 'nvim-lightbulb'

local lsp = vim.lsp
local buf_keymap = vim.api.nvim_buf_set_keymap
local cmd = vim.cmd

-- vim.api.nvim_command 'hi link LightBulbFloatWin YellowFloat'
-- vim.api.nvim_command 'hi link LightBulbVirtualText YellowFloat'

local sign_define = vim.fn.sign_define
sign_define('DiagnosticSignError', { text = '', numhl = 'RedSign' })
sign_define('DiagnosticSignWarn', { text = '', numhl = 'YellowSign' })
sign_define('DiagnosticSignInfo', { text = '', numhl = 'WhiteSign' })
sign_define('DiagnosticSignHint', { text = '', numhl = 'BlueSign' })
trouble.setup()
-- lightbulb.setup {
--   sign = { enabled = false },
--   virtual_text = { enabled = true, text = '', hl_mode = 'blend' },
--   float = { enabled = false, text = '', win_opts = { winblend = 100, anchor = 'NE' } },
-- }

vim.diagnostic.config { virtual_lines = { only_current_line = true }, virtual_text = false }
lsp.handlers['textDocument/publishDiagnostics'] = lsp.with(lsp.diagnostic.on_publish_diagnostics, {
  virtual_text = false,
  signs = true,
  update_in_insert = false,
  underline = true,
})

local severity = {
  'error',
  'warn',
  'info',
  'hint', -- map both hint and info to info?
}

lsp.handlers['window/showMessage'] = function(err, method, params, client_id)
  vim.notify(method.message, severity[params.type])
end

require('lsp_signature').setup { bind = true, handler_opts = { border = 'single' } }
local keymap_opts = { noremap = true, silent = true }
local function on_attach(client)
  require('lsp_signature').on_attach { bind = true, handler_opts = { border = 'single' } }
  buf_keymap(0, 'n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<CR>', keymap_opts)
  buf_keymap(0, 'n', 'gd', '<cmd>lua require"telescope.builtin".lsp_definitions()<CR>', keymap_opts)
  buf_keymap(0, 'n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', keymap_opts)
  buf_keymap(0, 'n', 'gi', '<cmd>lua require"telescope.builtin".lsp_implementations()<CR>', keymap_opts)
  buf_keymap(0, 'n', 'gS', '<cmd>lua vim.lsp.buf.signature_help()<CR>', keymap_opts)
  buf_keymap(0, 'n', 'gTD', '<cmd>lua vim.lsp.buf.type_definition()<CR>', keymap_opts)
  buf_keymap(0, 'n', '<leader>rn', '<cmd>lua require"renamer".rename()<CR>', keymap_opts)
  buf_keymap(0, 'v', '<leader>rn', '<cmd>lua require"renamer".rename()<CR>', keymap_opts)
  buf_keymap(0, 'n', 'gr', '<cmd>lua require"telescope.builtin".lsp_references()<CR>', keymap_opts)
  buf_keymap(0, 'n', 'gA', '<cmd>lua vim.lsp.buf.code_action()<CR>', keymap_opts)
  buf_keymap(0, 'v', 'gA', '<cmd>lua vim.lsp.buf.range_code_action()<CR>', keymap_opts)
  buf_keymap(0, 'n', ']e', '<cmd>lua vim.diagnostic.goto_next { float = {scope = "line"} }<cr>', keymap_opts)
  buf_keymap(0, 'n', '[e', '<cmd>lua vim.diagnostic.goto_prev { float = {scope = "line"} }<cr>', keymap_opts)

  if client.server_capabilities.documentFormattingProvider then
    buf_keymap(0, 'n', '<leader>f', '<cmd>lua vim.lsp.buf.format { async = true }<cr>', keymap_opts)
  end

  cmd 'augroup lsp_aucmds'
  if client.server_capabilities.documentHighlightProvider then
    cmd 'au CursorHold <buffer> lua vim.lsp.buf.document_highlight()'
    cmd 'au CursorMoved <buffer> lua vim.lsp.buf.clear_references()'
  end

  -- cmd 'au CursorHold,CursorHoldI <buffer> lua require"nvim-lightbulb".update_lightbulb ()'
  -- cmd 'au CursorHold,CursorHoldI <buffer> lua vim.diagnostic.open_float(0, { scope = "line" })'
  cmd 'augroup END'
end

local function prefer_null_ls_fmt(client)
  client.server_capabilities.documentHighlightProvider = false
  client.server_capabilities.documentFormattingProvider = false
  on_attach(client)
end

local servers = {
  bashls = {},
  cmake = {},
  cssls = {
    cmd = { 'vscode-css-languageserver', '--stdio' },
    filetypes = { 'css', 'scss', 'less', 'sass' },
    root_dir = lspconfig.util.root_pattern('package.json', '.git'),
  },
  -- ghcide = {},
  html = { cmd = { 'vscode-html-languageserver', '--stdio' } },
  jsonls = { prefer_null_ls = true, cmd = { 'vscode-json-languageserver', '--stdio' } },
  julials = { settings = { julia = { format = { indent = 2 } } } },
  ocamllsp = {},
  pyright = { settings = { python = { formatting = { provider = 'yapf' }, linting = { pytypeEnabled = true } } } },
  rust_analyzer = {},
  sumneko_lua = { prefer_null_ls = true },
  -- sumneko_lua = {
  --   prefer_null_ls = true,
  --   cmd = { 'lua-language-server' },
  --   settings = {
  --     Lua = {
  --       diagnostics = { globals = { 'vim' } },
  --       runtime = { version = 'LuaJIT', path = vim.split(package.path, ';') },
  --       workspace = {
  --         library = {
  --           [vim.fn.expand '$VIMRUNTIME/lua'] = true,
  --           [vim.fn.expand '$VIMRUNTIME/lua/vim/lsp'] = true,
  --         },
  --       },
  --     },
  --   },
  -- },
  texlab = {
    settings = {
      texlab = {
        chktex = { onOpenAndSave = true },
        formatterLineLength = 100,
        forwardSearch = {
          executable = 'sioyek',
          args = { '--forward-search-file', '%f', '--forward-search-line', '%l', '%p' },
        },
      },
    },
  },
  ltex = {
    cmd = { '/usr/bin/ltex-ls' },
    on_attach = function(client, bufnr)
      require('ltex_extra').setup {}
    end,
    settings = {
      ltex = {
        checkFrequency = 'save',
        additionalRules = { enablePickyRules = true },
        ['ltex-ls'] = { path = '/opt/ltex-ls' },
      },
    },
  },
  tsserver = {},
  vimls = {},
}

local client_capabilities = vim.lsp.protocol.make_client_capabilities()
client_capabilities = require('cmp_nvim_lsp').update_capabilities(client_capabilities)
client_capabilities.offsetEncoding = { 'utf-16' }

require('clangd_extensions').setup {
  server = {
    on_attach = prefer_null_ls_fmt,
    cmd = {
      'clangd',
      '--background-index',
      '--clang-tidy',
      '--completion-style=bundled',
      '--header-insertion=iwyu',
      '--cross-file-rename',
    },
    init_options = {
      clangdFileStatus = true,
      usePlaceholders = true,
      completeUnimported = true,
      semanticHighlighting = true,
    },
    capabilities = client_capabilities,
  },
  extensions = { inlay_hints = { only_current_line = false, show_variable_name = true } },
}

for server, config in pairs(servers) do
  if config.prefer_null_ls then
    if config.on_attach then
      local old_on_attach = config.on_attach
      config.on_attach = function(client, bufnr)
        old_on_attach(client, bufnr)
        prefer_null_ls_fmt(client)
      end
    else
      config.on_attach = prefer_null_ls_fmt
    end
  elseif not config.on_attach then
    config.on_attach = on_attach
  end

  config.capabilities = vim.tbl_deep_extend('keep', config.capabilities or {}, client_capabilities)
  lspconfig[server].setup(config)
end

-- null-ls setup
local null_fmt = null_ls.builtins.formatting
local null_diag = null_ls.builtins.diagnostics
local null_act = null_ls.builtins.code_actions
null_ls.setup {
  sources = {
    null_diag.chktex,
    -- null_diag.cppcheck,
    -- null_diag.proselint,
    -- null_diag.pylint,
    null_diag.selene,
    null_diag.shellcheck,
    null_diag.teal,
    -- null_diag.vale,
    null_diag.vint,
    -- null_diag.write_good.with { filetypes = { 'markdown', 'tex' } },
    null_fmt.clang_format,
    -- null_fmt.cmake_format,
    null_fmt.isort,
    null_fmt.prettier,
    null_fmt.rustfmt,
    null_fmt.shfmt,
    null_fmt.stylua,
    null_fmt.trim_whitespace,
    null_fmt.yapf,
    -- null_fmt.black
    null_act.gitsigns,
    -- null_act.refactoring.with { filetypes = { 'javascript', 'typescript', 'lua', 'python', 'c', 'cpp' } },
  },
  on_attach = on_attach,
}

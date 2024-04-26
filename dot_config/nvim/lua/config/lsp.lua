local lspconfig = require 'lspconfig'
local lsp = vim.lsp
local buf_keymap = vim.api.nvim_buf_set_keymap
local cmd = vim.cmd

local border = {
  { '🭽', 'FloatBorder' },
  { '▔', 'FloatBorder' },
  { '🭾', 'FloatBorder' },
  { '▕', 'FloatBorder' },
  { '🭿', 'FloatBorder' },
  { '▁', 'FloatBorder' },
  { '🭼', 'FloatBorder' },
  { '▏', 'FloatBorder' },
}

local sign_define = vim.fn.sign_define
sign_define('DiagnosticSignError', { text = '', numhl = 'RedSign' })
sign_define('DiagnosticSignWarn', { text = '', numhl = 'YellowSign' })
sign_define('DiagnosticSignInfo', { text = '', numhl = 'WhiteSign' })
sign_define('DiagnosticSignHint', { text = '', numhl = 'BlueSign' })
vim.diagnostic.config {
  virtual_lines = { only_current_line = true },
  virtual_text = false,
  { float = { border = border } },
}
lsp.handlers['textDocument/publishDiagnostics'] = lsp.with(lsp.diagnostic.on_publish_diagnostics, {
  virtual_text = false,
  signs = true,
  update_in_insert = false,
  underline = true,
})

local keymap_opts = { noremap = true, silent = true }
local function setup_keymaps(client, _bufnr)
  buf_keymap(0, 'n', 'gD', '', vim.tbl_extend('keep', { callback = vim.lsp.buf.declaration }, keymap_opts))
  buf_keymap(0, 'n', 'gd', '<cmd>Glance definitions<CR>', keymap_opts)
  buf_keymap(0, 'n', 'gi', '<cmd>Glance implementations<CR>', keymap_opts)
  buf_keymap(0, 'n', 'gS', '', vim.tbl_extend('keep', { callback = vim.lsp.buf.signature_help }, keymap_opts))
  buf_keymap(0, 'n', 'gTD', '', vim.tbl_extend('keep', { callback = vim.lsp.buf.type_definition }, keymap_opts))
  buf_keymap(0, 'n', '<leader>rn', '', {
    callback = function()
      return ':IncRename ' .. vim.fn.expand '<cword>'
    end,
    expr = true,
  })
  buf_keymap(0, 'v', '<leader>rn', '', {
    callback = function()
      return ':IncRename ' .. vim.fn.expand '<cword>'
    end,
    expr = true,
  })
  buf_keymap(0, 'n', 'gr', '<cmd>Glance references<CR>', keymap_opts)
  buf_keymap(0, 'n', 'gA', '', vim.tbl_extend('keep', { callback = vim.lsp.buf.code_action }, keymap_opts))
  buf_keymap(0, 'v', 'gA', '', vim.tbl_extend('keep', { callback = vim.lsp.buf.range_code_action }, keymap_opts))

  -- TODO: Use the nicer new API for autocommands
  cmd 'augroup lsp_aucmds'
  if client.server_capabilities.documentHighlightProvider then
    cmd 'au CursorHold <buffer> lua vim.lsp.buf.document_highlight()'
    cmd 'au CursorMoved <buffer> lua vim.lsp.buf.clear_references()'
  end
  cmd 'augroup END'
end

local client_capabilities = require('cmp_nvim_lsp').default_capabilities()
client_capabilities.offsetEncoding = { 'utf-16' }

local on_attach_fns = {
  function(client, bufnr)
    if client.server_capabilities.documentSymbolProvider then
      require('nvim-navic').attach(client, bufnr)
    end

    vim.lsp.inlay_hint.enable(
      client.server_capabilities.inlayHintProvider ~= nil and client.server_capabilities.inlayHintProvider ~= false
    )
  end,
  setup_keymaps,
}

local function do_on_attach_fns(client, bufnr)
  for _, fn in ipairs(on_attach_fns) do
    fn(client, bufnr)
  end
end

local servers = {
  bashls = {},
  neocmake = {},
  clangd = {
    on_attach = function(client, bufnr)
      do_on_attach_fns(client, bufnr, true)
    end,
    cmd = {
      'clangd',
      '--background-index',
      '--clang-tidy',
      '--completion-style=bundled',
      '--header-insertion=iwyu',
      '--cross-file-rename',
      '--all-scopes-completion',
      '--log=error',
      '--suggest-missing-includes',
      '--pch-storage=memory',
    },
    init_options = {
      clangdFileStatus = true,
      usePlaceholders = true,
      completeUnimported = true,
      semanticHighlighting = true,
    },
    capabilities = client_capabilities,
  },
  cssls = {
    cmd = { 'vscode-css-languageserver', '--stdio' },
    filetypes = { 'css', 'scss', 'less', 'sass' },
    root_dir = lspconfig.util.root_pattern('package.json', '.git'),
  },
  dockerls = {},
  -- ghcide = {},
  html = { cmd = { 'vscode-html-languageserver', '--stdio' } },
  jsonls = { cmd = { 'vscode-json-languageserver', '--stdio' } },
  julials = {
    on_new_config = function(new_config, _)
      local julia = vim.fn.expand '~/.julia/environments/nvim-lspconfig/bin/julia'
      if lspconfig.util.path.is_file(julia) then
        new_config.cmd[1] = julia
      end
    end,
    settings = { julia = { format = { indent = 2 } } },
  },
  ocamllsp = {},
  pyright = {
    settings = {
      python = {
        analysis = { useLibraryCodeForTypes = true },
        formatting = { provider = 'yapf' },
        linting = { pytypeEnabled = true },
      },
    },
  },
  -- pylyzer = {},
  ruff_lsp = {},
  rust_analyzer = {
    settings = {
      ['rust-analyzer'] = {
        cargo = { allFeatures = true },
        checkOnSave = {
          command = 'clippy',
          extraArgs = { '--no-deps' },
        },
      },
    },
  },
  lua_ls = {
    before_init = require('neodev.lsp').before_init,
    single_file_support = true,
    settings = {
      Lua = {
        workspace = {
          checkThirdParty = false,
          library = vim.api.nvim_get_runtime_file('', true),
        },
        completion = {
          workspaceWord = true,
          callSnippet = 'Both',
        },
        runtime = { version = 'LuaJIT' },
        diagnostics = { globals = { 'vim' } },
        telemetry = { enable = false },
      },
      diagnostics = {
        groupSeverity = {
          strong = 'Warning',
          strict = 'Warning',
        },
        groupFileStatus = {
          ['ambiguity'] = 'Opened',
          ['await'] = 'Opened',
          ['codestyle'] = 'None',
          ['duplicate'] = 'Opened',
          ['global'] = 'Opened',
          ['luadoc'] = 'Opened',
          ['redefined'] = 'Opened',
          ['strict'] = 'Opened',
          ['strong'] = 'Opened',
          ['type-check'] = 'Opened',
          ['unbalanced'] = 'Opened',
          ['unused'] = 'Opened',
        },
        unusedLocalExclude = { '_*' },
      },
      format = {
        enable = false,
        defaultConfig = {
          indent_style = 'space',
          indent_size = '2',
          continuation_indent_size = '2',
        },
      },
    },
  },
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
    cmd = {
      (function()
        if vim.loop.os_uname().sysname == 'Darwin' then
          return '/opt/homebrew/bin/ltex-ls'
        end

        return '/usr/bin/ltex-ls'
      end)(),
    },
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

require('clangd_extensions').setup {
  ast = {
    role_icons = {
      type = '',
      declaration = '',
      expression = '',
      specifier = '',
      statement = '',
      ['template argument'] = '',
    },
    kind_icons = {
      Compound = '',
      Recovery = '',
      TranslationUnit = '',
      PackExpansion = '',
      TemplateTypeParm = '',
      TemplateTemplateParm = '',
      TemplateParamObject = '',
    },
  },
}

for server, config in pairs(servers) do
  -- TODO: maybe refactor to avoid creating a new closure per server
  if config.on_attach then
    local old_on_attach = config.on_attach
    config.on_attach = function(client, bufnr)
      old_on_attach(client, bufnr)
      do_on_attach_fns(client, bufnr)
    end
  else
    config.on_attach = function(client, bufnr)
      do_on_attach_fns(client, bufnr)
    end
  end

  config.capabilities = vim.tbl_deep_extend('keep', config.capabilities or {}, client_capabilities)
  lspconfig[server].setup(config)
end

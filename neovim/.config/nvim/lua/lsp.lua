local nvim_lsp = require('nvim_lsp')
local lsp_status = require('lsp-status')
local diagnostic = require('diagnostic')

local severity_map = {'E', 'W', 'I', 'H'}

local function diagnostics_to_items(diagnostics)
  local items = {}
  local grouped = setmetatable({}, {
    __index = function(t, k)
      local v = {}
      rawset(t, k, v)
      return v
    end
  })

  local fname = vim.api.nvim_buf_get_name(0)
  for _, d in ipairs(diagnostics) do
    local range = d.range
    table.insert(grouped[fname], {start = range.start, item = d})
  end

  local keys = vim.tbl_keys(grouped)
  table.sort(keys)
  for _, name in ipairs(keys) do
    local rows = grouped[name]
    table.sort(rows, function(a, b)
      if a.start.line < b.start.line or
        (a.start.line == b.start.line and a.start.character < b.start.character) then
        return true
      end

      return false
    end)

    for _, row in ipairs(rows) do
      table.insert(items, {
        filename = name,
        lnum = row.start.line + 1,
        text = row.item.message,
        ['type'] = severity_map[row.item.severity],
        col = row.start.character + 1,
        vcol = true
      })
    end
  end

  return items
end

diagnostic.diagnostics_loclist = function(local_result)
  if local_result then for _, v in ipairs(local_result) do v.uri = v.uri or local_result.uri end end
  vim.lsp.util.set_loclist(diagnostics_to_items(local_result))
end

local texlab_search_status = vim.tbl_add_reverse_lookup {
  Success = 0,
  Error = 1,
  Failure = 2,
  Unconfigured = 3
}

lsp_status.config {
  kind_labels = vim.g.completion_customize_lsp_label,
  select_symbol = function(cursor_pos, symbol)
    if symbol.valueRange then
      local value_range = {
        ['start'] = {character = 0, line = vim.fn.byte2line(symbol.valueRange[1])},
        ['end'] = {character = 0, line = vim.fn.byte2line(symbol.valueRange[2])}
      }

      return require('lsp-status/util').in_range(cursor_pos, value_range)
    end
  end
}

lsp_status.register_progress()

local function make_on_attach(config)
  return function(client)
    if config.before then config.before(client) end

    lsp_status.on_attach(client)
    diagnostic.on_attach()
    local opts = {noremap = true, silent = true}
    vim.api.nvim_buf_set_keymap(0, 'n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<CR>', opts)
    vim.api.nvim_buf_set_keymap(0, 'n', 'gd', '<cmd>lua vim.lsp.buf.definition()<CR>', opts)
    vim.api.nvim_buf_set_keymap(0, 'n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
    vim.api.nvim_buf_set_keymap(0, 'n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
    vim.api.nvim_buf_set_keymap(0, 'n', '<c-s>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
    vim.api.nvim_buf_set_keymap(0, 'n', 'gTD', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
    vim.api.nvim_buf_set_keymap(0, 'n', '<leader>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
    vim.api.nvim_buf_set_keymap(0, 'n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
    vim.api.nvim_buf_set_keymap(0, 'n', 'gA', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
    vim.api.nvim_buf_set_keymap(0, 'n', '<leader>e',
                                '<cmd>lua vim.lsp.util.show_line_diagnostics()<CR>', opts)
    vim.api.nvim_buf_set_keymap(0, 'n', ']e', '<cmd>NextDiagnosticCycle<cr>', opts)
    vim.api.nvim_buf_set_keymap(0, 'n', '[e', '<cmd>PrevDiagnosticCycle<cr>', opts)

    if client.resolved_capabilities.document_formatting then
      vim.api.nvim_buf_set_keymap(0, 'n', '<leader>lf', '<cmd>lua vim.lsp.buf.formatting()<cr>',
                                  opts)
    end

    if client.resolved_capabilities.document_highlight then
      vim.api.nvim_command('augroup lsp_aucmds')
      vim.api.nvim_command('au CursorHold <buffer> lua vim.lsp.buf.document_highlight()')
      vim.api.nvim_command('au CursorMoved <buffer> lua vim.lsp.buf.clear_references()')
      vim.api.nvim_command('augroup END')
    end

    if config.after then config.after(client) end
  end
end

local servers = {
  bashls = {},
  clangd = {
    cmd = {
      'clangd', -- '--background-index',
      '--clang-tidy', '--completion-style=bundled', '--header-insertion=iwyu',
      '--suggest-missing-includes', '--cross-file-rename'
    },
    callbacks = lsp_status.extensions.clangd.setup(),
    init_options = {
      clangdFileStatus = true,
      usePlaceholders = true,
      completeUnimported = true,
      semanticHighlighting = true
    }
  },
  cssls = {
    filetypes = {"css", "scss", "less", "sass"},
    root_dir = nvim_lsp.util.root_pattern("package.json", ".git")
  },
  ghcide = {},
  html = {},
  jsonls = {cmd = {'json-languageserver', '--stdio'}},
  julials = {
    cmd = {
      "julia", "--startup-file=no", "--history-file=no", "-e", [[
        using Pkg;
        Pkg.instantiate()
        using LanguageServer,  LanguageServer.SymbolServer;
        depot_path = get(ENV, "JULIA_DEPOT_PATH", "")
        project_path = dirname(something(Base.current_project(pwd()), Base.load_path_expand(LOAD_PATH[2])))
        # Make sure that we only load packages from this environment specifically.
        empty!(LOAD_PATH)
        push!(LOAD_PATH, "@")
        server = LanguageServer.LanguageServerInstance(stdin, stdout, project_path, depot_path);
        server.runlinter = true;
        run(server);
        ]]
    },
    settings = {julia = {format = {indent = 2}}}
  },
  ocamllsp = {},
  pyls_ms = {
    cmd = {'mspyls'},
    callbacks = lsp_status.extensions.pyls_ms.setup(),
    settings = {
      python = {
        jediEnabled = false,
        analysis = {cachingLevel = 'Library'},
        formatting = {provider = 'yapf'},
        venvFolders = {"envs", ".pyenv", ".direnv", ".cache/pypoetry/virtualenvs"},
        workspaceSymbols = {enabled = true}
      }
    },
    root_dir = function(fname)
      return nvim_lsp.util.root_pattern('pyproject.toml', 'setup.py', 'setup.cfg',
                                        'requirements.txt', 'mypy.ini', '.pylintrc', '.flake8rc',
                                        '.gitignore')(fname) or
               nvim_lsp.util.find_git_ancestor(fname) or vim.loop.os_homedir()
    end
  },
  rust_analyzer = {},
  sumneko_lua = {
    cmd = {'lua-language-server'},
    settings = {
      Lua = {
        diagnostics = {globals = {'vim'}},
        completion = {keywordSnippet = 'Disable'},
        runtime = {version = 'LuaJIT', path = vim.split(package.path, ';')},
        workspace = {
          library = {
            [vim.fn.expand("$VIMRUNTIME/lua")] = true,
            [vim.fn.expand("$VIMRUNTIME/lua/vim/lsp")] = true
          }
        }
      }
    }
  },
  texlab = {
    settings = {
      latex = {forwardSearch = {executable = 'okular', args = {'--unique', 'file:%p#src:%l%f'}}}
    },
    commands = {
      TexlabForwardSearch = {
        function()
          local pos = vim.api.nvim_win_get_cursor(0)
          local params = {
            textDocument = {uri = vim.uri_from_bufnr(0)},
            position = {line = pos[1] - 1, character = pos[2]}
          }

          vim.lsp.buf_request(0, 'textDocument/forwardSearch', params, function(err, _, result, _)
            if err then error(tostring(err)) end
            print('Forward search ' .. vim.inspect(pos) .. ' ' .. texlab_search_status[result])
          end)
        end,
        description = 'Run synctex forward search'
      }
    }
  },
  tsserver = {},
  vimls = {}
}

local snippet_capabilities = {
  textDocument = {completion = {completionItem = {snippetSupport = true}}}
}

local function deep_extend(policy, ...)
  local result = {}
  local function helper(policy, k, v1, v2)
    if type(v1) ~= 'table' or type(v2) ~= 'table' then
      if policy == 'error' then
        error('Key ' .. vim.inspect(k) .. ' is already present with value ' .. vim.inspect(v1))
      elseif policy == 'force' then
        return v2
      else
        return v1
      end
    else
      return deep_extend(policy, v1, v2)
    end
  end

  for _, t in ipairs({...}) do
    for k, v in pairs(t) do
      if result[k] ~= nil then
        result[k] = helper(policy, k, result[k], v)
      else
        result[k] = v
      end
    end
  end

  return result
end

for server, config in pairs(servers) do
  config.on_attach = make_on_attach(config)
  config.capabilities = deep_extend('keep', config.capabilities or {}, lsp_status.capabilities,
                                    snippet_capabilities)

  nvim_lsp[server].setup(config)
end

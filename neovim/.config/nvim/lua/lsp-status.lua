local lsp_util = require('vim.lsp.util')
local lsp_proto = require('vim.lsp.protocol')
local functools = require('functools')

local messages = {}

local aliases = {
  pyls_ms = 'MPLS',
}

local function progress_callback(_, _, msg, client_id, buffnr)
  if not messages[buffnr] then
    messages[buffnr] = {}
  end

  if not messages[buffnr][client_id] then
    messages[buffnr][client_id] = {}
  end

  if not messages[buffnr][client_id].progress then
    messages[buffnr][client_id].progress = {}
  end

  local val = msg.value
  if val.kind then
    if val.kind == 'begin' then
      messages[buffnr][client_id].progress[msg.token] = {
        title = val.title,
        message = val.message,
        percentage = val.percentage,
        spinner = 1,
      }
    elseif val.kind == 'report' then
      messages[buffnr][client_id].progress[msg.token].message = val.message
      messages[buffnr][client_id].progress[msg.token].percentage = val.percentage
      messages[buffnr][client_id].progress[msg.token].spinner = messages[buffnr][client_id].progress[msg.token].spinner + 1
    elseif val.kind == 'end' then
      messages[buffnr][client_id].progress[msg.token].message = val.message
      messages[buffnr][client_id].progress[msg.token].done = true
      messages[buffnr][client_id].progress[msg.token].spinner = nil
    end
  else
    table.insert(messages[buffnr][client_id], { content = val, show_once = true })
  end

  vim.api.nvim_command('doautocmd User LspMessageUpdate')
end

local function register_progress()
  vim.lsp.callbacks['$/progress'] = progress_callback
end

local function register_client(client_name, bufnr)
  local buf = bufnr or vim.fn.bufnr()
  if not messages[buf] then
    messages[buf] = {}
  end

  if not messages[buf][client_name] then
    messages[buf][client_name] = {}
  end
end

local extension_callbacks = {}

extension_callbacks.pyls_ms = {
  ['python/setStatusBarMessage'] = function(_, _, message, buffnr)
    table.insert(messages[buffnr].pyls_ms, { content = message[1] })
    vim.api.nvim_command('doautocmd User LspMessageUpdate')
  end,
  ['python/beginProgress'] = function(_, _, _, buffnr)
    if not messages[buffnr] then
      messages[buffnr] = {}
    end

    if not messages[buffnr].pyls_ms then
      messages[buffnr].pyls_ms = {}
    end

    if not messages[buffnr].pyls_ms.progress then
      messages[buffnr].pyls_ms.progress = {}
    end

    if not messages[buffnr].pyls_ms.progress[1] then
      messages[buffnr].pyls_ms.progress[1] = { spinner = 1, title = 'MPLS' }
    end
  end,
  ['python/reportProgress'] = function(_, _, message, buffnr)
    messages[buffnr].pyls_ms.progress[1].spinner = messages[buffnr].pyls_ms.progress[1].spinner + 1
    messages[buffnr].pyls_ms.progress[1].title = message[1]
    vim.api.nvim_command('doautocmd User LspMessageUpdate')
  end,
  ['python/endProgress'] = function(_, _, _, buffnr)
    messages[buffnr].pyls_ms.progress[1] = nil
    vim.api.nvim_command('doautocmd User LspMessageUpdate')
  end,
}

extension_callbacks.clangd = {
  ['textDocument/clangd.fileStatus'] = function(_, _, statusMessage, _, buffnr)
    table.insert(messages[buffnr].clangd, {
      uri = statusMessage.uri,
      content = statusMessage.state,
      show_once = true
    })
    vim.api.nvim_command('doautocmd User LspMessageUpdate')
  end
}

local function extract_symbols(items, _result)
  local result = _result or {}
  for _, item in ipairs(items) do
    local kind = lsp_proto.SymbolKind[item.kind] or 'Unknown'
    local sym_range = nil
    if item.location then -- Item is a SymbolInformation
      sym_range = item.location.range
    elseif item.range then -- Item is a DocumentSymbol
      sym_range = item.range
    end

    if sym_range then
      sym_range.start.line = sym_range.start.line + 1
      sym_range['end'].line = sym_range['end'].line + 1
    end

    table.insert(result, {
      filename = item.location and vim.uri_to_fname(item.location.uri) or nil,
      range = sym_range,
      kind = kind,
      text = item.name
    })
    if item.children then
      extract_symbols(item.children, result)
    end
  end

  return result
end

local function in_range(pos, range)
  local line = pos[1]
  local char = pos[2]
  if line < range.start.line or line > range['end'].line then return false end
  if
    line == range.start.line and char < range.start.character or
    line == range['end'].line and char > range['end'].character
  then
    return false
  end

  return true
end

local function current_function_callback(_, _, result, _, _)
  vim.b.lsp_current_function = ''
  local function_symbols = functools.filter(extract_symbols(result),
    function(_, v)
      return v.kind == 'Class' or v.kind == 'Function' or v.kind == 'Method'
    end)

  if not function_symbols or #function_symbols == 0 then
    vim.api.nvim_command('doautocmd User LspStatusUpdate')
    return
  end

  local cursor_pos = vim.api.nvim_win_get_cursor(0)
  for _, sym in ipairs(function_symbols) do
    if
      sym.range and
      in_range(cursor_pos, sym.range)
    then
      local fn_name = sym.text
      if vim.fn.has_key(vim.g.completion_customize_lsp_label, sym.kind) then
        fn_name = vim.g.completion_customize_lsp_label[sym.kind] .. ' ' .. fn_name
      end

      vim.b.lsp_current_function = fn_name
      vim.api.nvim_command('doautocmd User LspStatusUpdate')
      return
    end
  end
end

local function update_current_function()
  local params = { textDocument = lsp_util.make_text_document_params() }
  vim.lsp.buf_request(0, 'textDocument/documentSymbol', params, current_function_callback)
end

local function get_all_diagnostics()
  local result = {}
  local levels = {
    errors = 'Error',
    warnings = 'Warning',
    info = 'Information',
    hints = 'Hint'
  }

  for k, level in pairs(levels) do
    result[k] = vim.lsp.util.buf_diagnostics_count(level)
  end

  return result
end

local function get_messages()
  local buf = vim.fn.bufnr()
  if not messages[buf] then
    return {}
  end

  local buf_clients = messages[buf]
  local buf_messages = {}
  local msg_remove = {}
  local progress_remove = {}
  for client, msgs in pairs(buf_clients) do
    for i, msg in ipairs(msgs) do
      if msg.show_once then
        table.insert(msg_remove, { client = client, idx = i })
      end

      table.insert(buf_messages, { name = client, content = msg.content })
    end

    local progress_contexts = buf_clients[client].progress
    if progress_contexts then
      for token, ctx in pairs(progress_contexts) do
        table.insert(buf_messages, { name = client,
          title = ctx.title,
          message = ctx.message,
          percentage = ctx.percentage,
          progress = true,
          spinner = ctx.spinner,
        })

        if ctx.done then
          table.insert(progress_remove, { client = client, token = token })
        end
      end
    end
  end

  for _, item in ipairs(msg_remove) do
    table.remove(messages[buf][item.client], item.idx)
  end

  for _, item in ipairs(progress_remove) do
    messages[buf][item.client].progress[item.token] = nil
  end

  return buf_messages
end

local function generate_statusline()
  if #vim.lsp.buf_get_clients() == 0 then
    return ''
  end

  local diagnostics = get_all_diagnostics()
  local buf_messages = get_messages()
  local only_hint = true
  local some_diagnostics = false
  local status_parts = {}
  if diagnostics.errors and diagnostics.errors > 0 then
    table.insert(status_parts, vim.g.indicator_errors .. ' ' .. diagnostics.errors)
    only_hint = false
    some_diagnostics = true
  end

  if diagnostics.warnings and diagnostics.warnings > 0 then
    table.insert(status_parts, vim.g.indicator_warnings .. ' ' .. diagnostics.warnings)
    only_hint = false
    some_diagnostics = true
  end

  if diagnostics.info and diagnostics.info > 0 then
    table.insert(status_parts, vim.g.indicator_info .. ' ' .. diagnostics.info)
    only_hint = false
    some_diagnostics = true
  end

  if diagnostics.hints and diagnostics.hints > 0 then
    table.insert(status_parts, vim.g.indicator_hint .. ' ' .. diagnostics.hints)
    some_diagnostics = true
  end

  local msgs = {}
  for _, msg in ipairs(buf_messages) do
    local name = aliases[msg.name] or msg.name
    local client_name = '[' .. name .. ']'
    if msg.progress then
      local contents = msg.title
      if msg.message then
        contents = contents .. ' ' .. msg.message
      end

      if msg.percentage then
        contents = contents .. ' (' .. msg.percentage .. ')'
      end

      if msg.spinner then
        contents = vim.g.spinner_frames[(msg.spinner % #vim.g.spinner_frames) + 1] .. ' ' .. contents
      end

      table.insert(msgs, client_name .. ' ' .. contents)
    else
      table.insert(msgs, client_name .. ' ' .. msg.content)
    end
  end

  local base_status = vim.trim(table.concat(status_parts, ' ') .. ' ' .. table.concat(msgs, ' '))
  local symbol = ' 🇻' .. ((some_diagnostics and only_hint) and '' or ' ')
  local current_function = vim.b.lsp_current_function
  if current_function and current_function ~= '' then
    symbol = symbol .. '(' .. current_function .. ') '
  end

  if base_status ~= '' then
    return symbol .. base_status .. ' '
  end

  return symbol .. vim.g.indicator_ok .. ' '
end

local function loading_message(bufnr, client_name, msg)
  if not messages[bufnr] or not messages[bufnr][client_name] then
    register_client(client_name, bufnr)
  end

  table.insert(messages[bufnr][client_name], { content = msg, loading = true })
  return messages[bufnr][client_name][#messages[bufnr][client_name]]
end

local M = {
  update_current_function = update_current_function,
  diagnostics = get_all_diagnostics,
  messages = get_messages,
  register_progress = register_progress,
  register_client = register_client,
  extension_callbacks = extension_callbacks,
  statusline = generate_statusline,
  loading_message = loading_message
}

return M

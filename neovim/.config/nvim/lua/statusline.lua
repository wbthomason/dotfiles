local lsp_status = require('lsp-status')
local aliases = {
  pyls_ms = 'MPLS',
}

local function statusline_lsp()
  if #vim.lsp.buf_get_clients() == 0 then
    return ''
  end

  local diagnostics = lsp_status.diagnostics()
  local buf_messages = lsp_status.messages()
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

local M = {
  lsp = statusline_lsp
}

return M


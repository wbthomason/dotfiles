local get_mode = vim.api.nvim_get_mode
local get_icon_color = require('nvim-web-devicons').get_icon_color
local get_current_win = vim.api.nvim_get_current_win

local bg_color = '#222222'
local fg_color = '#e9e9e9'

local function setup_colors()
  if vim.g.colors_name ~= 'nazgul' then
    local statusline_hl = vim.api.nvim_get_hl(0, { name = 'Statusline', link = false })
    bg_color = statusline_hl.bg
    fg_color = statusline_hl.fg
  end

  local set_hl = vim.api.nvim_set_hl
  set_hl(0, 'Statusline', { fg = fg_color, bg = bg_color })
  set_hl(0, 'StatuslineSeparator', { fg = bg_color })
  set_hl(0, 'StatuslineNormal', { bg = bg_color, fg = fg_color })
  set_hl(0, 'StatuslineVC', { bg = bg_color, fg = '#a9a9a9' })
  set_hl(0, 'StatuslineNormalAccent', { bg = '#403834', bold = true, fg = fg_color })
  set_hl(0, 'StatuslineInsertAccent', { fg = fg_color, bold = true, bg = '#726b67' })
  set_hl(0, 'StatuslineReplaceAccent', { fg = fg_color, bold = true, bg = '#afaf00' })
  set_hl(0, 'StatuslineConfirmAccent', { fg = fg_color, bold = true, bg = '#83adad' })
  set_hl(0, 'StatuslineTerminalAccent', { fg = fg_color, bold = true, bg = '#6f6f6f' })
  set_hl(0, 'StatuslineMiscAccent', { fg = fg_color, bold = true, bg = '#948d89' })
  set_hl(0, 'StatuslineLSPInfo', { fg = '#a9a9a9', bold = true, bg = bg_color })

  -- Diagnostics
  local error_hl = vim.api.nvim_get_hl(0, { name = 'DiagnosticError', link = false })
  set_hl(0, 'StatuslineDiagnosticError', { fg = error_hl.fg, bg = bg_color })
  local warning_hl = vim.api.nvim_get_hl(0, { name = 'DiagnosticWarning', link = false })
  set_hl(0, 'StatuslineDiagnosticWarning', { fg = warning_hl.fg, bg = bg_color })
  local info_hl = vim.api.nvim_get_hl(0, { name = 'DiagnosticInfo', link = false })
  set_hl(0, 'StatuslineDiagnosticInfo', { fg = info_hl.fg, bg = bg_color })
  local hint_hl = vim.api.nvim_get_hl(0, { name = 'DiagnosticHint', link = false })
  set_hl(0, 'StatuslineDiagnosticHint', { fg = hint_hl.fg, bg = bg_color })
end

vim.api.nvim_create_autocmd('ColorScheme', { pattern = '*', callback = setup_colors })
setup_colors()

local function vcs()
  local branch_sign = ''
  local git_info = vim.b.gitsigns_status_dict
  if not git_info or git_info.head == '' then
    return '[ no vcs ]'
  end
  local added = git_info.added and ('+' .. git_info.added .. ' ') or ''
  local modified = git_info.changed and ('~' .. git_info.changed .. ' ') or ''
  local removed = git_info.removed and ('-' .. git_info.removed .. ' ') or ''
  local pad = ((added ~= '') or (removed ~= '') or (modified ~= '')) and ' ' or ''
  local diff_str = string.format('%s%s%s%s', added, removed, modified, pad)
  return string.format('%s%s %s ', diff_str, branch_sign, git_info.head)
end

local mode_table = {
  n = 'Normal',
  no = 'N·Operator Pending',
  v = 'Visual',
  V = 'V·Line',
  ['^V'] = 'V·Block',
  s = 'Select',
  S = 'S·Line',
  ['^S'] = 'S·Block',
  i = 'Insert',
  ic = 'Insert',
  R = 'Replace',
  Rv = 'V·Replace',
  c = 'Command',
  cv = 'Vim Ex',
  ce = 'Ex',
  r = 'Prompt',
  rm = 'More',
  ['r?'] = 'Confirm',
  ['!'] = 'Shell',
  t = 'Terminal',
}

local function mode_name(mode)
  return string.upper(mode_table[mode] or 'V-Block')
end

local function update_colors(mode)
  local mode_color = 'StatuslineMiscAccent'
  if mode == 'n' then
    mode_color = 'StatuslineNormalAccent'
  elseif mode == 'i' or mode == 'ic' then
    mode_color = 'StatuslineInsertAccent'
  elseif mode == 'R' then
    mode_color = 'StatuslineReplaceAccent'
  elseif mode == 'c' then
    mode_color = 'StatuslineConfirmAccent'
  elseif mode == 't' then
    mode_color = 'StatuslineTerminalAccent'
  else
    mode_color = 'StatuslineMiscAccent'
  end

  return mode_color
end

local function get_paste()
  return vim.o.paste and 'PASTE ' or ''
end

local function get_readonly_space()
  return ((vim.o.paste and vim.bo.readonly) and ' ' or '') and '%r' .. (vim.bo.readonly and ' ' or '')
end

-- Copied from @akinsho's config
local error_icon = '' -- '✗'
local warning_icon = ''
local info_icon = '' --  
local hint_icon = '⚑' --  ⚑
local function diagnostics()
  local errors = #vim.diagnostic.get(0, { severity = vim.diagnostic.severity.ERROR })
  local warnings = #vim.diagnostic.get(0, { severity = vim.diagnostic.severity.WARN })
  local hints = #vim.diagnostic.get(0, { severity = vim.diagnostic.severity.HINT })
  local info = #vim.diagnostic.get(0, { severity = vim.diagnostic.severity.INFO })
  local components = {}
  if errors > 0 then
    components[#components + 1] = '%#StatuslineDiagnosticError#' .. error_icon .. ' ' .. errors
  end

  if warnings > 0 then
    components[#components + 1] = '%#StatuslineDiagnosticWarning#' .. warning_icon .. ' ' .. warnings
  end

  if hints > 0 then
    components[#components + 1] = '%#StatuslineDiagnosticHint#' .. hint_icon .. ' ' .. hints
  end

  if info > 0 then
    components[#components + 1] = '%#StatuslineDiagnosticInfo#' .. info_icon .. ' ' .. info
  end

  return table.concat(components, ' ')
end

local function lsp_servers()
  local names = {}
  for _, server in pairs(vim.lsp.get_active_clients { bufnr = 0 }) do
    table.insert(names, server.name)
  end
  if #names == 0 then
    return ''
  end

  return '[ ' .. table.concat(names, ' ') .. ' ]'
end

local function filetype_icon()
  local fname = vim.fn.fnamemodify(vim.api.nvim_buf_get_name(0), ':t')
  local ft = vim.bo.filetype
  local icon, color = get_icon_color(fname, vim.fn.fnamemodify(fname, ':e'), { default = true })
  vim.api.nvim_set_hl(0, 'StatuslineFiletype', { fg = color, bg = bg_color })
  return icon, ft
end

local statusline_format =
  '%%#%s# %s %%<%%#StatuslineFilenameNoMod# %s%s%%<%%=%%#StatuslineFiletype#%s%%#Statusline#%s%s%s%s%%=%%#StatuslineVC#%s'

local statuslines = {}
local function status()
  setup_colors()
  local win_id = vim.g.statusline_winid
  if win_id == get_current_win() or statuslines[win_id] == nil then
    local mode = get_mode().mode
    local mode_color = update_colors(mode)
    local ft_icon, ft_name = filetype_icon()
    local lsp_info = lsp_servers()
    statuslines[win_id] = string.format(
      statusline_format,
      mode_color,
      mode_name(mode),
      get_paste(),
      get_readonly_space(),
      ft_icon,
      (ft_name ~= '') and (' ' .. ft_name) or '',
      (ft_name ~= '' and lsp_info ~= '') and ' · ' or '',
      lsp_info,
      (lsp_info ~= '') and ' ' or '',
      vcs()
    )
  end

  return statuslines[win_id]
end

return { status = status }

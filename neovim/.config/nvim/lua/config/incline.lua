local get_icon = require('nvim-web-devicons').get_icon
local f_mod = vim.fn.fnamemodify
local fmt = string.format
local get_window_width = vim.api.nvim_win_get_width
local pathshorten = vim.fn.pathshorten
local buf_get_name = vim.api.nvim_buf_get_name
local buf_get_option = vim.api.nvim_buf_get_option

local function filename(buf_name, win_id)
  local base_name = f_mod(buf_name, [[:~:.]])
  local space = math.min(50, math.floor(0.4 * get_window_width(win_id)))
  if string.len(base_name) <= space then
    return base_name
  else
    return pathshorten(base_name)
  end
end

local diagnostic_indicators = {
  [vim.diagnostic.severity.ERROR] = '',
  [vim.diagnostic.severity.WARN] = '',
  [vim.diagnostic.severity.INFO] = '🛈',
  [vim.diagnostic.severity.HINT] = '❗',
  ok = '  ',
}

local function render_diagnostics(bufnr)
  local raw_diagnostics = vim.diagnostic.get(bufnr)
  if #raw_diagnostics == 0 then
    return diagnostic_indicators.ok
  end

  local diagnostic_counts = {
    [vim.diagnostic.severity.ERROR] = 0,
    [vim.diagnostic.severity.WARN] = 0,
    [vim.diagnostic.severity.INFO] = 0,
    [vim.diagnostic.severity.HINT] = 0,
  }

  for i = 1, #raw_diagnostics do
    diagnostic_counts[raw_diagnostics[i].severity] = diagnostic_counts[raw_diagnostics[i].severity] + 1
  end

  local result = ''
  for level, count in pairs(diagnostic_counts) do
    if count > 0 then
      result = string.format('%s %s %d', result, diagnostic_indicators[level], count)
    end
  end

  return result
end

local base_bg = '#222222'
local base_fg = '#e9e9e9'
local mod_fg = '#b55f5f'

local function render_label(props)
  local buf_name = buf_get_name(props.buf)
  local icon = get_icon(buf_name, f_mod(buf_name, ':e'), { default = true })
  local diagnostics = render_diagnostics(props.buf)
  local bg = base_bg
  local fg = base_fg
  local gui = nil
  if not props.focused then
    gui = 'italic'
  end

  if props.focused then
    bg = base_fg
    fg = base_bg
  end

  if buf_get_option(props.buf, 'modified') then
    fg = mod_fg
    gui = gui and (gui .. ',bold') or 'bold'
  end

  local has_diagnostics = diagnostics ~= diagnostic_indicators.ok
  local shortened_name = filename(buf_name, props.win)
  return {
    fmt(' %s %s %s%s', icon, shortened_name, diagnostics, has_diagnostics and ' ' or ''),
    gui = gui,
    guibg = bg,
    guifg = fg,
  }
end

require('incline').setup {
  render = render_label,
  window = { zindex = 60, width = 'fit', winhighlight = { Normal = 'Normal' } },
  hide = { cursorline = 'focused_win', focused_win = false },
  ignore = { floating_wins = true, unlisted_buffers = true, buftypes = 'special', wintypes = 'special' },
}

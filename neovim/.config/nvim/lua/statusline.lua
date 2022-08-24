local devicons = require 'nvim-web-devicons'

local get_mode = vim.api.nvim_get_mode
local get_current_win = vim.api.nvim_get_current_win
local get_window_buf = vim.api.nvim_win_get_buf
local buf_get_name = vim.api.nvim_buf_get_name
local fnamemodify = vim.fn.fnamemodify
local get_window_width = vim.api.nvim_win_get_width
local pathshorten = vim.fn.pathshorten

local diagnostic_indicators = {
  [vim.diagnostic.severity.ERROR] = '',
  [vim.diagnostic.severity.WARN] = '',
  [vim.diagnostic.severity.INFO] = '🛈',
  [vim.diagnostic.severity.HINT] = '❗',
  ok = '  ',
}

local function icon(path)
  local name = fnamemodify(path, ':t')
  local extension = fnamemodify(path, ':e')
  return devicons.get_icon(name, extension, { default = true })
end

local function vcs()
  local branch_sign = ''
  local git_info = vim.b.gitsigns_status_dict
  if not git_info or git_info.head == '' then
    return ''
  end
  local added = git_info.added and ('+' .. git_info.added .. ' ') or ''
  local modified = git_info.changed and ('~' .. git_info.changed .. ' ') or ''
  local removed = git_info.removed and ('-' .. git_info.removed .. ' ') or ''
  local pad = ((added ~= '') or (removed ~= '') or (modified ~= '')) and ' ' or ''
  local diff_str = string.format('%s%s%s%s', added, removed, modified, pad)
  return string.format('%s%s %s ', diff_str, branch_sign, git_info.head)
end

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

  return result .. ' '
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

local function filename(buf_name, win_id)
  local base_name = fnamemodify(buf_name, [[:~:.]])
  local space = math.min(50, math.floor(0.4 * get_window_width(win_id)))
  if string.len(base_name) <= space then
    return base_name
  else
    return pathshorten(base_name)
  end
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

  local filename_color
  if vim.bo.modified then
    filename_color = 'StatuslineFilenameModified'
  else
    filename_color = 'StatuslineFilenameNoMod'
  end

  return mode_color, filename_color
end

local function set_modified_symbol(modified)
  local statusline_bg = vim.fn.synIDattr(vim.fn.synIDtrans(vim.fn.hlID 'Statusline'), 'bg#', 'gui')
  if modified then
    vim.cmd([[hi StatuslineModified guibg=]] .. statusline_bg .. [[ gui=bold guifg=#d75f5f]])
    return '  ●'
  else
    vim.cmd([[ hi StatuslineModified guibg=]] .. statusline_bg .. [[ gui=bold guifg=#afaf00]])
    return ''
  end
end

local function get_paste()
  return vim.o.paste and 'PASTE ' or ''
end

local function get_readonly_space()
  return ((vim.o.paste and vim.bo.readonly) and ' ' or '') and '%r' .. (vim.bo.readonly and ' ' or '')
end

local statusline_format =
  '%%#%s# %s %%#StatuslineFiletype# %s%%#StatuslineModified#%s%%#%s# %s%s%%<%%#%s# %s%s%%<%%=%%#StatuslineVC#%s %%#StatuslineLint#%s%%#StatuslineFiletype#'

local statuslines = {}
local function status()
  local win_id = vim.g.statusline_winid
  if win_id == get_current_win() or statuslines[win_id] == nil then
    local mode = get_mode().mode
    local buf_nr = get_window_buf(win_id)
    local bufname = buf_get_name(buf_nr)
    local filename_segment = filename(bufname, win_id)
    local mode_color, filename_color = update_colors(mode)
    local line_col_segment = filename_segment ~= '' and ' %#StatuslineLineCol#| %l:%#StatuslineLineCol#%c ' or ''
    statuslines[win_id] = string.format(
      statusline_format,
      mode_color,
      mode_name(mode),
      icon(bufname),
      set_modified_symbol(vim.bo.modified),
      filename_color,
      filename_segment,
      line_col_segment,
      filename_color,
      get_paste(),
      get_readonly_space(),
      vcs(),
      render_diagnostics(buf_nr)
    )
  else
    -- print(vim.g.statusline_winid, win_getid(winnr()))
  end

  return statuslines[win_id]
end

return { status = status }

local utils = require('utils')
local git = require('git')
local lsp_status = require('lsp-status')

local indicators = {
  checking = '',
  warnings = '',
  errors = '',
  ok = '',
  info = '🛈',
  hint = '❗'
}

local spinner_frames = {'⣾', '⣽', '⣻', '⢿', '⡿', '⣟', '⣯', '⣷'}

local function icon() return utils.icons.lookup_filetype(vim.bo.filetype) end

local function filetype()
  local ft = vim.bo.filetype
  return ft ~= '' and ft or 'no filetype'
end

local ale = {
  warnings = function(counts)
    local all_errors = counts['error'] + counts.style_error
    local all_non_errors = counts.total - all_errors
    return all_non_errors == 0 and '' or string.format('%s %d', indicators.warnings, all_non_errors)
  end,
  errors = function(counts)
    local all_errors = counts['error'] + counts.style_error
    return all_errors == 0 and '' or string.format('%s %d', indicators.errors, all_errors)
  end,
  ok = function(counts) return counts.total == 0 and indicators.ok or '' end,
  counts = function(buf) return vim.fn['ale#statusline#Count'](buf) end,
  enabled = function(buf)
    local ale_linted = vim.fn.getbufvar(buf, 'ale_linted', false)
    return ale_linted and ale_linted > 0
  end,
  frame_idx = 1,
  icon = ' 🍺 '
}

ale.checking = function(buf)
  local result = ''
  if vim.fn['ale#engine#IsCheckingBuffer'](buf) ~= 0 then
    result = spinner_frames[ale.frame_idx % #spinner_frames]
    ale.frame_idx = ale.frame_idx + 1
  else
    ale.frame_idx = 1
  end

  return result
end

setmetatable(ale, {
  __call = function(ale_tbl, buf)
    if not ale_tbl.enabled(buf) then return '' end

    local checking = ale_tbl.checking(buf)
    if checking ~= '' then return string.format('%s%s ', ale_tbl.icon, checking) end

    local counts = ale_tbl.counts(buf)
    local ok = ale_tbl.ok(counts)
    if ok ~= '' then return string.format('%s%s ', ale_tbl.icon, ok) end

    local warnings = ale_tbl.warnings(counts)
    local errors = ale_tbl.errors(counts)
    return string.format('%s%s%s%s ', ale_tbl.icon, warnings,
                         warnings == '' and '' or (errors == '' and '' or ' '), errors)
  end
})

local function vcs(path)
  local branch_sign = ''
  local git_info = git.info(path)
  if not git_info or git_info.branch == '' then return '' end
  local changes = git_info.stats
  local added = changes.added > 0 and ('+' .. changes.added .. ' ') or ''
  local modified = changes.modified > 0 and ('~' .. changes.modified .. ' ') or ''
  local removed = changes.removed > 0 and ('-' .. changes.removed) or ''
  local pad = ((added ~= '') or (removed ~= '') or (modified ~= '')) and ' ' or ''
  local diff_str = string.format('%s%s%s%s', added, removed, modified, pad)
  return string.format('%s%s %s ', diff_str, branch_sign, git_info.branch)
end

local function lint_lsp(buf)
  local result = ale(buf)
  if #vim.lsp.buf_get_clients(buf) > 0 then result = result .. lsp_status.status() end
  return result
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
  R = 'Replace',
  Rv = 'V·Replace',
  c = 'Command',
  cv = 'Vim Ex',
  ce = 'Ex',
  r = 'Prompt',
  rm = 'More',
  ['r?'] = 'Confirm',
  ['!'] = 'Shell',
  t = 'Terminal'
}

local function get_mode(mode) return string.upper(mode_table[mode] or 'V-Block') end

local function filename(buf_name, win_id)
  local base_name = vim.fn.fnamemodify(buf_name, [[:~:.]])
  local space = math.min(60, math.floor(0.6 * vim.fn.winwidth(win_id)))
  if string.len(base_name) <= space then
    return base_name
  else
    return vim.fn.pathshorten(base_name)
  end
end

local function update_colors(mode)
  if mode == 'n' then
    vim.cmd [[hi StatuslineAccent guibg=#d75f5f gui=bold guifg=#e9e9e9]]
  elseif mode == 'i' then
    vim.cmd [[hi StatuslineAccent guifg=#e9e9e9 gui=bold guibg=#dab997]]
  elseif mode == 'R' then
    vim.cmd [[hi StatuslineAccent guifg=#e9e9e9 gui=bold guibg=#afaf00]]
  elseif mode == 'c' then
    vim.cmd [[hi StatuslineAccent guifg=#e9e9e9 gui=bold guibg=#83adad]]
  elseif mode == 't' then
    vim.cmd [[hi StatuslineAccent guifg=#e9e9e9 gui=bold guibg=#6f6f6f]]
  else
    vim.cmd [[hi StatuslineAccent guifg=#e9e9e9 gui=bold guibg=#f485dd]]
  end

  if vim.bo.modified then
    vim.cmd [[hi StatuslineFilename guifg=#d75f5f gui=bold guibg=#3a3a3a]]
  else
    vim.cmd [[hi StatuslineFilename guifg=#e9e9e9 gui=bold guibg=#3a3a3a]]
  end
end

local function set_modified_symbol(modified)
  if modified then
    vim.cmd [[hi StatuslineModified guibg=#3a3a3a gui=bold guifg=#d75f5f]]
    return '  ●'
  else
    vim.cmd [[ hi StatuslineModified guibg=#3a3a3a gui=bold guifg=#afaf00]]
    return ''
  end
end

local function get_paste() return vim.o.paste and 'PASTE ' or '' end

local function get_readonly_space()
  return ((vim.o.paste and vim.bo.readonly) and ' ' or '') and '%r'
           .. (vim.bo.readonly and ' ' or '')
end

local function status()
  local mode = vim.fn.mode()
  local buf_nr = vim.fn.bufnr()
  local buf_name = vim.fn.bufname()
  local buf_path = vim.fn.resolve(vim.fn.fnamemodify(buf_name, ':p'))
  local win_id = vim.fn.win_getid()

  update_colors(mode)
  local line_components = {}
  table.insert(line_components, '%#StatuslineAccent# ' .. get_mode(mode) .. ' ')
  table.insert(line_components, '%#StatuslineFiletype# ' .. icon())
  table.insert(line_components, '%#StatuslineModified#' .. set_modified_symbol(vim.bo.modified))
  table.insert(line_components, '%#StatuslineFilename# ' .. filename(buf_name, win_id) .. ' %<')
  table.insert(line_components, '%#StatuslineFilename# ' .. get_paste())
  table.insert(line_components, get_readonly_space())
  table.insert(line_components, '%#StatuslineLineCol#(Ln %l/%L, %#StatuslineLineCol#Col %c) %<')
  table.insert(line_components, '%=')
  table.insert(line_components, '%#StatuslineVC#' .. vcs(buf_path) .. ' ')
  table.insert(line_components, '%#StatuslineLint#' .. lint_lsp(buf_nr) .. '%#StatuslineFiletype#')
  return table.concat(line_components, '')
end

return status

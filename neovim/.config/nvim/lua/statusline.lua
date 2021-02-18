local lsp_status = require('lsp-status')
local devicons = require('nvim-web-devicons')

-- vim.cmd [[augroup statusline_updates]]
-- vim.cmd [[au!]]
-- vim.cmd [[au BufWinEnter,WinEnter,BufEnter,BufDelete,SessionLoadPost,FileChangedShellPost * lua require('statusline').update()]]
-- vim.cmd [[augroup END]]

local mode_fn = vim.fn.mode
local win_getid = vim.fn.win_getid
local winbufnr_fn = vim.fn.winbufnr
local bufname_fn = vim.fn.bufname
local fnamemod_fn = vim.fn.fnamemodify
local winwidth_fn = vim.fn.winwidth
local pathshorten_fn = vim.fn.pathshorten

local function icon(path)
  local name = fnamemod_fn(path, ':t')
  local extension = fnamemod_fn(path, ':e')
  return devicons.get_icon(name, extension, {default = true})
end

local function vcs()
  local branch_sign = ''
  local git_info = vim.b.gitsigns_status_dict
  if not git_info or git_info.head == '' then return '' end
  local added = git_info.added and ('+' .. git_info.added .. ' ') or ''
  local modified = git_info.changed and ('~' .. git_info.changed .. ' ') or ''
  local removed = git_info.removed and ('-' .. git_info.removed .. ' ') or ''
  local pad = ((added ~= '') or (removed ~= '') or (modified ~= '')) and ' ' or ''
  local diff_str = string.format('%s%s%s%s', added, removed, modified, pad)
  return string.format('%s%s %s ', diff_str, branch_sign, git_info.head)
end

local function lint_lsp(buf)
  local result = ''
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
  local base_name = fnamemod_fn(buf_name, [[:~:.]])
  local space = math.min(50, math.floor(0.4 * winwidth_fn(win_id)))
  if string.len(base_name) <= space then
    return base_name
  else
    return pathshorten_fn(base_name)
  end
end

vim.cmd [[hi StatuslineNormalAccent guibg=#d75f5f gui=bold guifg=#e9e9e9]]
vim.cmd [[hi StatuslineInsertAccent guifg=#e9e9e9 gui=bold guibg=#dab997]]
vim.cmd [[hi StatuslineReplaceAccent guifg=#e9e9e9 gui=bold guibg=#afaf00]]
vim.cmd [[hi StatuslineConfirmAccent guifg=#e9e9e9 gui=bold guibg=#83adad]]
vim.cmd [[hi StatuslineTerminalAccent guifg=#e9e9e9 gui=bold guibg=#6f6f6f]]
vim.cmd [[hi StatuslineMiscAccent guifg=#e9e9e9 gui=bold guibg=#f485dd]]
vim.cmd [[hi StatuslineFilenameModified guifg=#d75f5f gui=bold guibg=#3a3a3a]]
vim.cmd [[hi StatuslineFilenameNoMod guifg=#e9e9e9 gui=bold guibg=#3a3a3a]]

local function update_colors(mode)
  local mode_color = 'StatuslineMiscAccent'
  if mode == 'n' then
    mode_color = 'StatuslineNormalAccent'
  elseif mode == 'i' then
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
  return ((vim.o.paste and vim.bo.readonly) and ' ' or '') and '%r' ..
           (vim.bo.readonly and ' ' or '')
end

local statusline_format =
  '%%#%s# %s %%#StatuslineFiletype# %s%%#StatuslineModified#%s%%#%s# %s%s%%<%%#%s# %s%s%%<%%=%%#StatuslineVC#%s %%#StatuslineLint#%s%%#StatuslineFiletype#'
local function status(win_num)
  local mode = mode_fn()
  local win_id = win_getid(win_num)
  local buf_nr = winbufnr_fn(win_id)
  local bufname = bufname_fn(buf_nr)
  local filename_segment = filename(bufname, win_id)
  local mode_color, filename_color = update_colors(mode)
  local line_col_segment = filename_segment ~= '' and
                             ' %#StatuslineLineCol#| %l:%#StatuslineLineCol#%c ' or ''
  return string.format(statusline_format, mode_color, get_mode(mode), icon(bufname),
                       set_modified_symbol(vim.bo.modified), filename_color, filename_segment,
                       line_col_segment, filename_color, get_paste(), get_readonly_space(), vcs(),
                       lint_lsp(buf_nr))
end

local function update() for i = 1, vim.fn.winnr('$') do vim.wo.statusline = status(i) end end

return {status = status, update = update}

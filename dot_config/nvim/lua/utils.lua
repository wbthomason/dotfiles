local icons = require 'nvim-web-devicons'
local get_icon = icons.get_icon
local f_mod = vim.fn.fnamemodify
local f_esc = vim.fn.fnameescape
local pshorten = vim.fn.pathshorten

local function cap_path_length(path)
  if string.len(path) > 50 then
    path = pshorten(path)
  end

  return path
end

local function display_path(path)
  local absolute_path = f_mod(path, ':p')
  local escaped_path = f_esc(absolute_path)
  return get_icon(escaped_path, f_mod(escaped_path, ':e'), { default = true })
    .. ' '
    .. cap_path_length(f_mod(absolute_path, ':~:.'))
end

return {
  cap_path_length = cap_path_length,
  display_path = display_path,
}

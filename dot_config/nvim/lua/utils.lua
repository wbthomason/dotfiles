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

local function display_path(path, include_icon)
  if path == nil then
    return nil
  end

  if include_icon == nil then
    include_icon = true
  end

  local absolute_path = f_mod(path, ':p')
  local escaped_path = f_esc(absolute_path)
  local icon_component = ''
  if include_icon then
    icon_component = get_icon(escaped_path, f_mod(escaped_path, ':e'), { default = true }) .. ' '
  end

  return icon_component .. cap_path_length(f_mod(absolute_path, ':~:.'))
end

local action_icons = {
  ['New file'] = '',
  ['Update plugins'] = '',
  NeoGit = '',
  ['Time startup'] = '⏱',
  Quit = '',
}

local filename_regex = vim.regex [[([^)]*)]]

local function iconify(item)
  if action_icons[item.name] then
    return action_icons[item.name] .. '\t'
  end

  local filename_start, filename_end = filename_regex:match_str(item.name)
  if not filename_start then
    return ''
  end

  local filename = item.name:sub(filename_start + 2, filename_end - 1)
  return get_icon(filename, f_mod(filename, ':e'), { default = true }) .. '\t'
end

return {
  cap_path_length = cap_path_length,
  display_path = display_path,
  iconify = iconify,
  icon_hook = function(content)
    local coords = MiniStarter.content_coords(content, 'item')
    -- Go backwards to avoid conflict when inserting units
    for i = #coords, 1, -1 do
      local l_num, u_num = coords[i].line, coords[i].unit
      local bullet_unit = {
        string = iconify(content[l_num][u_num].item),
        type = 'item_bullet',
        hl = 'MiniStarterItemBullet',
        -- Use `_item` instead of `item` because it is better to be 'private'
        _item = content[l_num][u_num].item,
        _place_cursor = true,
      }
      table.insert(content[l_num], u_num, bullet_unit)
    end

    return content
  end,
}

-- Plugin to show "hover" information in a floating window, like VS Code

local hover = {}
local defaults = {}
defaults.window_settings = {
  relative = 'win',
  focusable = false,
  style = 'minimal'
}

defaults.window_width = 60

local sources = {
  errors = function(position)
    local qf_items = vim.fn.getqflist()
    local loc_items = vim.fn.getloclist(0)
    local items = {}
    vim.list_extend(items, qf_items)
    vim.list_extend(items, loc_items)
    if #items == 0 then
      return nil
    end

    local relevant_items = {}
    for _, item in ipairs(items) do
      if item.bufnr == position.buffer
        and item.valid
        and item.col == position.col
        and item.lnum == position.row then
        table.insert(relevant_items, vim.fn.printf('[%s] %s', item['type'], item.text))
      end
    end

    return relevant_items
  end,
  -- TODO: Customizable git command
  -- TODO: Prettier presentation of results
  blame = function(position)
    local blame_cmd = vim.fn.printf('git blame -L %d,%d -- %s', position.row, position.row, position.filename)
    local result = vim.fn.system(blame_cmd)
    if vim.v.shell_error ~= 0 then
      return nil
    end

    local commit = vim.split(result, ' ')[1]
    local format = '%aN in %h, %ar%n%s'
    local commit_cmd = vim.fn.printf('git show --format="%s" --no-patch %s', format, commit)
    local blame_msg = vim.fn.system(commit_cmd)
    if vim.v.shell_error ~= 0 then
      return nil
    end

    return blame_msg
  end,
  coc = {}
}

defaults.sources = { errors = sources.errors, blame = sources.blame }
hover.sources = defaults.sources

hover.add_source = function(name, source)
  hover.sources[name] = source
end

hover.show_window = function(position, height, width)
  local buffer = vim.api.nvim_create_buf(false, true)
  -- vim.api.nvim_buf_set_option(buffer, 'buftype', 'nofile')
  local max_width = vim.api.nvim_get_option('columns')
  local max_height = vim.api.nvim_get_option('lines')
  width = math.min(width, max_width)
  height = math.min(height, max_height)
  local settings = hover.window_settings or defaults.window_settings
  settings.width = width
  settings.height = height
  settings.row = position.row
  settings.col = position.col
  local window = vim.api.nvim_open_win(buffer, false, settings)
  return { buf = buffer, win = window }
end

hover.format_section = function(name, content)
  return vim.fn.printf('[%s]\n%s', name, content)
end

hover.make_section = function(name, source, position)
  local content = source(position)
  if content then
    return (type(source) == 'table' and source.formatter) and source.formatter(content) or hover.format_section(name, content)
  end

  return nil
end

hover.make_separator = function(width, pad, separator_pattern)
  if pad == nil then
    pad = ' '
  end

  if separator_pattern == nil then
    separator_pattern = '-'
  end

  local reps = math.floor(width / #separator_pattern)
  local padding = width - reps * #separator_pattern
  local separator = string.rep(separator_pattern, reps)
  if pad then
    separator = string.rep(pad, math.floor(padding / 2)) .. separator
    separator = separator .. string.rep(pad, math.ceil(padding / 2))
  end

  return separator
end

hover.setup_window = function(main_buf, window_data)
  local buf_aucmd = 'autocmd CursorMoved <buffer=' .. main_buf .. '> ++once :bwipeout ' .. window_data.buf
  vim.fn.execute(buf_aucmd)
  local win_aucmd = 'autocmd CursorMoved <buffer=' .. main_buf .. '> ++once :call nvim_win_close(' .. window_data.win .. ', v:true)'
  vim.fn.execute(win_aucmd)
end

hover.fill_window = function(content)
  for i, section in ipairs(content) do
    vim.api.nvim_command('normal a' .. section)
  end
end

hover.hover = function()
  -- Get position
  local cursor_pos = vim.fn.getcurpos()
  local col = cursor_pos[3]
  local row = cursor_pos[2]
  local buffer = vim.fn.bufnr(vim.fn.bufname('%'))
  local filename = vim.fn.expand('%:p')
  local position = { row = row, col = col, buffer = buffer, filename = filename}

  -- Get the separator
  local width = hover.window_width or defaults.window_width
  local separator = hover.make_separator(width)

  -- Get each source's content at the position
  -- TODO: Probably want to allow for async sources
  local content = {}
  for name, source in pairs(hover.sources) do
    local source_content = hover.make_section(name, source, position)
    if source_content then
      table.insert(content, source_content)
      table.insert(content, separator)
    end
  end

  if #content > 1 then
    -- Remove the final extraneous separator
    table.remove(content, #content)
  else
    -- No results!
    return
  end

  -- Display the results
  local height = 0
  for _, item in ipairs(content) do
    height = height + math.ceil(#item / width) + 1
  end

  local prev_win = vim.api.nvim_get_current_win()
  local window = hover.show_window(position, height, width)
  vim.api.nvim_set_current_win(window.win)
  hover.fill_window(content)
  vim.api.nvim_win_set_cursor(window.win, {1, 0})
  vim.api.nvim_set_current_win(prev_win)
  hover.setup_window(position.buffer, window)
end

return hover

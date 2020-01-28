-- Plugin to show "hover" information in a floating window, like VS Code

local hover = {}
local defaults = {}
defaults.window_settings = {
  relative = 'win',
  focusable = false,
  style = 'minimal',
  anchor = 'SW'
}

defaults.window_width = 60

local sources = {
  errors = function(position)
    local qf_items = vim.fn.getqflist()
    local loc_items = vim.fn.getloclist(0)
    local items = {}
    vim.list_extend(items, qf_items)
    vim.list_extend(items, loc_items)

    local relevant_items = {}
    for _, item in ipairs(items) do
      if item.bufnr == position.buffer
        and item.valid
        and item.col == position.col
        and item.lnum == position.row then
        table.insert(relevant_items, item.text)
      end
    end

    if #relevant_items == 0 then
      return nil
    end

    return relevant_items
  end,
  -- TODO: Customizable git command
  blame = function(position)
    local blame_cmd = vim.fn.printf('git blame -L %d,%d -- %s', position.row, position.row, position.filename)
    local result = vim.fn.system(blame_cmd)
    if vim.v.shell_error ~= 0 then
      return nil
    end

    local commit = vim.split(result, ' ')[1]
    local format = '%aN in %h, %ar%n%n%s'
    local commit_cmd = vim.fn.printf('git show --format="%s" --no-patch %s', format, commit)
    local blame_msg = vim.fn.system(commit_cmd)
    if vim.v.shell_error ~= 0 then
      return nil
    end

    return vim.split(blame_msg, '\n')
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
  vim.api.nvim_buf_set_option(buffer, 'buftype', 'nofile')
  vim.api.nvim_buf_set_option(buffer, 'textwidth', width)

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

hover.format_section = function(name, content, width)
  local reps = width - (#name + 2)
  local left_reps = math.min(math.floor(0.1 * reps), 5)
  local right_reps = reps - left_reps
  local header = string.rep('━', left_reps) .. '<' .. name .. '>' .. string.rep('━', right_reps)
  return { header, content }
end

hover.make_section = function(name, source, position, width)
  local content = source(position)
  if content then
    return (type(source) == 'table' and source.formatter) and source.formatter(content, width) or hover.format_section(name, content, width)
  end

  return nil
end

hover.setup_window = function(main_buf, window_data)
  local buf_aucmd = 'autocmd CursorMoved <buffer=' .. main_buf .. '> ++once :bwipeout ' .. window_data.buf
  vim.fn.execute(buf_aucmd)
end

hover.fill_window = function(buffer, content)
  vim.api.nvim_buf_set_lines(buffer, 0, 0, false, content)
  vim.api.nvim_command('%normal gqq')
end

hover.hover = function()
  -- Get position
  local cursor_pos = vim.fn.getcurpos()
  local col = cursor_pos[3]
  local row = cursor_pos[2]
  local buffer = vim.fn.bufnr(vim.fn.bufname('%'))
  local filename = vim.fn.expand('%:p')
  local position = { row = row, col = col, buffer = buffer, filename = filename}


  -- Get each source's content at the position
  -- TODO: Probably want to allow for async sources
  local content = {}
  local width = hover.window_width or defaults.window_width
  for name, source in pairs(hover.sources) do
    local source_content = hover.make_section(name, source, position, width)
    if source_content then
      vim.list_extend(content, source_content)
    end
  end

  content = vim.tbl_flatten(content)
  if #content == 0 then
    -- No results!
    return
  end

  -- Display the results
  local height = 0
  for _, item in ipairs(content) do
    height = height + math.ceil(#item / width)
  end

  height = height - 1

  local prev_win = vim.api.nvim_get_current_win()
  local window = hover.show_window(position, height, width)
  vim.api.nvim_set_current_win(window.win)
  vim.api.nvim_command('set winhl=Normal:HoverDisplay')
  hover.fill_window(window.buf, content)
  vim.api.nvim_win_set_cursor(window.win, {1, 0})
  vim.api.nvim_set_current_win(prev_win)
  hover.setup_window(position.buffer, window)
end

return hover

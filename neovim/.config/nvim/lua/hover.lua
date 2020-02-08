-- Plugin to show "hover" information in a floating window, like VS Code

local api = vim.api
local hover = {}
local defaults = {}
defaults.window_settings = {
  relative = 'win',
  focusable = false,
  style = 'minimal',
  anchor = 'SW'
}

defaults.max_window_width = 60

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
  -- TODO: Make async
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
  coc_diagnostics = function(position)
    local items = vim.fn.CocAction('diagnosticList')
    local relevant_items = {}
    for _, item in ipairs(items) do
      local start_pos = item.location.range.start
      local end_pos = item.location.range['end']
      local bufnr = vim.fn.bufnr(item.file)
      if bufnr == position.buffer
        and (start_pos.character <= position.col and position.col <= end_pos.character)
        and (start_pos.line + 1 <= position.row and position.row <= end_pos.line + 1) then
        table.insert(relevant_items, item.message)
      end
    end

    if #relevant_items == 0 then
      return nil
    end

    return relevant_items
  end
}

defaults.sources = { errors = sources.errors, blame = sources.blame, coc_diagnostics = sources.coc_diagnostics }
hover.sources = defaults.sources

hover.add_source = function(name, source)
  hover.sources[name] = source
end

hover.show_window = function(position, height, width)
  local buffer = api.nvim_create_buf(false, true)
  api.nvim_buf_set_option(buffer, 'buftype', 'nofile')
  api.nvim_buf_set_option(buffer, 'textwidth', width - 2)
  api.nvim_buf_set_option(buffer, 'filetype', 'hoverwindow')

  local max_width = api.nvim_get_option('columns')
  local max_height = api.nvim_get_option('lines')
  width = math.min(width, max_width)
  height = math.min(height, max_height)
  local settings = vim.deepcopy(hover.window_settings or defaults.window_settings)
  settings.width = width
  settings.height = height
  settings.bufpos = { position.row - 1, position.col }
  local window = api.nvim_open_win(buffer, false, settings)
  return { buf = buffer, win = window }
end

hover.format_section = function(name, content, width)
  local reps = width - (#name + 2)
  local left_reps = math.min(math.floor(0.1 * reps), 5)
  local right_reps = reps - left_reps
  local header = string.rep('━', left_reps) .. '<' .. name .. '>' .. string.rep('━', right_reps)
  for idx, line in ipairs(content) do
    content[idx] = ' ' .. line
  end

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
  local cmds = {
    vim.fn.printf(
      'autocmd CursorMoved <buffer=%d> ++once :bwipeout %d | autocmd! * <buffer=%d>',
      main_buf,
      window_data.buf,
      main_buf
    ),
    vim.fn.printf(
      'autocmd CursorMovedI <buffer=%d> ++once :bwipeout %d | autocmd! * <buffer=%d>',
      main_buf,
      window_data.buf,
      main_buf
    )
  }

  for _, cmd in ipairs(cmds) do
    vim.fn.execute(cmd)
  end
end

hover.fill_window = function(buffer, content)
  api.nvim_buf_set_lines(buffer, 0, 0, false, content)
  api.nvim_command('%normal gqq')
end

hover.hover = function()
  -- Get position
  local cursor_pos = vim.fn.getcurpos()
  local col = cursor_pos[3]
  local row = cursor_pos[2]
  local buffer = vim.fn.bufnr(vim.fn.bufname('%'))
  local filename = vim.fn.expand('%:p')
  local position = { row = row, col = col, buffer = buffer, filename = filename }

  -- Get each source's content at the position
  -- TODO: Probably want to allow for async sources
  local content = {}
  local width = hover.window_width or defaults.max_window_width
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

  local prev_win = api.nvim_get_current_win()
  local window = hover.show_window(position, height, width)
  api.nvim_set_current_win(window.win)
  api.nvim_command('set winhl=Normal:HoverDisplay')
  hover.fill_window(window.buf, content)
  api.nvim_win_set_cursor(window.win, {1, 0})
  api.nvim_set_current_win(prev_win)
  hover.setup_window(position.buffer, window)
end

return hover

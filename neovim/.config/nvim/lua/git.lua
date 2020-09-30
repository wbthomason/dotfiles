-- Adapted from https://github.com/itchyny/vim-gitbranch and https://github.com/mhinz/vim-signify
local function get_path_dir(path)
  local dir = path
  local prev = ''
  while dir ~= prev do
    local git_dir = dir .. '/.git'
    local dir_info = vim.loop.fs_stat(git_dir)
    if dir_info and dir_info['type'] == 'directory' then
      local obj_dir_info = vim.loop.fs_stat(git_dir .. '/objects')
      if obj_dir_info and obj_dir_info['type'] == 'directory' then
        local refs_dir_info = vim.loop.fs_stat(git_dir .. '/refs')
        if refs_dir_info and refs_dir_info['type'] == 'directory' then
          local head_info = vim.loop.fs_stat(git_dir .. '/HEAD')
          if head_info and head_info.size > 10 then return git_dir end
        end
      end
    elseif dir_info and dir_info['type'] == 'file' then
      local reldir = vim.fn.readfile(git_dir)[1] or ''
      if string.find(reldir, '^gitdir: ') then
        return vim.fn.simplify(dir .. '/' .. string.sub(reldir, 9))
      end
    end

    prev = dir
    dir = vim.fn.fnamemodify(dir, ':h')
  end

  return ''
end

local function detect_branch(path)
  local vim = vim
  local path_dir = get_path_dir(path)
  if path_dir then
    path = path_dir .. '/HEAD'
    if vim.fn.filereadable(path) ~= 0 then return path end
  end
end

local git_info = {}

local function init_git_info(path)
  local buf = vim.fn.bufnr(path)
  if buf < 0 then return false end
  git_info[path] = {
    branch_path = detect_branch(path),
    updating = false,
    disabled = false,
    stats = {added = 0, removed = 0, modified = 0}
  }

  vim.cmd [[augroup mygit]]
  vim.cmd(string.format('au! * <buffer=%d>', buf))
  for _, event in ipairs({
    'BufEnter', 'WinEnter', 'BufWritePost', 'CursorHold', 'CursorHoldI', 'FocusGained',
    'ShellCmdPost', 'VimResume'
  }) do vim.cmd(string.format('au %s <buffer=%d> call git#update_changes("%s")', event, buf, path)) end
  vim.cmd [[augroup END]]
  return true
end

local function get_branch(path)
  if not git_info[path] then if not init_git_info(path) then return end end
  if git_info[path].branch then return git_info[path].branch end
  local branch_path = git_info[path].branch_path
  local result = ''
  if vim.fn.filereadable(branch_path) ~= 0 then
    local branch = vim.fn.readfile(branch_path)[1] or ''
    if string.find(branch, '^ref: ') then
      result = vim.fn.substitute(branch, [[^ref: \%(refs/\%(heads/\|remotes/\|tags/\)\=\)\=]], '',
                                 '')
    elseif vim.fn.match(branch, [[^\x\{20\}]]) >= 0 then
      result = string.sub(branch, 1, 6)
    end
  end

  return result
end

-- TODO: Make this place signs too
local process_diff = vim.schedule_wrap(function(exit_success, path, ctx, output_log, _)
  if ctx.temp_base_file_name then
    vim.loop.fs_close(ctx.temp_base_file_fd)
    vim.loop.fs_unlink(ctx.temp_base_file_name)
  end

  if ctx.temp_buf_file_name then
    vim.loop.fs_close(ctx.temp_buf_file_fd)
    vim.loop.fs_unlink(ctx.temp_buf_file_name)
  end

  if not exit_success then
    git_info[path].updating = false
    git_info[path].disabled = true
    return
  end

  if #output_log == 0 then
    git_info[path].stats = {added = 0, removed = 0, modified = 0}
    git_info[path].updating = false
    return
  end

  local output = {}
  if ctx.split then
    for _, line in ipairs(output_log) do
      for _, chunk in ipairs(vim.split(line, '\n')) do table.insert(output, chunk) end
    end
  else
    output = output_log
  end

  local stats = {added = 0, removed = 0, modified = 0}
  for _, line in ipairs(output) do
    if string.find(line, [[^@@ ]]) then
      local tokens = vim.fn.matchlist(line, [[^@@ -\v(\d+),?(\d*) \+(\d+),?(\d*)]])
      local line_stats = {
        mod_count = tokens[3] == '' and 1 or tonumber(tokens[3]),
        new_count = tokens[5] == '' and 1 or tonumber(tokens[5])
      }

      if line_stats.mod_count == 0 and line_stats.new_count > 0 then
        stats.added = stats.added + line_stats.new_count
      elseif line_stats.mod_count > 0 and line_stats.new_count == 0 then
        stats.removed = stats.removed + line_stats.mod_count
      else
        stats.modified = stats.modified + line_stats.mod_count
      end
    end
  end

  git_info[path].stats = stats
  git_info[path].updating = false
end)

local function make_callback(pipe, err_tbl, output_tbl)
  return function(err, data)
    if err then table.insert(err_tbl, err) end
    if data then
      table.insert(output_tbl, data)
    else
      vim.loop.read_stop(pipe)
      vim.loop.close(pipe)
    end
  end
end

local function update_changes(path)
  if git_info[path] and git_info[path].updating then return git_info[path] end
  if git_info[path] then
    if git_info[path].disabled then return git_info[path] end
    git_info[path].updating = true
  else
    if not init_git_info(path) then return end
    git_info[path].updating = true
  end

  local job
  local ctx = {}
  local buf_file_name = vim.fn.shellescape(vim.fn.fnamemodify(path, ':t'))
  local buf = vim.fn.bufnr(path)
  local buf_file_dir = vim.fn.fnamemodify(path, ':p:h')
  if vim.fn.getbufvar(buf, '&modified', false) == 1 then
    local temp_buf_file_fd, temp_buf_file_name = vim.loop.fs_mkstemp(
                                                   string.format('/tmp/nvim-git-buf%dXXXXXX', buf))
    ctx.temp_buf_file_name = temp_buf_file_name
    ctx.temp_buf_file_fd = temp_buf_file_fd
    local buf_contents = vim.fn.getbufline(buf, 1, '$')
    if #buf_contents >= 1 or (buf_contents[1] and buf_contents[1] ~= '') or vim.fn.line2byte(1) ~= -1 then
      if vim.fn.getbufvar(buf, '&bomb', false) == 1 then
        buf_contents[1] = '' .. buf_contents[1]
      end

      for idx, _ in ipairs(buf_contents) do
        buf_contents[idx] = buf_contents[idx] .. '\n'
      end

      local temp_file = io.open(temp_buf_file_name, 'w')
      temp_file:write(unpack(buf_contents))
      temp_file:close()
    end

    local temp_base_file_fd, temp_base_file_name =
      vim.loop.fs_mkstemp(string.format('/tmp/nvim-git-buf%d-baseXXXXXX', buf))
    ctx.temp_base_file_name = temp_base_file_name
    ctx.temp_base_file_fd = temp_base_file_fd
    temp_base_file_name = vim.fn.fnameescape(temp_base_file_name)
    temp_buf_file_name = vim.fn.fnameescape(temp_buf_file_name)
    job = {
      'sh', '-c',
      string.format('git -C %s show HEAD:./%s > %s && diff -U0 %s %s', buf_file_dir, buf_file_name, temp_base_file_name,
                    temp_base_file_name, temp_buf_file_name)
    }
    ctx.exit_code = {[1] = true, [0] = true}
    ctx.split = true
  else
    job = {
      'sh', '-c',
      string.format('git -C %s --no-pager diff --no-color --no-ext-diff -U0 -- %s', buf_file_dir,
                    buf_file_name)
    }
    ctx.exit_code = {[0] = true}
    ctx.split = true
  end

  local err_log = {}
  local output_log = {}
  local stdout = vim.loop.new_pipe(false)
  local stderr = vim.loop.new_pipe(false)

  local cmd = job[1]
  local options = {args = {unpack(job, 2)}, stdio = {nil, stdout, stderr}}

  local handle = nil
  handle = vim.loop.spawn(cmd, options, function(exit_code, _)
    handle:close()
    local check = vim.loop.new_check()
    vim.loop.check_start(check, function()
      for _, pipe in pairs({stdout, stderr}) do
        if not vim.loop.is_closing(pipe) then return end
      end

      vim.loop.check_stop(check)
      process_diff(ctx.exit_code[exit_code] ~= nil, path, ctx, output_log, err_log)
    end)
  end)

  vim.loop.read_start(stdout, make_callback(stdout, err_log, output_log))
  vim.loop.read_start(stderr, make_callback(stderr, err_log, output_log))
end

local function get_info(path)
  if not git_info[path] then
    if not init_git_info(path) then return nil end
    git_info[path].branch = get_branch(path)
    update_changes(path)
  end

  return git_info[path]
end

local function status(path)
  -- TODO: needs to open a buffer with changes listed, make keybindings for committing,
  -- adding/removing files, undoing changes, viewing files, viewing change hunks, staging parts of
  -- hunks, etc.
  assert(false, 'Not implemented!')
end

local function commit(path) assert(false, 'Not implemented!') end

local function push(path) assert(false, 'Not implemented!') end

local function pull(path) assert(false, 'Not implemented!') end

local M = {
  info = get_info,
  update = update_changes,
  status = status,
  commit = commit,
  push = push,
  pull = pull
}

return M

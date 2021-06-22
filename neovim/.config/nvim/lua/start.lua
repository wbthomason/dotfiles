local counter = 25
local total_paths = 25
local offset = 5

local use_vcs_root = true
local files = {}

local function cap_path_length(path)
  if string.len(path) > 70 then path = vim.fn.pathshorten(path) end
  return path
end

local function relativize(path) return cap_path_length(vim.fn.fnamemodify(path, [[:~:.]])) end
local regex = vim.regex
local path_skip_list = {
  regex('runtime/doc/.*\\.txt'), regex('bundle/.*/doc/.*\\.txt'), regex('plugged/.*/doc/.*\\.txt'),
  regex('/.git/'), regex('fugitiveblame$'),
  regex(vim.fn.escape(vim.fn.fnamemodify(vim.fn.resolve(os.getenv('VIMRUNTIME')), ':p'), '\\') ..
          'doc/.*\\.txt')
}

local function skip(path)
  local n = #path_skip_list
  for i = 1, n do if path_skip_list[i]:match_str(path) then return true end end
  return false
end

local function filter_oldfiles(prefix, fmt)
  prefix = regex('\\V' .. vim.fn.escape(prefix, '\\'))
  local is_dir = vim.fn.escape('/', '\\') .. '$'
  local oldfiles = {}
  for _, file in ipairs(vim.v.oldfiles) do
    if counter <= 0 then break end
    local absolute_path = vim.fn.glob(vim.fn.fnamemodify(file, ':p'))
    if absolute_path and absolute_path ~= '' and not files[absolute_path] and
      string.match(absolute_path, is_dir) == nil and prefix:match_str(absolute_path) ~= nil and
      not skip(absolute_path) then
      files[absolute_path] = true
      table.insert(oldfiles, {
        key = total_paths - counter,
        cmd = 'edit ' .. vim.fn.fnameescape(absolute_path),
        disp = cap_path_length(vim.fn.fnamemodify(absolute_path, fmt))
      })
      counter = counter - 1
    end
  end

  return oldfiles
end

local function current_dir_files()
  if use_vcs_root then
    local path = vim.fn.fnamemodify(vim.fn.getcwd(), ':p:h')
    local root = vim.fn.finddir('.git', path .. ';')
    if root ~= '' then vim.cmd('lcd ' .. vim.fn.fnameescape(vim.fn.fnamemodify(root, ':h'))) end
  end

  local dir = vim.fn.expand(vim.fn.getcwd())
  return filter_oldfiles(dir, ':~:.')
end

local function recent_files() return filter_oldfiles('', ':p:~') end

local commands = {
  {key = 'u', disp = 'Update plugins', cmd = 'PackerUpdate'},
  {key = 'c', disp = 'Clean plugins', cmd = 'PackerClean'},
  {key = 't', disp = 'Time startup', cmd = 'StartupTime'},
  {key = 's', disp = 'Start Prosession', cmd = 'Prosession .'},
  {key = 'q', disp = 'Quit', cmd = 'q!'}
}

local cur_dir = relativize(vim.fn.expand(vim.fn.getcwd()))
cur_dir = (cur_dir ~= '') and cur_dir or '~'

-- TODO: Maybe make the show functions unevaluated and run async? Would require rewriting using LUV
-- functions, which isn't a bad idea anyway
local sections = {
  {title = 'Commands', show = commands},
  {title = string.format('Recent Files in %s', cur_dir), show = current_dir_files()},
  {title = 'Recent Files', show = recent_files()}
}

local boundaries = {}
local keybindings = {}

local function make_sections()
  boundaries = {}
  keybindings = {}
  local linenr = 0
  local set_lines = vim.api.nvim_buf_set_lines
  local highlight = vim.api.nvim_buf_add_highlight
  for _, section in ipairs(sections) do
    if next(section.show) ~= nil then
      set_lines(0, linenr, linenr, false, {' ' .. section.title})
      highlight(0, -1, 'Title', linenr, 1, -1)
      local size = 1
      for _, item in ipairs(section.show) do
        local key = item.key
        local key_len
        if type(key) == 'string' then
          key_len = string.len(key)
        else
          key_len = (key < 10) and 1 or 2
        end

        local padding = (key_len == 1) and '  ' or ' '
        set_lines(0, linenr + size, linenr + size, false,
                  {'   [' .. key .. ']' .. padding .. item.disp})
        highlight(0, -1, 'StartifyBracket', linenr + size, 3, 4)
        highlight(0, -1, 'StartifyNumber', linenr + size, 4, 4 + key_len)
        highlight(0, -1, 'StartifyBracket', linenr + size, 4 + key_len, 5 + key_len)
        highlight(0, -1, 'StartifyPath', linenr + size, 5 + key_len, -1)
        table.insert(keybindings, {key = key, cmd = item.cmd})
        size = size + 1
      end

      set_lines(0, -1, -1, false, {''})
      table.insert(boundaries, {linenr + 1, linenr + size})
      linenr = linenr + size + 1
    end
  end

  return keybindings
end

local function move_cursor(d)
  local curr_line = vim.fn.line('.')
  for idx, range in ipairs(boundaries) do
    if range[1] <= curr_line and range[2] >= curr_line then
      if range[1] >= curr_line + d then
        if idx > 1 then vim.fn.cursor(boundaries[idx - 1][2], offset) end
      elseif range[2] < curr_line + d then
        if idx < #boundaries then vim.fn.cursor(boundaries[idx + 1][1] + 1, offset) end
      else
        vim.fn.cursor(curr_line + d, offset)
      end

      return
    end
  end
end

local function handle_j() move_cursor(1) end

local function handle_k() move_cursor(-1) end

local function handle_cr()
  local line_num = vim.fn.line('.')
  local curr_line = vim.api.nvim_buf_get_lines(0, line_num - 1, line_num, false)[1]
  local key = string.match(curr_line, '%[([^%]]*)%]')
  for _, binding in ipairs(keybindings) do
    if binding.key == key then
      vim.cmd(binding.cmd)
      return
    end
  end
end

local function setup_keys()
  -- First, the nav keys
  local map = vim.api.nvim_buf_set_keymap
  map(0, 'n', 'h', '<NOP>', {noremap = true, silent = true})
  map(0, 'n', 'l', '<NOP>', {noremap = true, silent = true})
  map(0, 'n', 'j', '<cmd>lua require"start".handle_j()<cr>', {noremap = true, silent = true})
  map(0, 'n', 'k', '<cmd>lua require"start".handle_k()<cr>', {noremap = true, silent = true})
  map(0, 'n', '<cr>', '<cmd>lua require"start".handle_cr()<cr>', {noremap = true, silent = true})

  -- Then, the defined keybindings
  for _, binding in ipairs(keybindings) do
    map(0, 'n', tostring(binding.key), '<cmd>:' .. binding.cmd .. '<cr>',
        {noremap = true, silent = true})
  end
end

local function start_screen()
  if vim.fn.argc() ~= 0 or vim.fn.line2byte('$') ~= -1 or vim.o.insertmode or not vim.o.modifiable then
    return
  end

  vim.cmd [[set eventignore=all]]
  vim.cmd [[noautocmd silent! setlocal bufhidden=wipe colorcolumn= foldcolumn=0 matchpairs= nobuflisted nocursorcolumn nocursorline nolist nonumber norelativenumber nospell noswapfile signcolumn=no synmaxcol& statusline= filetype=startify]]
  make_sections()
  vim.cmd [[noautocmd setlocal nomodifiable nomodified]]
  -- Position cursor
  vim.fn.cursor(2, offset)
  setup_keys()
  vim.cmd [[set eventignore=""]]
end

return {start = start_screen, handle_j = handle_j, handle_k = handle_k, handle_cr = handle_cr}

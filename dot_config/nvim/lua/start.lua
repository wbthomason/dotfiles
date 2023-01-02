-- TODO: Could almost certainly make this a lot faster, especially by using Luv more directly in the
-- MRU logic, making make_sections() construct the full table of strings first and then call
-- set_lines only once (still need to deal with highlights), maybe making file info fill in async
local icons = require 'nvim-web-devicons'

local counter = 15
local offset = 5

local function cap_path_length(path)
  if string.len(path) > 50 then
    path = vim.fn.pathshorten(path)
  end
  return path
end

local regex = vim.regex
local path_skip_list = {
  regex 'runtime/doc/.*\\.txt',
  regex '/.git/',
  regex(vim.fn.escape(vim.fn.fnamemodify(vim.fn.resolve(os.getenv 'VIMRUNTIME'), ':p'), '\\') .. 'doc/.*\\.txt'),
}

local function skip(path)
  local n = #path_skip_list
  for i = 1, n do
    if path_skip_list[i]:match_str(path) then
      return true
    end
  end
  return false
end

local function recent_files()
  local oldfiles = {}
  local f_mod = vim.fn.fnamemodify
  local f_esc = vim.fn.fnameescape
  local get_icon = icons.get_icon
  local f_stat = vim.loop.fs_stat
  local unfiltered_oldfiles = vim.v.oldfiles
  for _, file in ipairs(unfiltered_oldfiles) do
    if #oldfiles >= counter then
      break
    end

    local absolute_path = f_mod(file, ':p')
    local path_info = f_stat(absolute_path)
    if path_info and path_info.type ~= 'directory' and not skip(absolute_path) then
      local escaped_path = f_esc(absolute_path)
      oldfiles[#oldfiles + 1] = {
        key = tostring(#oldfiles),
        cmd = 'edit ' .. escaped_path,
        -- disp = cap_path_length(f_mod(absolute_path, ':~:.')), --get_icon(escaped_path, f_mod(escaped_path, ':e'), { default = true }) .. ' ' .. cap_path_length( f_mod(absolute_path, ':~:.')),
        disp = get_icon(escaped_path, f_mod(escaped_path, ':e'), { default = true }) .. ' ' .. cap_path_length(
          f_mod(absolute_path, ':~:.')
        ),
        editing = true,
      }
    end
  end

  return oldfiles
end

local commands = {
  { key = 'e', disp = '  New file', cmd = 'ene | startinsert', editing = true },
  { key = 'u', disp = '  Update plugins', cmd = 'Lazy sync' },
  { key = 'b', disp = '  File Browser', cmd = 'Telescope file_browser' },
  { key = 'r', disp = '  Recent files', cmd = 'Telescope oldfiles' },
  { key = 's', disp = '  Start Prosession', cmd = 'Prosession .', editing = true },
  { key = 'g', disp = '  NeoGit', cmd = 'Neogit' },
  { key = 't', disp = '⏱  Time startup', cmd = 'StartupTime' },
  { key = 'q', disp = '  Quit', cmd = 'qa' },
}

-- TODO: Maybe make the show functions unevaluated and run async? Would require rewriting using LUV
-- functions, which isn't a bad idea anyway
local sections = {
  { title = 'Commands', show = commands },
  { title = 'Recent Files', show = recent_files() },
}

local boundaries = {}
local keybindings = {}

local function longest_elems()
  local longest_title = 0
  local longest_item = 0
  for _, section in ipairs(sections) do
    local title_len = string.len(section.title)
    if title_len > longest_title then
      longest_title = title_len
    end
    for _, item in ipairs(section.show) do
      local item_len = string.len(item.disp)
      if item_len > longest_item then
        longest_item = item_len
      end
    end
  end

  return longest_title, longest_item
end

local function make_sections()
  boundaries = {}
  keybindings = {}
  local set_lines = vim.api.nvim_buf_set_lines
  local highlight = vim.api.nvim_buf_add_highlight
  local win_width = vim.fn.winwidth(0)
  local linenr = 2
  set_lines(0, 0, 0, false, { '', '' })
  local longest_title, longest_item = longest_elems()
  local title_indent = bit.arshift(win_width - longest_title, 1)
  local section_indent = bit.arshift(win_width - longest_item - 4, 1)
  offset = section_indent + 1
  local section_padding = string.rep(' ', section_indent)
  for _, section in ipairs(sections) do
    if next(section.show) ~= nil then
      local section_title_indent = title_indent + bit.arshift(longest_title - string.len(section.title), 1)
      local title_padding = string.rep(' ', section_title_indent)
      set_lines(0, linenr, linenr, false, { title_padding .. section.title, '' })
      highlight(0, -1, 'SpecialComment', linenr, 1, -1)
      linenr = linenr + 1
      local size = 1
      for _, item in ipairs(section.show) do
        local key = item.key
        local key_len
        if type(key) == 'string' then
          key_len = string.len(key)
        else
          key_len = (key < 10) and 1 or 2
        end

        local key_padding = (key_len == 1) and '  ' or ' '
        set_lines(
          0,
          linenr + size,
          linenr + size,
          false,
          { string.format('%s(%s)%s%s', section_padding, key, key_padding, item.disp) }
        )
        highlight(0, -1, 'StartifyBracket', linenr + size, section_indent, section_indent + 1)
        highlight(0, -1, 'StartifyNumber', linenr + size, section_indent + 1, section_indent + 1 + key_len)
        highlight(0, -1, 'StartifyBracket', linenr + size, section_indent + 1 + key_len, section_indent + 2 + key_len)
        keybindings[#keybindings + 1] = { key = key, cmd = item.cmd, editing = item.editing }
        size = size + 1
      end

      set_lines(0, -1, -1, false, { '', '' })
      boundaries[#boundaries + 1] = { linenr + 1, linenr + size }
      linenr = linenr + size + 2
    end
  end

  return keybindings
end

local function move_cursor(d)
  local curr_line = vim.fn.line '.'
  for idx, range in ipairs(boundaries) do
    if range[1] <= curr_line and range[2] >= curr_line then
      if range[1] >= curr_line + d then
        if idx > 1 then
          vim.fn.cursor(boundaries[idx - 1][2], offset)
        end
      elseif range[2] < curr_line + d then
        if idx < #boundaries then
          vim.fn.cursor(boundaries[idx + 1][1] + 1, offset)
        end
      else
        vim.fn.cursor(curr_line + d, offset)
      end

      return
    end
  end
end

local function handle_j()
  move_cursor(1)
end

local function handle_k()
  move_cursor(-1)
end

local function do_binding(binding)
  if binding.editing then
    vim.cmd [[ doautocmd User ActuallyEditing ]]
  end

  vim.cmd(binding.cmd)
end

local function handle_key(key)
  for _, binding in ipairs(keybindings) do
    if binding.key == key then
      do_binding(binding)
      return
    end
  end
end

local function handle_cr()
  local line_num = vim.fn.line '.'
  local curr_line = vim.api.nvim_buf_get_lines(0, line_num - 1, line_num, false)[1]
  local key = string.match(curr_line, '%(([^%]]*)%)')
  handle_key(key)
end

local function setup_keys()
  -- First, the nav keys
  local map = vim.api.nvim_buf_set_keymap
  map(0, 'n', 'h', '<NOP>', { noremap = true, silent = true })
  map(0, 'n', 'l', '<NOP>', { noremap = true, silent = true })
  map(0, 'n', 'j', '', { noremap = true, silent = true, callback = handle_j })
  map(0, 'n', 'k', '', { noremap = true, silent = true, callback = handle_k })
  map(0, 'n', '<cr>', '', { noremap = true, silent = true, callback = handle_cr })

  -- Then, the defined keybindings
  for _, binding in ipairs(keybindings) do
    map(0, 'n', tostring(binding.key), '', {
      noremap = true,
      silent = true,
      callback = function()
        do_binding(binding)
      end,
    })
  end
end

local function start_screen()
  if vim.fn.argc() ~= 0 or vim.fn.line2byte '$' ~= -1 or vim.o.insertmode or not vim.o.modifiable then
    vim.api.nvim_exec_autocmds('User', { pattern = 'ActuallyEditing' })
    return
  end

  vim.o.eventignore = 'all'
  vim.opt_local.bufhidden = 'wipe'
  vim.opt_local.colorcolumn = ''
  vim.opt_local.foldcolumn = '0'
  vim.opt_local.matchpairs = ''
  vim.opt_local.buflisted = false
  vim.opt_local.cursorcolumn = false
  vim.opt_local.cursorline = false
  vim.opt_local.list = false
  vim.opt_local.number = false
  vim.opt_local.relativenumber = false
  vim.opt_local.spell = false
  vim.opt_local.swapfile = false
  vim.opt_local.signcolumn = 'no'
  vim.opt_local.synmaxcol = 0
  vim.opt_local.statusline = ''
  vim.opt_local.filetype = 'startify'

  -- vim.cmd [[noautocmd silent! setlocal bufhidden=wipe colorcolumn= foldcolumn=0 matchpairs= nobuflisted nocursorcolumn nocursorline nolist nonumber norelativenumber nospell noswapfile signcolumn=no synmaxcol& statusline= filetype=startify]]
  make_sections()
  vim.opt_local.modifiable = false
  vim.opt_local.modified = false
  vim.api.nvim_win_set_cursor(0, { 5, offset })
  setup_keys()
  vim.o.eventignore = ''
end
return { start = start_screen, handle_j = handle_j, handle_k = handle_k, handle_cr = handle_cr }

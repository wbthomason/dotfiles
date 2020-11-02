" Automatically generated packer.nvim plugin loader code

if !has('nvim')
  finish
endif

lua << END
local plugins = {
  ["completion-nvim"] = {
    config = { "\27LJ\1\2·\2\0\0\5\0\f\0\0294\0\0\0%\1\1\0>\0\2\0027\1\2\0%\2\3\0004\3\0\0%\4\3\0>\3\2\0027\3\4\3>\1\3\0014\1\5\0007\1\6\1%\2\a\0>\1\2\0014\1\5\0007\1\6\1%\2\b\0>\1\2\0014\1\5\0007\1\6\1%\2\t\0>\1\2\0017\1\n\0>\1\1\0014\1\5\0007\1\6\1%\2\v\0>\1\2\1G\0\1\0\25 doautoall FileType \14on_attach\18 augroup END : au BufEnter * lua require('completion').on_attach() \25 augroup lsp_aucmds \bcmd\bvim\18complete_item\vvimtex\24addCompletionSource\15completion\frequire\0" },
    loaded = false,
    only_sequence = false,
    only_setup = false,
    path = "/home/wil/.local/share/nvim/site/pack/packer/opt/completion-nvim"
  },
  ["completion-treesitter"] = {
    loaded = false,
    only_sequence = false,
    only_setup = false,
    path = "/home/wil/.local/share/nvim/site/pack/packer/opt/completion-treesitter"
  },
  ["iron.nvim"] = {
    commands = { "IronRepl", "IronWatchCurrentFile", "IronSend" },
    loaded = false,
    only_sequence = false,
    only_setup = false,
    path = "/home/wil/.local/share/nvim/site/pack/packer/opt/iron.nvim"
  },
  ["nvim-dap"] = {
    loaded = false,
    only_sequence = false,
    only_setup = false,
    path = "/home/wil/.local/share/nvim/site/pack/packer/opt/nvim-dap"
  },
  ["nvim-treesitter"] = {
    config = { 'require("treesitter")' },
    loaded = false,
    only_sequence = false,
    only_setup = false,
    path = "/home/wil/.local/share/nvim/site/pack/packer/opt/nvim-treesitter"
  },
  ["packer.nvim"] = {
    loaded = false,
    only_sequence = false,
    only_setup = false,
    path = "/home/wil/.local/share/nvim/site/pack/packer/opt/packer.nvim"
  },
  undotree = {
    commands = { "UndotreeToggle" },
    loaded = false,
    only_sequence = false,
    only_setup = false,
    path = "/home/wil/.local/share/nvim/site/pack/packer/opt/undotree"
  },
  ["vim-dispatch"] = {
    commands = { "Dispatch", "Make", "Focus", "Start" },
    loaded = false,
    only_sequence = false,
    only_setup = false,
    path = "/home/wil/.local/share/nvim/site/pack/packer/opt/vim-dispatch"
  },
  ["vim-enmasse"] = {
    commands = { "EnMasse" },
    loaded = false,
    only_sequence = false,
    only_setup = false,
    path = "/home/wil/.local/share/nvim/site/pack/packer/opt/vim-enmasse"
  },
  ["vim-fugitive"] = {
    commands = { "Gpull", "Gpush", "Gstatus" },
    loaded = false,
    only_sequence = false,
    only_setup = false,
    path = "/home/wil/.local/share/nvim/site/pack/packer/opt/vim-fugitive"
  },
  ["vim-grepper"] = {
    commands = { "Grepper" },
    loaded = false,
    only_sequence = false,
    only_setup = false,
    path = "/home/wil/.local/share/nvim/site/pack/packer/opt/vim-grepper"
  },
  ["vim-matchup"] = {
    loaded = false,
    only_sequence = false,
    only_setup = false,
    path = "/home/wil/.local/share/nvim/site/pack/packer/opt/vim-matchup"
  },
  ["vim-obsession"] = {
    after = { "vim-prosession" },
    commands = { "Prosession" },
    loaded = false,
    only_sequence = false,
    only_setup = false,
    path = "/home/wil/.local/share/nvim/site/pack/packer/opt/vim-obsession"
  },
  ["vim-prosession"] = {
    load_after = {
      ["vim-obsession"] = true
    },
    loaded = false,
    only_sequence = false,
    only_setup = false,
    path = "/home/wil/.local/share/nvim/site/pack/packer/opt/vim-prosession"
  },
  ["vim-sayonara"] = {
    commands = { "Sayonara" },
    loaded = false,
    only_sequence = false,
    only_setup = false,
    path = "/home/wil/.local/share/nvim/site/pack/packer/opt/vim-sayonara"
  },
  ["vim-startuptime"] = {
    commands = { "StartupTime" },
    loaded = false,
    only_sequence = false,
    only_setup = false,
    path = "/home/wil/.local/share/nvim/site/pack/packer/opt/vim-startuptime"
  },
  vimspector = {
    loaded = false,
    only_sequence = false,
    only_setup = false,
    path = "/home/wil/.local/share/nvim/site/pack/packer/opt/vimspector"
  },
  ["vista.vim"] = {
    commands = { "Vista" },
    loaded = false,
    only_sequence = false,
    only_setup = false,
    path = "/home/wil/.local/share/nvim/site/pack/packer/opt/vista.vim"
  }
}

local function handle_bufread(names)
  for _, name in ipairs(names) do
    local path = plugins[name].path
    for _, dir in ipairs({ 'ftdetect', 'ftplugin', 'after/ftdetect', 'after/ftplugin' }) do
      if #vim.fn.finddir(dir, path) > 0 then
        vim.cmd('doautocmd BufRead')
        return
      end
    end
  end
end

_packer_load = nil

local function handle_after(name, before)
  local plugin = plugins[name]
  plugin.load_after[before] = nil
  if next(plugin.load_after) == nil then
    _packer_load({name}, {})
  end
end

_packer_load = function(names, cause)
  local some_unloaded = false
  for _, name in ipairs(names) do
    if not plugins[name].loaded then
      some_unloaded = true
      break
    end
  end

  if not some_unloaded then return end

  local fmt = string.format
  local del_cmds = {}
  local del_maps = {}
  for _, name in ipairs(names) do
    if plugins[name].commands then
      for _, cmd in ipairs(plugins[name].commands) do
        del_cmds[cmd] = true
      end
    end

    if plugins[name].keys then
      for _, key in ipairs(plugins[name].keys) do
        del_maps[key] = true
      end
    end
  end

  for cmd, _ in pairs(del_cmds) do
    vim.cmd('silent! delcommand ' .. cmd)
  end

  for key, _ in pairs(del_maps) do
    vim.cmd(fmt('silent! %sunmap %s', key[1], key[2]))
  end

  for _, name in ipairs(names) do
    if not plugins[name].loaded then
      vim.cmd('packadd ' .. name)
      if plugins[name].config then
        for _i, config_line in ipairs(plugins[name].config) do
          loadstring(config_line)()
        end
      end

      if plugins[name].after then
        for _, after_name in ipairs(plugins[name].after) do
          handle_after(after_name, name)
          vim.cmd('redraw')
        end
      end

      plugins[name].loaded = true
    end
  end

  handle_bufread(names)

  if cause.cmd then
    local lines = cause.l1 == cause.l2 and '' or (cause.l1 .. ',' .. cause.l2)
    vim.cmd(fmt('%s%s%s %s', lines, cause.cmd, cause.bang, cause.args))
  elseif cause.keys then
    local keys = cause.keys
    local extra = ''
    while true do
      local c = vim.fn.getchar(0)
      if c == 0 then break end
      extra = extra .. vim.fn.nr2char(c)
    end

    if cause.prefix then
      local prefix = vim.v.count and vim.v.count or ''
      prefix = prefix .. '"' .. vim.v.register .. cause.prefix
      if vim.fn.mode('full') == 'no' then
        if vim.v.operator == 'c' then
          prefix = '' .. prefix
        end

        prefix = prefix .. vim.v.operator
      end

      vim.fn.feedkeys(prefix, 'n')
    end

    -- NOTE: I'm not sure if the below substitution is correct; it might correspond to the literal
    -- characters \<Plug> rather than the special <Plug> key.
    vim.fn.feedkeys(string.gsub(string.gsub(cause.keys, '^<Plug>', '\\<Plug>') .. extra, '<[cC][rR]>', '\r'))
  elseif cause.event then
    vim.cmd(fmt('doautocmd <nomodeline> %s', cause.event))
  elseif cause.ft then
    vim.cmd(fmt('doautocmd <nomodeline> %s FileType %s', 'filetypeplugin', cause.ft))
    vim.cmd(fmt('doautocmd <nomodeline> %s FileType %s', 'filetypeindent', cause.ft))
  end
end

-- Runtimepath customization

-- Pre-load configuration
-- Setup for: vimspector
loadstring("\27LJ\1\2B\0\0\2\0\4\0\0054\0\0\0007\0\1\0%\1\3\0:\1\2\0G\0\1\0\nHUMAN\31vimspector_enable_mappings\6g\bvim\0")()
-- Post-load configuration
-- Conditional loads
-- Load plugins in order defined by `after`
END

function! s:load(names, cause) abort
call luaeval('_packer_load(_A[1], _A[2])', [a:names, a:cause])
endfunction


" Command lazy-loads
command! -nargs=* -range -bang -complete=file Make call s:load(['vim-dispatch'], { "cmd": "Make", "l1": <line1>, "l2": <line2>, "bang": <q-bang>, "args": <q-args> })
command! -nargs=* -range -bang -complete=file Dispatch call s:load(['vim-dispatch'], { "cmd": "Dispatch", "l1": <line1>, "l2": <line2>, "bang": <q-bang>, "args": <q-args> })
command! -nargs=* -range -bang -complete=file Start call s:load(['vim-dispatch'], { "cmd": "Start", "l1": <line1>, "l2": <line2>, "bang": <q-bang>, "args": <q-args> })
command! -nargs=* -range -bang -complete=file Gpull call s:load(['vim-fugitive'], { "cmd": "Gpull", "l1": <line1>, "l2": <line2>, "bang": <q-bang>, "args": <q-args> })
command! -nargs=* -range -bang -complete=file Vista call s:load(['vista.vim'], { "cmd": "Vista", "l1": <line1>, "l2": <line2>, "bang": <q-bang>, "args": <q-args> })
command! -nargs=* -range -bang -complete=file StartupTime call s:load(['vim-startuptime'], { "cmd": "StartupTime", "l1": <line1>, "l2": <line2>, "bang": <q-bang>, "args": <q-args> })
command! -nargs=* -range -bang -complete=file EnMasse call s:load(['vim-enmasse'], { "cmd": "EnMasse", "l1": <line1>, "l2": <line2>, "bang": <q-bang>, "args": <q-args> })
command! -nargs=* -range -bang -complete=file Gstatus call s:load(['vim-fugitive'], { "cmd": "Gstatus", "l1": <line1>, "l2": <line2>, "bang": <q-bang>, "args": <q-args> })
command! -nargs=* -range -bang -complete=file IronRepl call s:load(['iron.nvim'], { "cmd": "IronRepl", "l1": <line1>, "l2": <line2>, "bang": <q-bang>, "args": <q-args> })
command! -nargs=* -range -bang -complete=file UndotreeToggle call s:load(['undotree'], { "cmd": "UndotreeToggle", "l1": <line1>, "l2": <line2>, "bang": <q-bang>, "args": <q-args> })
command! -nargs=* -range -bang -complete=file Gpush call s:load(['vim-fugitive'], { "cmd": "Gpush", "l1": <line1>, "l2": <line2>, "bang": <q-bang>, "args": <q-args> })
command! -nargs=* -range -bang -complete=file IronWatchCurrentFile call s:load(['iron.nvim'], { "cmd": "IronWatchCurrentFile", "l1": <line1>, "l2": <line2>, "bang": <q-bang>, "args": <q-args> })
command! -nargs=* -range -bang -complete=file Prosession call s:load(['vim-obsession'], { "cmd": "Prosession", "l1": <line1>, "l2": <line2>, "bang": <q-bang>, "args": <q-args> })
command! -nargs=* -range -bang -complete=file Focus call s:load(['vim-dispatch'], { "cmd": "Focus", "l1": <line1>, "l2": <line2>, "bang": <q-bang>, "args": <q-args> })
command! -nargs=* -range -bang -complete=file Grepper call s:load(['vim-grepper'], { "cmd": "Grepper", "l1": <line1>, "l2": <line2>, "bang": <q-bang>, "args": <q-args> })
command! -nargs=* -range -bang -complete=file Sayonara call s:load(['vim-sayonara'], { "cmd": "Sayonara", "l1": <line1>, "l2": <line2>, "bang": <q-bang>, "args": <q-args> })
command! -nargs=* -range -bang -complete=file IronSend call s:load(['iron.nvim'], { "cmd": "IronSend", "l1": <line1>, "l2": <line2>, "bang": <q-bang>, "args": <q-args> })

" Keymap lazy-loads

augroup packer_load_aucmds
  au!
  " Filetype lazy-loads
  " Event lazy-loads
  au VimEnter * ++once call s:load(['vim-matchup', 'nvim-treesitter'], { "event": "VimEnter *" })
  au InsertEnter * ++once call s:load(['completion-nvim'], { "event": "InsertEnter *" })
augroup END

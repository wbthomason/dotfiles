" Automatically generated packer.nvim plugin loader code

if !has('nvim-0.5')
  echohl WarningMsg
  echom "Invalid Neovim version for packer.nvim!"
  echohl None
  finish
endif
try

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
    after = { "nvim-treesitter-refactor", "nvim-treesitter-textobjects" },
    config = { "\27LJ\1\2Œ\b\0\0\a\0\28\0!4\0\0\0%\1\1\0>\0\2\0027\1\2\0003\2\4\0003\3\3\0:\3\5\0023\3\6\0:\3\a\0023\3\b\0:\3\t\0023\3\n\0003\4\v\0:\4\f\3:\3\r\0023\3\16\0003\4\14\0003\5\15\0:\5\f\4:\4\17\0033\4\18\0:\4\19\3:\3\20\0023\3\25\0003\4\21\0003\5\23\0003\6\22\0:\6\24\5:\5\f\4:\4\26\3:\3\27\2>\1\2\1G\0\1\0\16textobjects\vselect\1\0\0\aiF\1\0\15\aaC\17@class.outer\aas\21@statement.outer\ail\16@loop.inner\aad\19@comment.outer\aac\23@conditional.outer\aie\17@block.inner\aaf\20@function.outer\aiC\17@class.inner\ais\21@statement.inner\aal\16@loop.outer\aif\20@function.inner\aim\16@call.inner\aic\23@conditional.inner\aae\17@block.outer\aam\16@call.outer\1\0\4\bcpp$(function_definition) @function\vpython$(function_definition) @function\tjava#(method_declaration) @function\6c$(function_definition) @function\1\0\1\venable\2\rrefactor\26highlight_definitions\1\0\1\venable\2\17smart_rename\1\0\0\1\0\1\17smart_rename\bgrr\1\0\1\venable\2\26incremental_selection\fkeymaps\1\0\4\19init_selection\bgnn\22scope_incremental\bgrc\21node_incremental\bgrn\21node_decremental\bgrm\1\0\1\venable\2\vindent\1\0\1\venable\2\14highlight\1\0\2\21use_languagetree\2\venable\2\21ensure_installed\1\0\0\1\18\0\0\tbash\6c\bcpp\blua\bcss\vfennel\fhaskell\thtml\15javascript\tjson\njulia\nocaml\20ocaml_interface\vpython\trust\ttoml\15typescript\nsetup\28nvim-treesitter.configs\frequire\0" },
    loaded = false,
    only_sequence = false,
    only_setup = false,
    path = "/home/wil/.local/share/nvim/site/pack/packer/opt/nvim-treesitter"
  },
  ["nvim-treesitter-refactor"] = {
    load_after = {
      ["nvim-treesitter"] = true
    },
    loaded = false,
    only_sequence = false,
    only_setup = false,
    path = "/home/wil/.local/share/nvim/site/pack/packer/opt/nvim-treesitter-refactor"
  },
  ["nvim-treesitter-textobjects"] = {
    load_after = {
      ["nvim-treesitter"] = true
    },
    loaded = false,
    only_sequence = false,
    only_setup = false,
    path = "/home/wil/.local/share/nvim/site/pack/packer/opt/nvim-treesitter-textobjects"
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

local packer_load = nil
local function handle_after(name, before)
  local plugin = plugins[name]
  plugin.load_after[before] = nil
  if next(plugin.load_after) == nil then
    packer_load({name}, {})
  end
end

packer_load = function(names, cause)
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
      local prefix = vim.v.count ~= 0 and vim.v.count or ''
      prefix = prefix .. '"' .. vim.v.register .. cause.prefix
      if vim.fn.mode('full') == 'no' then
        if vim.v.operator == 'c' then
          prefix = '' .. prefix
        end

        prefix = prefix .. vim.v.operator
      end

      vim.fn.feedkeys(prefix, 'n')
    end

    local escaped_keys = vim.api.nvim_replace_termcodes(cause.keys .. extra, true, true, true)
    vim.api.nvim_feedkeys(escaped_keys, 'm', true)
  elseif cause.event then
    vim.cmd(fmt('doautocmd <nomodeline> %s', cause.event))
  elseif cause.ft then
    vim.cmd(fmt('doautocmd <nomodeline> %s FileType %s', 'filetypeplugin', cause.ft))
    vim.cmd(fmt('doautocmd <nomodeline> %s FileType %s', 'filetypeindent', cause.ft))
  end
end

_packer_load_wrapper = function(names, cause)
  success, err_msg = pcall(packer_load, names, cause)
  if not success then
    vim.cmd('echohl ErrorMsg')
    vim.cmd('echomsg "Error in packer_compiled: ' .. vim.fn.escape(err_msg, '"') .. '"')
    vim.cmd('echomsg "Please check your config for correctness"')
    vim.cmd('echohl None')
  end
end

-- Runtimepath customization

-- Pre-load configuration
-- Setup for: vimspector
loadstring("\27LJ\1\2B\0\0\2\0\4\0\0054\0\0\0007\0\1\0%\1\3\0:\1\2\0G\0\1\0\nHUMAN\31vimspector_enable_mappings\6g\bvim\0")()
-- Post-load configuration
-- Config for: formatter.nvim
loadstring("\27LJ\1\2Š\1\0\0\4\0\6\0\n3\0\0\0003\1\1\0004\2\2\0007\2\3\0027\2\4\2'\3\0\0>\2\2\2;\2\2\1:\1\5\0H\0\2\0\targs\22nvim_buf_get_name\bapi\bvim\1\4\0\0\21--stdin-filepath\0\19--single-quote\1\0\2\nstdin\2\bexe\rprettier{\0\0\5\0\a\0\f3\0\0\0002\1\3\0%\2\1\0004\3\2\0007\3\3\0037\3\4\3%\4\5\0>\3\2\2$\2\3\2;\2\1\1:\1\6\0H\0\2\0\targs\b%:t\vexpand\afn\bvim\22-assume-filename=\1\0\2\nstdin\2\bexe\17clang-formatD\0\0\2\0\3\0\0043\0\0\0003\1\1\0:\1\2\0H\0\2\0\targs\1\2\0\0\18--emit=stdout\1\0\2\nstdin\2\bexe\frustfmt(\0\0\1\0\1\0\0023\0\0\0H\0\2\0\1\0\2\bexe\15lua-format\nstdin\2\"\0\0\1\0\1\0\0023\0\0\0H\0\2\0\1\0\2\bexe\tyapf\nstdin\2>\0\0\2\0\3\0\0043\0\0\0003\1\1\0:\1\2\0H\0\2\0\targs\1\3\0\0\6-\f--quiet\1\0\2\nstdin\2\bexe\nisortY\0\0\2\0\3\0\0043\0\0\0003\1\1\0:\1\2\0H\0\2\0\targs\1\4\0\0\b-sl\19-g /dev/stderr\0162>/dev/null\1\0\2\nstdin\2\bexe\16latexindent\2\1\0\v\0\22\0,1\0\0\0001\1\1\0001\2\2\0001\3\3\0001\4\4\0001\5\5\0001\6\6\0004\a\a\0%\b\b\0>\a\2\0027\a\t\a3\b\n\0003\t\v\0002\n\3\0;\0\1\n:\n\f\t2\n\3\0;\0\1\n:\n\r\t2\n\3\0;\0\1\n:\n\14\t2\n\3\0;\2\1\n:\n\15\t2\n\3\0;\5\1\n;\4\2\n:\n\16\t2\n\3\0;\6\1\n:\n\17\t2\n\3\0;\1\1\n:\n\18\t2\n\3\0;\1\1\n:\n\19\t2\n\3\0;\3\1\n:\n\20\t:\t\21\b>\a\2\1G\0\1\0\rfiletype\blua\bcpp\6c\btex\vpython\trust\thtml\tjson\15javascript\1\0\0\1\0\1\flogging\1\nsetup\14formatter\frequire\0\0\0\0\0\0\0\0")()
-- Conditional loads
-- Load plugins in order defined by `after`
END

function! s:load(names, cause) abort
  call luaeval('_packer_load_wrapper(_A[1], _A[2])', [a:names, a:cause])
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
command! -nargs=* -range -bang -complete=file IronSend call s:load(['iron.nvim'], { "cmd": "IronSend", "l1": <line1>, "l2": <line2>, "bang": <q-bang>, "args": <q-args> })
command! -nargs=* -range -bang -complete=file Focus call s:load(['vim-dispatch'], { "cmd": "Focus", "l1": <line1>, "l2": <line2>, "bang": <q-bang>, "args": <q-args> })
command! -nargs=* -range -bang -complete=file IronWatchCurrentFile call s:load(['iron.nvim'], { "cmd": "IronWatchCurrentFile", "l1": <line1>, "l2": <line2>, "bang": <q-bang>, "args": <q-args> })
command! -nargs=* -range -bang -complete=file Gpush call s:load(['vim-fugitive'], { "cmd": "Gpush", "l1": <line1>, "l2": <line2>, "bang": <q-bang>, "args": <q-args> })
command! -nargs=* -range -bang -complete=file UndotreeToggle call s:load(['undotree'], { "cmd": "UndotreeToggle", "l1": <line1>, "l2": <line2>, "bang": <q-bang>, "args": <q-args> })
command! -nargs=* -range -bang -complete=file Grepper call s:load(['vim-grepper'], { "cmd": "Grepper", "l1": <line1>, "l2": <line2>, "bang": <q-bang>, "args": <q-args> })
command! -nargs=* -range -bang -complete=file Sayonara call s:load(['vim-sayonara'], { "cmd": "Sayonara", "l1": <line1>, "l2": <line2>, "bang": <q-bang>, "args": <q-args> })
command! -nargs=* -range -bang -complete=file Prosession call s:load(['vim-obsession'], { "cmd": "Prosession", "l1": <line1>, "l2": <line2>, "bang": <q-bang>, "args": <q-args> })

" Keymap lazy-loads

augroup packer_load_aucmds
  au!
  " Filetype lazy-loads
  " Event lazy-loads
  au VimEnter * ++once call s:load(['vim-matchup', 'nvim-treesitter'], { "event": "VimEnter *" })
  au InsertEnter * ++once call s:load(['completion-nvim'], { "event": "InsertEnter *" })
  " Function lazy-loads
augroup END

catch
  echohl ErrorMsg
  echom "Error in packer_compiled: " .. v:exception
  echom "Please check your config for correctness"
  echohl None
endtry

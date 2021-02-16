" Automatically generated packer.nvim plugin loader code

if !has('nvim-0.5')
  echohl WarningMsg
  echom "Invalid Neovim version for packer.nvim!"
  echohl None
  finish
endif

packadd packer.nvim

try

lua << END
local package_path_str = "/home/wil/.cache/nvim/packer_hererocks/2.0.5/share/lua/5.1/?.lua;/home/wil/.cache/nvim/packer_hererocks/2.0.5/share/lua/5.1/?/init.lua;/home/wil/.cache/nvim/packer_hererocks/2.0.5/lib/luarocks/rocks-5.1/?.lua;/home/wil/.cache/nvim/packer_hererocks/2.0.5/lib/luarocks/rocks-5.1/?/init.lua"
local install_cpath_pattern = "/home/wil/.cache/nvim/packer_hererocks/2.0.5/lib/lua/5.1/?.so"
if not string.find(package.path, package_path_str, 1, true) then
  package.path = package.path .. ';' .. package_path_str
end

if not string.find(package.cpath, install_cpath_pattern, 1, true) then
  package.cpath = package.cpath .. ';' .. install_cpath_pattern
end

local function try_loadstring(s, component, name)
  local success, result = pcall(loadstring(s))
  if not success then
    print('Error running ' .. component .. ' for ' .. name)
    error(result)
  end
  return result
end

_G.packer_plugins = {
  ["formatter.nvim"] = {
    config = { "require('config.format')" },
    loaded = true,
    path = "/home/wil/.local/share/nvim/site/pack/packer/start/formatter.nvim"
  },
  ["fzf-lsp.nvim"] = {
    loaded = true,
    path = "/home/wil/.local/share/nvim/site/pack/packer/start/fzf-lsp.nvim"
  },
  ["fzf.vim"] = {
    config = { "require('config.fzf')" },
    loaded = true,
    path = "/home/wil/.local/share/nvim/site/pack/packer/start/fzf.vim"
  },
  ["gitsigns.nvim"] = {
    config = { "require('config.gitsigns')" },
    loaded = true,
    path = "/home/wil/.local/share/nvim/site/pack/packer/start/gitsigns.nvim"
  },
  indentLine = {
    after_files = { "/home/wil/.local/share/nvim/site/pack/packer/opt/indentLine/after/plugin/indentLine.vim" },
    loaded = false,
    needs_bufread = false,
    path = "/home/wil/.local/share/nvim/site/pack/packer/opt/indentLine"
  },
  ["lazygit.nvim"] = {
    loaded = true,
    path = "/home/wil/.local/share/nvim/site/pack/packer/start/lazygit.nvim"
  },
  ["lsp-status.nvim"] = {
    loaded = true,
    path = "/home/wil/.local/share/nvim/site/pack/packer/start/lsp-status.nvim"
  },
  ["lspkind-nvim"] = {
    loaded = true,
    path = "/home/wil/.local/share/nvim/site/pack/packer/start/lspkind-nvim"
  },
  mesonic = {
    loaded = true,
    path = "/home/wil/.local/share/nvim/site/pack/packer/start/mesonic"
  },
  ["nvim-colorizer.lua"] = {
    config = { "require('colorizer').setup {'css', 'javascript', 'vim', 'html'}" },
    loaded = false,
    needs_bufread = false,
    path = "/home/wil/.local/share/nvim/site/pack/packer/opt/nvim-colorizer.lua"
  },
  ["nvim-compe"] = {
    after_files = { "/home/wil/.local/share/nvim/site/pack/packer/opt/nvim-compe/after/plugin/compe_buffer.vim", "/home/wil/.local/share/nvim/site/pack/packer/opt/nvim-compe/after/plugin/compe_calc.vim", "/home/wil/.local/share/nvim/site/pack/packer/opt/nvim-compe/after/plugin/compe_nvim_lsp.vim", "/home/wil/.local/share/nvim/site/pack/packer/opt/nvim-compe/after/plugin/compe_nvim_lua.vim", "/home/wil/.local/share/nvim/site/pack/packer/opt/nvim-compe/after/plugin/compe_omni.vim", "/home/wil/.local/share/nvim/site/pack/packer/opt/nvim-compe/after/plugin/compe_path.vim", "/home/wil/.local/share/nvim/site/pack/packer/opt/nvim-compe/after/plugin/compe_snippets_nvim.vim", "/home/wil/.local/share/nvim/site/pack/packer/opt/nvim-compe/after/plugin/compe_spell.vim", "/home/wil/.local/share/nvim/site/pack/packer/opt/nvim-compe/after/plugin/compe_tags.vim", "/home/wil/.local/share/nvim/site/pack/packer/opt/nvim-compe/after/plugin/compe_treesitter.vim", "/home/wil/.local/share/nvim/site/pack/packer/opt/nvim-compe/after/plugin/compe_ultisnips.vim", "/home/wil/.local/share/nvim/site/pack/packer/opt/nvim-compe/after/plugin/compe_vim_lsc.vim", "/home/wil/.local/share/nvim/site/pack/packer/opt/nvim-compe/after/plugin/compe_vim_lsp.vim", "/home/wil/.local/share/nvim/site/pack/packer/opt/nvim-compe/after/plugin/compe_vsnip.vim" },
    config = { "require('config.compe')" },
    loaded = false,
    needs_bufread = false,
    path = "/home/wil/.local/share/nvim/site/pack/packer/opt/nvim-compe"
  },
  ["nvim-dap"] = {
    loaded = false,
    needs_bufread = false,
    path = "/home/wil/.local/share/nvim/site/pack/packer/opt/nvim-dap"
  },
  ["nvim-lightbulb"] = {
    config = { "require('config.lightbulb')" },
    loaded = true,
    path = "/home/wil/.local/share/nvim/site/pack/packer/start/nvim-lightbulb"
  },
  ["nvim-lspconfig"] = {
    loaded = true,
    path = "/home/wil/.local/share/nvim/site/pack/packer/start/nvim-lspconfig"
  },
  ["nvim-treesitter"] = {
    config = { "require('config.treesitter')" },
    loaded = true,
    path = "/home/wil/.local/share/nvim/site/pack/packer/start/nvim-treesitter"
  },
  ["nvim-treesitter-refactor"] = {
    loaded = true,
    path = "/home/wil/.local/share/nvim/site/pack/packer/start/nvim-treesitter-refactor"
  },
  ["nvim-treesitter-textobjects"] = {
    loaded = true,
    path = "/home/wil/.local/share/nvim/site/pack/packer/start/nvim-treesitter-textobjects"
  },
  ["nvim-web-devicons"] = {
    loaded = true,
    path = "/home/wil/.local/share/nvim/site/pack/packer/start/nvim-web-devicons"
  },
  ["packer.nvim"] = {
    loaded = false,
    needs_bufread = false,
    path = "/home/wil/.local/share/nvim/site/pack/packer/opt/packer.nvim"
  },
  ["pddl.vim"] = {
    loaded = true,
    path = "/home/wil/.local/share/nvim/site/pack/packer/start/pddl.vim"
  },
  ["plenary.nvim"] = {
    loaded = true,
    path = "/home/wil/.local/share/nvim/site/pack/packer/start/plenary.nvim"
  },
  ["targets.vim"] = {
    loaded = true,
    path = "/home/wil/.local/share/nvim/site/pack/packer/start/targets.vim"
  },
  tcomment_vim = {
    loaded = true,
    path = "/home/wil/.local/share/nvim/site/pack/packer/start/tcomment_vim"
  },
  undotree = {
    commands = { "UndotreeToggle" },
    config = { "vim.g.undotree_SetFocusWhenToggle = 1" },
    loaded = false,
    needs_bufread = false,
    path = "/home/wil/.local/share/nvim/site/pack/packer/opt/undotree"
  },
  ["vim-cool"] = {
    loaded = true,
    path = "/home/wil/.local/share/nvim/site/pack/packer/start/vim-cool"
  },
  ["vim-dirvish"] = {
    loaded = true,
    path = "/home/wil/.local/share/nvim/site/pack/packer/start/vim-dirvish"
  },
  ["vim-dispatch"] = {
    commands = { "Dispatch", "Make", "Focus", "Start" },
    loaded = false,
    needs_bufread = false,
    path = "/home/wil/.local/share/nvim/site/pack/packer/opt/vim-dispatch"
  },
  ["vim-easy-align"] = {
    config = { "require('config.easy_align')" },
    loaded = true,
    path = "/home/wil/.local/share/nvim/site/pack/packer/start/vim-easy-align"
  },
  ["vim-enmasse"] = {
    commands = { "EnMasse" },
    loaded = false,
    needs_bufread = false,
    path = "/home/wil/.local/share/nvim/site/pack/packer/opt/vim-enmasse"
  },
  ["vim-floaterm"] = {
    loaded = true,
    path = "/home/wil/.local/share/nvim/site/pack/packer/start/vim-floaterm"
  },
  ["vim-matchup"] = {
    after_files = { "/home/wil/.local/share/nvim/site/pack/packer/opt/vim-matchup/after/plugin/matchit.vim" },
    loaded = false,
    needs_bufread = true,
    path = "/home/wil/.local/share/nvim/site/pack/packer/opt/vim-matchup"
  },
  ["vim-nazgul"] = {
    loaded = true,
    path = "/home/wil/.local/share/nvim/site/pack/packer/start/vim-nazgul"
  },
  ["vim-obsession"] = {
    after = { "vim-prosession" },
    commands = { "Prosession" },
    loaded = false,
    needs_bufread = false,
    path = "/home/wil/.local/share/nvim/site/pack/packer/opt/vim-obsession"
  },
  ["vim-peekaboo"] = {
    loaded = true,
    path = "/home/wil/.local/share/nvim/site/pack/packer/start/vim-peekaboo"
  },
  ["vim-prosession"] = {
    config = { "require('config.prosession')" },
    load_after = {
      ["vim-obsession"] = true
    },
    loaded = false,
    needs_bufread = false,
    path = "/home/wil/.local/share/nvim/site/pack/packer/opt/vim-prosession"
  },
  ["vim-sandwich"] = {
    loaded = true,
    path = "/home/wil/.local/share/nvim/site/pack/packer/start/vim-sandwich"
  },
  ["vim-sayonara"] = {
    commands = { "Sayonara" },
    loaded = false,
    needs_bufread = false,
    path = "/home/wil/.local/share/nvim/site/pack/packer/opt/vim-sayonara"
  },
  ["vim-sneak"] = {
    config = { "require('config.sneak')" },
    loaded = true,
    path = "/home/wil/.local/share/nvim/site/pack/packer/start/vim-sneak"
  },
  ["vim-startuptime"] = {
    commands = { "StartupTime" },
    config = { "vim.g.startuptime_tries = 10" },
    loaded = false,
    needs_bufread = false,
    path = "/home/wil/.local/share/nvim/site/pack/packer/opt/vim-startuptime"
  },
  ["vim-vsnip"] = {
    config = { "require('config.vsnip')" },
    loaded = false,
    needs_bufread = false,
    path = "/home/wil/.local/share/nvim/site/pack/packer/opt/vim-vsnip"
  },
  ["vim-wordmotion"] = {
    loaded = true,
    path = "/home/wil/.local/share/nvim/site/pack/packer/start/vim-wordmotion"
  },
  vimtex = {
    config = { "require('config.vimtex')" },
    loaded = true,
    path = "/home/wil/.local/share/nvim/site/pack/packer/start/vimtex"
  }
}

-- Setup for: indentLine
require('config.indentline')
vim.cmd [[packadd indentLine]]
-- Setup for: vim-matchup
require('config.matchup')
vim.cmd [[packadd vim-matchup]]
-- Config for: formatter.nvim
require('config.format')
-- Config for: nvim-treesitter
require('config.treesitter')
-- Config for: vimtex
require('config.vimtex')
-- Config for: gitsigns.nvim
require('config.gitsigns')
-- Config for: vim-sneak
require('config.sneak')
-- Config for: nvim-lightbulb
require('config.lightbulb')
-- Config for: fzf.vim
require('config.fzf')
-- Config for: vim-easy-align
require('config.easy_align')

-- Command lazy-loads
vim.cmd [[command! -nargs=* -range -bang -complete=file Make lua require("packer.load")({'vim-dispatch'}, { cmd = "Make", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]]
vim.cmd [[command! -nargs=* -range -bang -complete=file StartupTime lua require("packer.load")({'vim-startuptime'}, { cmd = "StartupTime", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]]
vim.cmd [[command! -nargs=* -range -bang -complete=file EnMasse lua require("packer.load")({'vim-enmasse'}, { cmd = "EnMasse", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]]
vim.cmd [[command! -nargs=* -range -bang -complete=file Dispatch lua require("packer.load")({'vim-dispatch'}, { cmd = "Dispatch", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]]
vim.cmd [[command! -nargs=* -range -bang -complete=file Focus lua require("packer.load")({'vim-dispatch'}, { cmd = "Focus", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]]
vim.cmd [[command! -nargs=* -range -bang -complete=file Start lua require("packer.load")({'vim-dispatch'}, { cmd = "Start", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]]
vim.cmd [[command! -nargs=* -range -bang -complete=file UndotreeToggle lua require("packer.load")({'undotree'}, { cmd = "UndotreeToggle", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]]
vim.cmd [[command! -nargs=* -range -bang -complete=file Sayonara lua require("packer.load")({'vim-sayonara'}, { cmd = "Sayonara", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]]
vim.cmd [[command! -nargs=* -range -bang -complete=file Prosession lua require("packer.load")({'vim-obsession'}, { cmd = "Prosession", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]]

vim.cmd [[augroup packer_load_aucmds]]
vim.cmd [[au!]]
  -- Filetype lazy-loads
vim.cmd [[au FileType html ++once lua require("packer.load")({'nvim-colorizer.lua'}, { ft = "html" }, _G.packer_plugins)]]
vim.cmd [[au FileType javascript ++once lua require("packer.load")({'nvim-colorizer.lua'}, { ft = "javascript" }, _G.packer_plugins)]]
vim.cmd [[au FileType css ++once lua require("packer.load")({'nvim-colorizer.lua'}, { ft = "css" }, _G.packer_plugins)]]
vim.cmd [[au FileType vim ++once lua require("packer.load")({'nvim-colorizer.lua'}, { ft = "vim" }, _G.packer_plugins)]]
  -- Event lazy-loads
vim.cmd [[au InsertEnter * ++once lua require("packer.load")({'nvim-compe', 'vim-vsnip'}, { event = "InsertEnter *" }, _G.packer_plugins)]]
vim.cmd("augroup END")
END

catch
  echohl ErrorMsg
  echom "Error in packer_compiled: " .. v:exception
  echom "Please check your config for correctness"
  echohl None
endtry

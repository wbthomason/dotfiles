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
    config = { "\27LJ\1\2Š\1\0\0\4\0\6\0\n3\0\0\0003\1\1\0004\2\2\0007\2\3\0027\2\4\2'\3\0\0>\2\2\2;\2\2\1:\1\5\0H\0\2\0\targs\22nvim_buf_get_name\bapi\bvim\1\4\0\0\21--stdin-filepath\0\19--single-quote\1\0\2\nstdin\2\bexe\rprettier{\0\0\5\0\a\0\f3\0\0\0002\1\3\0%\2\1\0004\3\2\0007\3\3\0037\3\4\3%\4\5\0>\3\2\2$\2\3\2;\2\1\1:\1\6\0H\0\2\0\targs\b%:t\vexpand\afn\bvim\22-assume-filename=\1\0\2\nstdin\2\bexe\17clang-formatD\0\0\2\0\3\0\0043\0\0\0003\1\1\0:\1\2\0H\0\2\0\targs\1\2\0\0\18--emit=stdout\1\0\2\nstdin\2\bexe\frustfmt(\0\0\1\0\1\0\0023\0\0\0H\0\2\0\1\0\2\bexe\15lua-format\nstdin\2\"\0\0\1\0\1\0\0023\0\0\0H\0\2\0\1\0\2\bexe\tyapf\nstdin\2>\0\0\2\0\3\0\0043\0\0\0003\1\1\0:\1\2\0H\0\2\0\targs\1\3\0\0\6-\f--quiet\1\0\2\nstdin\2\bexe\nisortY\0\0\2\0\3\0\0043\0\0\0003\1\1\0:\1\2\0H\0\2\0\targs\1\4\0\0\b-sl\19-g /dev/stderr\0162>/dev/null\1\0\2\nstdin\2\bexe\16latexindent\2\1\0\v\0\22\0,1\0\0\0001\1\1\0001\2\2\0001\3\3\0001\4\4\0001\5\5\0001\6\6\0004\a\a\0%\b\b\0>\a\2\0027\a\t\a3\b\n\0003\t\v\0002\n\3\0;\0\1\n:\n\f\t2\n\3\0;\0\1\n:\n\r\t2\n\3\0;\0\1\n:\n\14\t2\n\3\0;\2\1\n:\n\15\t2\n\3\0;\5\1\n;\4\2\n:\n\16\t2\n\3\0;\6\1\n:\n\17\t2\n\3\0;\1\1\n:\n\18\t2\n\3\0;\1\1\n:\n\19\t2\n\3\0;\3\1\n:\n\20\t:\t\21\b>\a\2\1G\0\1\0\rfiletype\blua\bcpp\6c\btex\vpython\trust\thtml\tjson\15javascript\1\0\0\1\0\1\flogging\1\nsetup\14formatter\frequire\0\0\0\0\0\0\0\0" },
    loaded = true,
    path = "/home/wil/.local/share/nvim/site/pack/packer/start/formatter.nvim"
  },
  ["fzf.vim"] = {
    loaded = true,
    path = "/home/wil/.local/share/nvim/site/pack/packer/start/fzf.vim"
  },
  indentLine = {
    config = { "\27LJ\1\2—\1\0\0\2\0\a\0\t4\0\0\0007\0\1\0%\1\3\0:\1\2\0'\1\1\0:\1\4\0003\1\6\0:\1\5\0G\0\1\0\1\6\0\0\btex\rmarkdown\btxt\rstartify\vpacker\31indentLine_fileTypeExclude\22indentLine_faster\bâ”‚\20indentLine_char\6g\bvim\0" },
    loaded = true,
    path = "/home/wil/.local/share/nvim/site/pack/packer/start/indentLine"
  },
  ["julia-vim"] = {
    loaded = true,
    path = "/home/wil/.local/share/nvim/site/pack/packer/start/julia-vim"
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
    path = "/home/wil/.local/share/nvim/site/pack/packer/opt/nvim-colorizer.lua"
  },
  ["nvim-compe"] = {
    config = { "\27LJ\1\2ˆ\5\0\0\a\0\25\0<4\0\0\0%\1\1\0>\0\2\0027\0\2\0004\1\3\0007\1\4\1)\2\2\0:\2\5\0014\1\3\0007\1\4\1)\2\2\0:\2\6\0014\1\3\0007\1\4\1)\2\2\0:\2\a\0014\1\3\0007\1\4\1)\2\2\0:\2\b\0014\1\3\0007\1\4\1)\2\2\0:\2\t\0014\1\3\0007\1\4\1)\2\2\0:\2\n\0014\1\3\0007\1\4\1)\2\2\0:\2\v\0014\1\0\0%\2\f\0>\1\2\0027\1\r\0013\2\14\0003\3\15\0:\3\16\2>\1\2\0013\1\17\0\16\2\0\0%\3\18\0%\4\19\0%\5\20\0\16\6\1\0>\2\5\1\16\2\0\0%\3\18\0%\4\21\0%\5\22\0\16\6\1\0>\2\5\1\16\2\0\0%\3\18\0%\4\23\0%\5\24\0\16\6\1\0>\2\5\1G\0\1\0\25compe#close('<c-e>')\n<c-e>\26compe#confirm('<cr>')\t<cr>\21compe#complete()\n<c-c>\6i\1\0\3\texpr\2\vsilent\2\fnoremap\2\vsource\1\0\5\rnvim_lua\2\rnvim_lsp\2\vbuffer\2\nvsnip\2\tpath\2\1\0\4\14preselect\valways\15min_length\3\1\fenabled\2\ndebug\1\nsetup\ncompe\25loaded_compe_vim_lsp\25loaded_compe_vim_lsc\27loaded_compe_ultisnips\22loaded_compe_tags\23loaded_compe_spell\31loaded_compe_snippets_nvim\28loaded_compe_treesitter\6g\bvim\bmap\17config.utils\frequire\0" },
    loaded = true,
    path = "/home/wil/.local/share/nvim/site/pack/packer/start/nvim-compe"
  },
  ["nvim-dap"] = {
    loaded = false,
    path = "/home/wil/.local/share/nvim/site/pack/packer/opt/nvim-dap"
  },
  ["nvim-lspconfig"] = {
    loaded = true,
    path = "/home/wil/.local/share/nvim/site/pack/packer/start/nvim-lspconfig"
  },
  ["nvim-treesitter"] = {
    config = { "\27LJ\1\2Œ\b\0\0\a\0\28\0!4\0\0\0%\1\1\0>\0\2\0027\1\2\0003\2\4\0003\3\3\0:\3\5\0023\3\6\0:\3\a\0023\3\b\0:\3\t\0023\3\n\0003\4\v\0:\4\f\3:\3\r\0023\3\16\0003\4\14\0003\5\15\0:\5\f\4:\4\17\0033\4\18\0:\4\19\3:\3\20\0023\3\25\0003\4\21\0003\5\23\0003\6\22\0:\6\24\5:\5\f\4:\4\26\3:\3\27\2>\1\2\1G\0\1\0\16textobjects\vselect\1\0\0\aiF\1\0\15\aaC\17@class.outer\aas\21@statement.outer\ail\16@loop.inner\aad\19@comment.outer\aac\23@conditional.outer\aie\17@block.inner\aaf\20@function.outer\aiC\17@class.inner\ais\21@statement.inner\aal\16@loop.outer\aif\20@function.inner\aim\16@call.inner\aic\23@conditional.inner\aae\17@block.outer\aam\16@call.outer\1\0\4\bcpp$(function_definition) @function\vpython$(function_definition) @function\tjava#(method_declaration) @function\6c$(function_definition) @function\1\0\1\venable\2\rrefactor\26highlight_definitions\1\0\1\venable\2\17smart_rename\1\0\0\1\0\1\17smart_rename\bgrr\1\0\1\venable\2\26incremental_selection\fkeymaps\1\0\4\19init_selection\bgnn\22scope_incremental\bgrc\21node_incremental\bgrn\21node_decremental\bgrm\1\0\1\venable\2\vindent\1\0\1\venable\2\14highlight\1\0\2\21use_languagetree\2\venable\2\21ensure_installed\1\0\0\1\18\0\0\tbash\6c\bcpp\blua\bcss\vfennel\fhaskell\thtml\15javascript\tjson\njulia\nocaml\20ocaml_interface\vpython\trust\ttoml\15typescript\nsetup\28nvim-treesitter.configs\frequire\0" },
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
  ["packer.nvim"] = {
    loaded = false,
    path = "/home/wil/.local/share/nvim/site/pack/packer/opt/packer.nvim"
  },
  ["pddl.vim"] = {
    loaded = true,
    path = "/home/wil/.local/share/nvim/site/pack/packer/start/pddl.vim"
  },
  ["pdf-scribe.nvim"] = {
    config = { "\27LJ\1\2n\0\0\2\0\6\0\a4\0\0\0007\0\1\0%\1\3\0:\1\2\0%\1\5\0:\1\4\0G\0\1\0\19~/gdrive/notes\24pdfscribe_notes_dir\20~/gdrive/papers\22pdfscribe_pdf_dir\6g\bvim\0" },
    loaded = true,
    path = "/home/wil/.local/share/nvim/site/pack/packer/start/pdf-scribe.nvim"
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
    path = "/home/wil/.local/share/nvim/site/pack/packer/opt/vim-dispatch"
  },
  ["vim-easy-align"] = {
    config = { "\27LJ\1\2\1\0\0\a\0\a\0\f4\0\0\0%\1\1\0>\0\2\0027\0\2\0003\1\3\0\16\2\0\0003\3\4\0%\4\5\0%\5\6\0\16\6\1\0>\2\5\1G\0\1\0\22<Plug>(EasyAlign)\aga\1\3\0\0\6x\6n\1\0\2\fnoremap\1\vsilent\2\bmap\17config.utils\frequire\0" },
    loaded = true,
    path = "/home/wil/.local/share/nvim/site/pack/packer/start/vim-easy-align"
  },
  ["vim-enmasse"] = {
    commands = { "EnMasse" },
    loaded = false,
    path = "/home/wil/.local/share/nvim/site/pack/packer/opt/vim-enmasse"
  },
  ["vim-floaterm"] = {
    loaded = true,
    path = "/home/wil/.local/share/nvim/site/pack/packer/start/vim-floaterm"
  },
  ["vim-fugitive"] = {
    commands = { "Gblame", "Gpull", "Gpush", "Gstatus" },
    loaded = false,
    path = "/home/wil/.local/share/nvim/site/pack/packer/opt/vim-fugitive"
  },
  ["vim-matchup"] = {
    loaded = false,
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
    path = "/home/wil/.local/share/nvim/site/pack/packer/opt/vim-prosession"
  },
  ["vim-sandwich"] = {
    loaded = true,
    path = "/home/wil/.local/share/nvim/site/pack/packer/start/vim-sandwich"
  },
  ["vim-sayonara"] = {
    commands = { "Sayonara" },
    loaded = false,
    path = "/home/wil/.local/share/nvim/site/pack/packer/opt/vim-sayonara"
  },
  ["vim-signify"] = {
    config = { "\27LJ\1\2Ö\1\0\0\2\0\n\0\0154\0\0\0007\0\1\0003\1\3\0:\1\2\0%\1\5\0:\1\4\0%\1\5\0:\1\6\0%\1\5\0:\1\a\0007\1\a\0:\1\b\0'\1\0\0:\1\t\0G\0\1\0\28signify_sign_show_count#signify_sign_delete_first_line\24signify_sign_delete\21signify_sign_add\bâ”‚\24signify_sign_change\1\2\0\0\bgit\21signify_vcs_list\6g\bvim\0" },
    loaded = true,
    path = "/home/wil/.local/share/nvim/site/pack/packer/start/vim-signify"
  },
  ["vim-sneak"] = {
    config = { "\27LJ\1\2ü\5\0\0\b\0\29\0†\0014\0\0\0%\1\1\0>\0\2\0027\0\2\0004\1\3\0007\1\4\0013\2\5\0\16\3\0\0%\4\6\0%\5\a\0%\6\b\0\16\a\2\0>\3\5\1\16\3\0\0%\4\6\0%\5\t\0%\6\n\0\16\a\2\0>\3\5\1\16\3\0\0%\4\v\0%\5\a\0%\6\b\0\16\a\2\0>\3\5\1\16\3\0\0%\4\v\0%\5\t\0%\6\n\0\16\a\2\0>\3\5\1\16\3\0\0%\4\f\0%\5\a\0%\6\b\0\16\a\2\0>\3\5\1\16\3\0\0%\4\f\0%\5\t\0%\6\n\0\16\a\2\0>\3\5\1\16\3\0\0%\4\r\0%\5\14\0%\6\15\0\16\a\2\0>\3\5\1\16\3\0\0%\4\r\0%\5\16\0%\6\17\0\16\a\2\0>\3\5\1\16\3\0\0%\4\6\0%\5\18\0%\6\19\0\16\a\2\0>\3\5\1\16\3\0\0%\4\6\0%\5\20\0%\6\21\0\16\a\2\0>\3\5\1\16\3\0\0%\4\v\0%\5\18\0%\6\19\0\16\a\2\0>\3\5\1\16\3\0\0%\4\v\0%\5\20\0%\6\21\0\16\a\2\0>\3\5\1\16\3\0\0%\4\f\0%\5\18\0%\6\19\0\16\a\2\0>\3\5\1\16\3\0\0%\4\f\0%\5\20\0%\6\21\0\16\a\2\0>\3\5\1\16\3\0\0%\4\6\0%\5\22\0%\6\23\0\16\a\2\0>\3\5\1\16\3\0\0%\4\6\0%\5\24\0%\6\25\0\16\a\2\0>\3\5\1\16\3\0\0%\4\v\0%\5\22\0%\6\23\0\16\a\2\0>\3\5\1\16\3\0\0%\4\v\0%\5\24\0%\6\25\0\16\a\2\0>\3\5\1\16\3\0\0%\4\f\0%\5\22\0%\6\23\0\16\a\2\0>\3\5\1\16\3\0\0%\4\f\0%\5\24\0%\6\25\0\16\a\2\0>\3\5\1'\3\1\0:\3\26\1'\3\1\0:\3\27\1'\3\1\0:\3\28\1G\0\1\0\21sneak#use_ic_scs\16sneak#label\17sneak#s_next\18<Plug>Sneak_T\6T\18<Plug>Sneak_t\6t\18<Plug>Sneak_F\6F\18<Plug>Sneak_f\6f\18<Plug>Sneak_,\6\\\18<Plug>Sneak_;\6;\5\6o\6x\18<Plug>Sneak_S\6Z\18<Plug>Sneak_s\6z\6n\1\0\1\fnoremap\1\6g\bvim\bmap\17config.utils\frequire\0" },
    loaded = true,
    path = "/home/wil/.local/share/nvim/site/pack/packer/start/vim-sneak"
  },
  ["vim-startuptime"] = {
    commands = { "StartupTime" },
    config = { "vim.g.startuptime_tries = 5" },
    loaded = false,
    path = "/home/wil/.local/share/nvim/site/pack/packer/opt/vim-startuptime"
  },
  ["vim-toml"] = {
    loaded = true,
    path = "/home/wil/.local/share/nvim/site/pack/packer/start/vim-toml"
  },
  ["vim-vsnip"] = {
    config = { "\27LJ\1\2°\2\0\0\a\0\n\0\0304\0\0\0%\1\1\0>\0\2\0027\0\2\0003\1\3\0\16\2\0\0%\3\4\0%\4\5\0%\5\6\0\16\6\1\0>\2\5\1\16\2\0\0%\3\a\0%\4\5\0%\5\6\0\16\6\1\0>\2\5\1\16\2\0\0%\3\4\0%\4\b\0%\5\t\0\16\6\1\0>\2\5\1\16\2\0\0%\3\a\0%\4\b\0%\5\t\0\16\6\1\0>\2\5\1G\0\1\0?vsnip#jumpable(-1) ? '<Plug>(vsnip-jump-prev)' : '<S-Tab>'\f<S-Tab>\6s=vsnip#jumpable(1)  ? '<Plug>(vsnip-jump-next)' : '<Tab>'\n<Tab>\6i\1\0\2\texpr\2\fnoremap\1\bmap\17config.utils\frequire\0" },
    loaded = true,
    path = "/home/wil/.local/share/nvim/site/pack/packer/start/vim-vsnip"
  },
  ["vim-wordmotion"] = {
    loaded = true,
    path = "/home/wil/.local/share/nvim/site/pack/packer/start/vim-wordmotion"
  },
  vimspector = {
    loaded = false,
    path = "/home/wil/.local/share/nvim/site/pack/packer/opt/vimspector"
  },
  vimtex = {
    config = { "\27LJ\1\2ú\3\0\0\2\0\21\0\0274\0\0\0007\0\1\0%\1\3\0:\1\2\0'\1\1\0:\1\4\0'\1\1\0:\1\5\0%\1\a\0:\1\6\0%\1\t\0:\1\b\0'\1\0\0:\1\n\0'\1\0\0:\1\v\0%\1\r\0:\1\f\0%\1\15\0:\1\14\0%\1\17\0:\1\16\0003\1\19\0:\1\18\0'\1\0\0:\1\20\0G\0\1\0\"vimtex_syntax_conceal_default\1\0\1\bfoo\2\26vimtex_syntax_conceal\r--unique(vimtex_view_general_options_latexmk&--unique file:@pdf\\#src:@line@tex vimtex_view_general_options\vokular\31vimtex_view_general_viewer(vimtex_view_forward_search_on_start\25vimtex_quickfix_mode\5\16tex_conceal\fpplatex\27vimtex_quickfix_method\28vimtex_complete_enabled\"vimtex_complete_recursive_bib\bnvr\29vimtex_compiler_progname\6g\bvim\0" },
    loaded = true,
    path = "/home/wil/.local/share/nvim/site/pack/packer/start/vimtex"
  }
}

-- Setup for: vimspector
vim.g.vimspector_enable_mappings = 'HUMAN'
-- Setup for: vim-matchup
try_loadstring("\27LJ\1\2‚\2\0\0\2\0\b\0\0154\0\0\0007\0\1\0'\1\1\0:\1\2\0'\1d\0:\1\3\0'\1\1\0:\1\4\0'\1\1\0:\1\5\0'\1\0\0:\1\6\0'\1\0\0:\1\a\0G\0\1\0\30matchup_transmute_enabled\"matchup_delim_start_plaintext\28matchup_override_vimtex*matchup_matchparen_hi_surround_always+matchup_matchparen_deferred_show_delay matchup_matchparen_deferred\6g\bvim\0", "setup", "vim-matchup")
vim.cmd [[packadd vim-matchup]]
-- Config for: formatter.nvim
try_loadstring("\27LJ\1\2Š\1\0\0\4\0\6\0\n3\0\0\0003\1\1\0004\2\2\0007\2\3\0027\2\4\2'\3\0\0>\2\2\2;\2\2\1:\1\5\0H\0\2\0\targs\22nvim_buf_get_name\bapi\bvim\1\4\0\0\21--stdin-filepath\0\19--single-quote\1\0\2\nstdin\2\bexe\rprettier{\0\0\5\0\a\0\f3\0\0\0002\1\3\0%\2\1\0004\3\2\0007\3\3\0037\3\4\3%\4\5\0>\3\2\2$\2\3\2;\2\1\1:\1\6\0H\0\2\0\targs\b%:t\vexpand\afn\bvim\22-assume-filename=\1\0\2\nstdin\2\bexe\17clang-formatD\0\0\2\0\3\0\0043\0\0\0003\1\1\0:\1\2\0H\0\2\0\targs\1\2\0\0\18--emit=stdout\1\0\2\nstdin\2\bexe\frustfmt(\0\0\1\0\1\0\0023\0\0\0H\0\2\0\1\0\2\bexe\15lua-format\nstdin\2\"\0\0\1\0\1\0\0023\0\0\0H\0\2\0\1\0\2\bexe\tyapf\nstdin\2>\0\0\2\0\3\0\0043\0\0\0003\1\1\0:\1\2\0H\0\2\0\targs\1\3\0\0\6-\f--quiet\1\0\2\nstdin\2\bexe\nisortY\0\0\2\0\3\0\0043\0\0\0003\1\1\0:\1\2\0H\0\2\0\targs\1\4\0\0\b-sl\19-g /dev/stderr\0162>/dev/null\1\0\2\nstdin\2\bexe\16latexindent\2\1\0\v\0\22\0,1\0\0\0001\1\1\0001\2\2\0001\3\3\0001\4\4\0001\5\5\0001\6\6\0004\a\a\0%\b\b\0>\a\2\0027\a\t\a3\b\n\0003\t\v\0002\n\3\0;\0\1\n:\n\f\t2\n\3\0;\0\1\n:\n\r\t2\n\3\0;\0\1\n:\n\14\t2\n\3\0;\2\1\n:\n\15\t2\n\3\0;\5\1\n;\4\2\n:\n\16\t2\n\3\0;\6\1\n:\n\17\t2\n\3\0;\1\1\n:\n\18\t2\n\3\0;\1\1\n:\n\19\t2\n\3\0;\3\1\n:\n\20\t:\t\21\b>\a\2\1G\0\1\0\rfiletype\blua\bcpp\6c\btex\vpython\trust\thtml\tjson\15javascript\1\0\0\1\0\1\flogging\1\nsetup\14formatter\frequire\0\0\0\0\0\0\0\0", "config", "formatter.nvim")
-- Config for: nvim-treesitter
try_loadstring("\27LJ\1\2Œ\b\0\0\a\0\28\0!4\0\0\0%\1\1\0>\0\2\0027\1\2\0003\2\4\0003\3\3\0:\3\5\0023\3\6\0:\3\a\0023\3\b\0:\3\t\0023\3\n\0003\4\v\0:\4\f\3:\3\r\0023\3\16\0003\4\14\0003\5\15\0:\5\f\4:\4\17\0033\4\18\0:\4\19\3:\3\20\0023\3\25\0003\4\21\0003\5\23\0003\6\22\0:\6\24\5:\5\f\4:\4\26\3:\3\27\2>\1\2\1G\0\1\0\16textobjects\vselect\1\0\0\aiF\1\0\15\aaC\17@class.outer\aas\21@statement.outer\ail\16@loop.inner\aad\19@comment.outer\aac\23@conditional.outer\aie\17@block.inner\aaf\20@function.outer\aiC\17@class.inner\ais\21@statement.inner\aal\16@loop.outer\aif\20@function.inner\aim\16@call.inner\aic\23@conditional.inner\aae\17@block.outer\aam\16@call.outer\1\0\4\bcpp$(function_definition) @function\vpython$(function_definition) @function\tjava#(method_declaration) @function\6c$(function_definition) @function\1\0\1\venable\2\rrefactor\26highlight_definitions\1\0\1\venable\2\17smart_rename\1\0\0\1\0\1\17smart_rename\bgrr\1\0\1\venable\2\26incremental_selection\fkeymaps\1\0\4\19init_selection\bgnn\22scope_incremental\bgrc\21node_incremental\bgrn\21node_decremental\bgrm\1\0\1\venable\2\vindent\1\0\1\venable\2\14highlight\1\0\2\21use_languagetree\2\venable\2\21ensure_installed\1\0\0\1\18\0\0\tbash\6c\bcpp\blua\bcss\vfennel\fhaskell\thtml\15javascript\tjson\njulia\nocaml\20ocaml_interface\vpython\trust\ttoml\15typescript\nsetup\28nvim-treesitter.configs\frequire\0", "config", "nvim-treesitter")
-- Config for: pdf-scribe.nvim
try_loadstring("\27LJ\1\2n\0\0\2\0\6\0\a4\0\0\0007\0\1\0%\1\3\0:\1\2\0%\1\5\0:\1\4\0G\0\1\0\19~/gdrive/notes\24pdfscribe_notes_dir\20~/gdrive/papers\22pdfscribe_pdf_dir\6g\bvim\0", "config", "pdf-scribe.nvim")
-- Config for: vim-vsnip
try_loadstring("\27LJ\1\2°\2\0\0\a\0\n\0\0304\0\0\0%\1\1\0>\0\2\0027\0\2\0003\1\3\0\16\2\0\0%\3\4\0%\4\5\0%\5\6\0\16\6\1\0>\2\5\1\16\2\0\0%\3\a\0%\4\5\0%\5\6\0\16\6\1\0>\2\5\1\16\2\0\0%\3\4\0%\4\b\0%\5\t\0\16\6\1\0>\2\5\1\16\2\0\0%\3\a\0%\4\b\0%\5\t\0\16\6\1\0>\2\5\1G\0\1\0?vsnip#jumpable(-1) ? '<Plug>(vsnip-jump-prev)' : '<S-Tab>'\f<S-Tab>\6s=vsnip#jumpable(1)  ? '<Plug>(vsnip-jump-next)' : '<Tab>'\n<Tab>\6i\1\0\2\texpr\2\fnoremap\1\bmap\17config.utils\frequire\0", "config", "vim-vsnip")
-- Config for: indentLine
try_loadstring("\27LJ\1\2—\1\0\0\2\0\a\0\t4\0\0\0007\0\1\0%\1\3\0:\1\2\0'\1\1\0:\1\4\0003\1\6\0:\1\5\0G\0\1\0\1\6\0\0\btex\rmarkdown\btxt\rstartify\vpacker\31indentLine_fileTypeExclude\22indentLine_faster\bâ”‚\20indentLine_char\6g\bvim\0", "config", "indentLine")
-- Config for: nvim-compe
try_loadstring("\27LJ\1\2ˆ\5\0\0\a\0\25\0<4\0\0\0%\1\1\0>\0\2\0027\0\2\0004\1\3\0007\1\4\1)\2\2\0:\2\5\0014\1\3\0007\1\4\1)\2\2\0:\2\6\0014\1\3\0007\1\4\1)\2\2\0:\2\a\0014\1\3\0007\1\4\1)\2\2\0:\2\b\0014\1\3\0007\1\4\1)\2\2\0:\2\t\0014\1\3\0007\1\4\1)\2\2\0:\2\n\0014\1\3\0007\1\4\1)\2\2\0:\2\v\0014\1\0\0%\2\f\0>\1\2\0027\1\r\0013\2\14\0003\3\15\0:\3\16\2>\1\2\0013\1\17\0\16\2\0\0%\3\18\0%\4\19\0%\5\20\0\16\6\1\0>\2\5\1\16\2\0\0%\3\18\0%\4\21\0%\5\22\0\16\6\1\0>\2\5\1\16\2\0\0%\3\18\0%\4\23\0%\5\24\0\16\6\1\0>\2\5\1G\0\1\0\25compe#close('<c-e>')\n<c-e>\26compe#confirm('<cr>')\t<cr>\21compe#complete()\n<c-c>\6i\1\0\3\texpr\2\vsilent\2\fnoremap\2\vsource\1\0\5\rnvim_lua\2\rnvim_lsp\2\vbuffer\2\nvsnip\2\tpath\2\1\0\4\14preselect\valways\15min_length\3\1\fenabled\2\ndebug\1\nsetup\ncompe\25loaded_compe_vim_lsp\25loaded_compe_vim_lsc\27loaded_compe_ultisnips\22loaded_compe_tags\23loaded_compe_spell\31loaded_compe_snippets_nvim\28loaded_compe_treesitter\6g\bvim\bmap\17config.utils\frequire\0", "config", "nvim-compe")
-- Config for: vim-easy-align
try_loadstring("\27LJ\1\2\1\0\0\a\0\a\0\f4\0\0\0%\1\1\0>\0\2\0027\0\2\0003\1\3\0\16\2\0\0003\3\4\0%\4\5\0%\5\6\0\16\6\1\0>\2\5\1G\0\1\0\22<Plug>(EasyAlign)\aga\1\3\0\0\6x\6n\1\0\2\fnoremap\1\vsilent\2\bmap\17config.utils\frequire\0", "config", "vim-easy-align")
-- Config for: vimtex
try_loadstring("\27LJ\1\2ú\3\0\0\2\0\21\0\0274\0\0\0007\0\1\0%\1\3\0:\1\2\0'\1\1\0:\1\4\0'\1\1\0:\1\5\0%\1\a\0:\1\6\0%\1\t\0:\1\b\0'\1\0\0:\1\n\0'\1\0\0:\1\v\0%\1\r\0:\1\f\0%\1\15\0:\1\14\0%\1\17\0:\1\16\0003\1\19\0:\1\18\0'\1\0\0:\1\20\0G\0\1\0\"vimtex_syntax_conceal_default\1\0\1\bfoo\2\26vimtex_syntax_conceal\r--unique(vimtex_view_general_options_latexmk&--unique file:@pdf\\#src:@line@tex vimtex_view_general_options\vokular\31vimtex_view_general_viewer(vimtex_view_forward_search_on_start\25vimtex_quickfix_mode\5\16tex_conceal\fpplatex\27vimtex_quickfix_method\28vimtex_complete_enabled\"vimtex_complete_recursive_bib\bnvr\29vimtex_compiler_progname\6g\bvim\0", "config", "vimtex")
-- Config for: vim-signify
try_loadstring("\27LJ\1\2Ö\1\0\0\2\0\n\0\0154\0\0\0007\0\1\0003\1\3\0:\1\2\0%\1\5\0:\1\4\0%\1\5\0:\1\6\0%\1\5\0:\1\a\0007\1\a\0:\1\b\0'\1\0\0:\1\t\0G\0\1\0\28signify_sign_show_count#signify_sign_delete_first_line\24signify_sign_delete\21signify_sign_add\bâ”‚\24signify_sign_change\1\2\0\0\bgit\21signify_vcs_list\6g\bvim\0", "config", "vim-signify")
-- Config for: vim-sneak
try_loadstring("\27LJ\1\2ü\5\0\0\b\0\29\0†\0014\0\0\0%\1\1\0>\0\2\0027\0\2\0004\1\3\0007\1\4\0013\2\5\0\16\3\0\0%\4\6\0%\5\a\0%\6\b\0\16\a\2\0>\3\5\1\16\3\0\0%\4\6\0%\5\t\0%\6\n\0\16\a\2\0>\3\5\1\16\3\0\0%\4\v\0%\5\a\0%\6\b\0\16\a\2\0>\3\5\1\16\3\0\0%\4\v\0%\5\t\0%\6\n\0\16\a\2\0>\3\5\1\16\3\0\0%\4\f\0%\5\a\0%\6\b\0\16\a\2\0>\3\5\1\16\3\0\0%\4\f\0%\5\t\0%\6\n\0\16\a\2\0>\3\5\1\16\3\0\0%\4\r\0%\5\14\0%\6\15\0\16\a\2\0>\3\5\1\16\3\0\0%\4\r\0%\5\16\0%\6\17\0\16\a\2\0>\3\5\1\16\3\0\0%\4\6\0%\5\18\0%\6\19\0\16\a\2\0>\3\5\1\16\3\0\0%\4\6\0%\5\20\0%\6\21\0\16\a\2\0>\3\5\1\16\3\0\0%\4\v\0%\5\18\0%\6\19\0\16\a\2\0>\3\5\1\16\3\0\0%\4\v\0%\5\20\0%\6\21\0\16\a\2\0>\3\5\1\16\3\0\0%\4\f\0%\5\18\0%\6\19\0\16\a\2\0>\3\5\1\16\3\0\0%\4\f\0%\5\20\0%\6\21\0\16\a\2\0>\3\5\1\16\3\0\0%\4\6\0%\5\22\0%\6\23\0\16\a\2\0>\3\5\1\16\3\0\0%\4\6\0%\5\24\0%\6\25\0\16\a\2\0>\3\5\1\16\3\0\0%\4\v\0%\5\22\0%\6\23\0\16\a\2\0>\3\5\1\16\3\0\0%\4\v\0%\5\24\0%\6\25\0\16\a\2\0>\3\5\1\16\3\0\0%\4\f\0%\5\22\0%\6\23\0\16\a\2\0>\3\5\1\16\3\0\0%\4\f\0%\5\24\0%\6\25\0\16\a\2\0>\3\5\1'\3\1\0:\3\26\1'\3\1\0:\3\27\1'\3\1\0:\3\28\1G\0\1\0\21sneak#use_ic_scs\16sneak#label\17sneak#s_next\18<Plug>Sneak_T\6T\18<Plug>Sneak_t\6t\18<Plug>Sneak_F\6F\18<Plug>Sneak_f\6f\18<Plug>Sneak_,\6\\\18<Plug>Sneak_;\6;\5\6o\6x\18<Plug>Sneak_S\6Z\18<Plug>Sneak_s\6z\6n\1\0\1\fnoremap\1\6g\bvim\bmap\17config.utils\frequire\0", "config", "vim-sneak")

-- Command lazy-loads
vim.cmd [[command! -nargs=* -range -bang -complete=file Make lua require("packer.load")({'vim-dispatch'}, { cmd = "Make", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]]
vim.cmd [[command! -nargs=* -range -bang -complete=file StartupTime lua require("packer.load")({'vim-startuptime'}, { cmd = "StartupTime", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]]
vim.cmd [[command! -nargs=* -range -bang -complete=file EnMasse lua require("packer.load")({'vim-enmasse'}, { cmd = "EnMasse", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]]
vim.cmd [[command! -nargs=* -range -bang -complete=file Gblame lua require("packer.load")({'vim-fugitive'}, { cmd = "Gblame", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]]
vim.cmd [[command! -nargs=* -range -bang -complete=file Gstatus lua require("packer.load")({'vim-fugitive'}, { cmd = "Gstatus", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]]
vim.cmd [[command! -nargs=* -range -bang -complete=file UndotreeToggle lua require("packer.load")({'undotree'}, { cmd = "UndotreeToggle", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]]
vim.cmd [[command! -nargs=* -range -bang -complete=file Start lua require("packer.load")({'vim-dispatch'}, { cmd = "Start", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]]
vim.cmd [[command! -nargs=* -range -bang -complete=file Gpull lua require("packer.load")({'vim-fugitive'}, { cmd = "Gpull", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]]
vim.cmd [[command! -nargs=* -range -bang -complete=file Gpush lua require("packer.load")({'vim-fugitive'}, { cmd = "Gpush", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]]
vim.cmd [[command! -nargs=* -range -bang -complete=file Dispatch lua require("packer.load")({'vim-dispatch'}, { cmd = "Dispatch", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]]
vim.cmd [[command! -nargs=* -range -bang -complete=file Focus lua require("packer.load")({'vim-dispatch'}, { cmd = "Focus", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]]
vim.cmd [[command! -nargs=* -range -bang -complete=file Sayonara lua require("packer.load")({'vim-sayonara'}, { cmd = "Sayonara", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]]
vim.cmd [[command! -nargs=* -range -bang -complete=file Prosession lua require("packer.load")({'vim-obsession'}, { cmd = "Prosession", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]]

vim.cmd [[augroup packer_load_aucmds]]
vim.cmd [[au!]]
  -- Event lazy-loads
vim.cmd [[au InsertEnter * ++once lua require("packer.load")({'nvim-colorizer.lua'}, { event = "InsertEnter *" }, _G.packer_plugins)]]
vim.cmd("augroup END")
END

catch
  echohl ErrorMsg
  echom "Error in packer_compiled: " .. v:exception
  echom "Please check your config for correctness"
  echohl None
endtry

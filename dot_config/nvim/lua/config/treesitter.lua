require('nvim-treesitter.configs').setup {
  auto_install = true,
  highlight = {
    enable = true,
    max_file_lines = 5000,
    disable = function(lang, buf)
      local max_filesize = 100 * 1024 -- 100 KB
      local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(buf))
      if ok and stats and stats.size > max_filesize then
        return true
      end
    end,
  },
  indent = { enable = false },
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = '<cr>',
      node_incremental = '<tab>',
      scope_incremental = '<cr>',
      scope_decremental = '<s-cr>',
      node_decremental = '<s-tab>',
    },
  },
  refactor = {
    smart_rename = { enable = true, keymaps = { smart_rename = 'grr' } },
    highlight_definitions = {
      enable = true,
      max_file_lines = 1000,
      disable = function(lang, buf)
        local max_filesize = 100 * 1024 -- 100 KB
        local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(buf))
        if ok and stats and stats.size > max_filesize then
          return true
        end
      end,
    },
  },
  textsubjects = {
    enable = true,
    lookahead = true,
    max_file_lines = 5000,
    keymaps = {
      ['.'] = 'textsubjects-smart',
      [';'] = 'textsubjects-container-outer',
      ['i;'] = 'textsubjects-container-inner',
    },
  },
  endwise = { enable = true, disable = { 'noice' } },
  matchup = {
    enable = true,
    include_match_words = true,
    enable_quotes = true,
    disable = function(lang, buf)
      if lang == 'noice' then
        return true
      end

      return false
    end,
  },
  -- autotag = { enable = true },
}

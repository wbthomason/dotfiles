local cmp = require 'cmp'
local luasnip = require 'luasnip'
luasnip.setup { region_check_events = 'InsertEnter', delete_check_events = 'InsertEnter' }
require('luasnip.loaders.from_vscode').lazy_load()

local has_words_before = function()
  local line, col = unpack(vim.api.nvim_win_get_cursor(0))
  return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match '%s' == nil
end

cmp.setup {
  completion = { completeopt = 'menu,menuone,noinsert' },
  sorting = {
    comparators = {
      -- function(entry1, entry2)
      --   local score1 = entry1.completion_item.score
      --   local score2 = entry2.completion_item.score
      --   if score1 and score2 then
      --     return (score1 - score2) < 0
      --   end
      -- end,

      -- The built-in comparators:
      cmp.config.compare.offset,
      cmp.config.compare.exact,
      cmp.config.compare.score,
      require('cmp-under-comparator').under,
      cmp.config.compare.kind,
      cmp.config.compare.sort_text,
      cmp.config.compare.length,
      cmp.config.compare.order,
    },
  },
  snippet = {
    expand = function(args)
      luasnip.lsp_expand(args.body)
    end,
  },
  window = {
    completion = {
      winhighlight = 'Normal:Pmenu,FloatBorder:Pmenu,Search:None',
      col_offset = -3,
      side_padding = 0,
    },
  },
  formatting = {
    fields = { 'kind', 'abbr', 'menu' },
    format = function(entry, vim_item)
      local kind = require('lspkind').cmp_format { mode = 'symbol_text', maxwidth = 50 }(entry, vim_item)
      local strings = vim.split(kind.kind, '%s', { trimempty = true })
      kind.kind = ' ' .. strings[1] .. ' '
      kind.menu = '    (' .. strings[2] .. ')'
      return kind
    end,
  },
  mapping = {
    ['<C-b>'] = cmp.mapping(cmp.mapping.scroll_docs(-4), { 'i', 'c' }),
    ['<C-f>'] = cmp.mapping(cmp.mapping.scroll_docs(4), { 'i', 'c' }),
    ['<C-Space>'] = cmp.mapping(cmp.mapping.complete(), { 'i', 'c' }),
    ['<C-y>'] = cmp.config.disable,
    ['<C-e>'] = cmp.mapping {
      i = cmp.mapping.abort(),
      c = cmp.mapping.close(),
    },
    ['<cr>'] = cmp.mapping.confirm { select = true, behavior = cmp.ConfirmBehavior.Replace },
    ['<tab>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      elseif luasnip.expand_or_jumpable() then
        luasnip.expand_or_jump()
      elseif has_words_before() then
        cmp.complete()
      else
        fallback()
      end
    end, { 'i', 's' }),
    ['<s-tab>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      elseif luasnip.jumpable(-1) then
        luasnip.jump(-1)
      else
        fallback()
      end
    end, { 'i', 's' }),
  },
  sources = {
    { name = 'nvim_lsp_signature_help' },
    { name = 'nvim_lsp' },
    { name = 'luasnip' },
    { name = 'nvim_lua' },
    { name = 'path' },
    { name = 'buffer' },
  },
}

cmp.setup.cmdline('/', {
  sources = { { name = 'buffer' } },
})

cmp.setup.cmdline(':', {
  sources = cmp.config.sources({ { name = 'path' } }, { { name = 'cmdline' } }),
})

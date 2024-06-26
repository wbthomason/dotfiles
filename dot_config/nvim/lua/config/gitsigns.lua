vim.api.nvim_set_hl(0, 'GitSignsAdd', { link = 'GreenSign' })
vim.api.nvim_set_hl(0, 'GitSignsAddNr', { link = 'GitSignsAdd' })
vim.api.nvim_set_hl(0, 'GitSignsChange', { link = 'BlueSign' })
vim.api.nvim_set_hl(0, 'GitSignsChangeNr', { link = 'GitSignsChange' })
vim.api.nvim_set_hl(0, 'GitSignsDelete', { link = 'RedSign' })
vim.api.nvim_set_hl(0, 'GitSignsDeleteNr', { link = 'GitSignsDelete' })
vim.api.nvim_set_hl(0, 'GitSignsTopdelete', { link = 'RedSign' })
vim.api.nvim_set_hl(0, 'GitSignsTopdeleteNr', { link = 'GitSignsTopdelete' })
vim.api.nvim_set_hl(0, 'GitSignsChangedelete', { link = 'PurpleSign' })
vim.api.nvim_set_hl(0, 'GitSignsChangedeleteNr', { link = 'GitSignsChangedelete' })
require('gitsigns').setup {
  signs = {
    add = { text = '│' },
    change = { text = '│' },
    delete = { text = '│' },
    topdelete = { text = '│' },
    changedelete = { text = '│' },
  },
}

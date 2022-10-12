require('gitsigns').setup {
  signs = {
    add = { hl = 'GreenSign', text = '│', numhl = 'GitSignsAddNr' },
    change = { hl = 'BlueSign', text = '│', numhl = 'GitSignsChangeNr' },
    delete = { hl = 'RedSign', text = '│', numhl = 'GitSignsDeleteNr' },
    topdelete = { hl = 'RedSign', text = '│', numhl = 'GitSignsDeleteNr' },
    changedelete = { hl = 'PurpleSign', text = '│', numhl = 'GitSignsChangeNr' },
  },
  keymaps = {},
}

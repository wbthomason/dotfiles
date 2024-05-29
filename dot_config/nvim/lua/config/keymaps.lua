-- Keybindings
-- Quit, close buffers, etc.
vim.keymap.set('n', '<leader>q', '<cmd>qa<cr>', { desc = 'Quit' })
vim.keymap.set('n', '<leader>x', '<cmd>x!<cr>', { desc = 'Save and quit' })
vim.keymap.set('n', '<leader>d', '<cmd>BufDel<cr>', { desc = 'Delete buffer', nowait = true })

-- Save buffer
vim.keymap.set('i', '<c-s>', '<esc><cmd>w<cr>a', { desc = 'Save' })
vim.keymap.set('n', '<leader>w', '<cmd>w<cr>', { desc = 'Save' })

-- Version control
vim.keymap.set('n', '<leader>g', '<cmd>Neogit<cr>', { desc = 'Neogit' })

-- Yank to clipboard
vim.keymap.set('n', 'y+', '<cmd>set opfunc=util#clipboard_yank<cr>g@', { desc = 'Yank to clipboard' })
vim.keymap.set('v', 'y+', '<cmd>set opfunc=util#clipboard_yank<cr>g@', { desc = 'Yank to clipboard' })

-- Window movement
vim.keymap.set('n', '<c-h>', '<c-w>h', { desc = 'Move to window at left' })
vim.keymap.set('n', '<c-j>', '<c-w>j', { desc = 'Move to window below' })
vim.keymap.set('n', '<c-k>', '<c-w>k', { desc = 'Move to window above' })
vim.keymap.set('n', '<c-l>', '<c-w>l', { desc = 'Move to window at right' })

-- Tab movement
vim.keymap.set('n', '<c-Left>', '<cmd>tabpre<cr>', { desc = 'Move to tab at left' })
vim.keymap.set('n', '<c-Right>', '<cmd>tabnext<cr>', { desc = 'Move to tab at right' })

-- Make relative line jumps store jumplist locations
vim.keymap.set('n', 'k', function()
  if vim.v.count > 1 then
    return [[m']] .. vim.v.count .. 'k'
  end

  return 'k'
end, { expr = true, silent = true })

vim.keymap.set('n', 'j', function()
  if vim.v.count > 1 then
    return [[m']] .. vim.v.count .. 'j'
  end

  return 'j'
end, { expr = true, silent = true })

vim.keymap.set('n', '<leader>s', function()
  MiniSessions.write(vim.fn.fnamemodify(vim.fn.getcwd(), ':t'), {})
end, {
  desc = 'Write current session',
})

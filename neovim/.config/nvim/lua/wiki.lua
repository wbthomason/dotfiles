local M = {}

function M.getCompletionItems(prefix)
  local items = {}
  if vim.fn.exists('g:wiki_loaded') == 1 then
    items = vim.api.nvim_call_function('wiki#complete#omnicomplete', {0, prefix})
  end

  return items
end

M.complete_item = {item = M.getCompletionItems}

return M

lua git = require('git')
function! git#update_changes(path) abort
  call luaeval('git.update(_A)', a:path)
endfunction

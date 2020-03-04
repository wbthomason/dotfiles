" Startify

let g:startify_bookmarks = [ '~/.config/nvim/', '~/.zshrc', '~/Dropbox/notes', '~/wiki' ]

let g:startify_lists = [
      \ { 'header': ['Commands'], 'type': 'commands' },
      \ { 'header': ['Sessions'], 'type': 'sessions' },
      \ { 'header': ['Recent Files in ' . substitute(getcwd(),$HOME,'~','')], 'type': 'dir' },
      \ { 'header': ['Recent Files'], 'type': 'files' },
      \ { 'header': ['Bookmarks'], 'type': 'bookmarks' }]

let g:startify_commands = [
      \ {'u': ['Update plugins', ':PackagerUpdate']},
      \ {'c': ['Clean plugins', ':PackagerClean']},
      \ {'t': ['Time startup', ':StartupTime']},
      \ {'s': ['Start Prosession', ':Prosession .']}]

let g:startify_session_persistence = 1
let g:startify_change_to_vcs_root = 1
let g:startify_custom_header = []

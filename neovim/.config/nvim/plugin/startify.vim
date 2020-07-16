" Startify

let g:startify_enable_unsafe = 1
let g:startify_lists = [
      \ { 'header': ['Commands'], 'type': 'commands' },
      \ { 'header': ['Sessions'], 'type': 'sessions' },
      \ { 'header': ['Recent Files in ' . substitute(getcwd(),$HOME,'~','')], 'type': 'dir' },
      \ { 'header': ['Recent Files'], 'type': 'files' }]

let g:startify_commands = [
      \ {'u': ['Update plugins', ':PackerUpdate']},
      \ {'c': ['Clean plugins', ':PackerClean']},
      \ {'t': ['Time startup', ':StartupTime']},
      \ {'s': ['Start Prosession', ':Prosession .']}]

let g:startify_session_persistence = 0
let g:startify_change_to_vcs_root = 1
let g:startify_custom_header = []

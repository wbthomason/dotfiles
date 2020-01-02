let g:clap_no_matches_msg = 'No matches'
let g:clap_current_selection_sign = {
      \ 'text': '=>',
      \ 'texthl': 'ClapCurrentSelection',
      \ 'linehl': 'ClapCurrentSelection'
      \}

let g:clap_selected_sign = {
      \ 'text': '->',
      \ 'texthl': 'ClapSelected',
      \ 'linehl': 'ClapSelected'
      \}

let g:clap_search_box_border_style = 'curve'

let g:clap_prompt_format = '%spinner% %provider_id% ❯  '
let g:clap_enable_icon = 1
let g:clap_provider_grep_enable_icon = 1
let g:clap_provider_grep_opts = '-H --no-heading --vimgrep --smart-case --hidden'

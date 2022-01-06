local parser_config = require('nvim-treesitter.parsers').get_parser_configs()
parser_config.org = {
  install_info = {
    url = 'https://github.com/milisims/tree-sitter-org',
    revision = 'f110024d539e676f25b72b7c80b0fd43c34264ef',
    files = { 'src/parser.c', 'src/scanner.cc' },
  },
  filetype = 'org',
}

require('nvim-treesitter.configs').setup {
  -- If TS highlights are not enabled at all, or disabled via `disable` prop, highlighting will fallback to default Vim syntax highlighting
  highlight = {
    enable = true,
    disable = { 'org' }, -- Remove this to use TS highlighter for some of the highlights (Experimental)
    additional_vim_regex_highlighting = { 'org' }, -- Required since TS highlighter doesn't support all syntax features (conceal)
  },
  ensure_installed = { 'org' }, -- Or run :TSUpdate org
}

require('orgmode').setup {
  org_agenda_files = { '~/notes/org/*', '~/projects/research/kavrakilab-research-log/**/*' },
  org_default_notes_file = '~/notes/org/notes.org',
  org_todo_keywords = { 'WAITING(w)', 'TODO(t)', 'IN-PROGRESS(p)', '|', 'DONE(d)', 'CANCELED(c)' },
  org_highlight_latex_and_related = 'entities',
}

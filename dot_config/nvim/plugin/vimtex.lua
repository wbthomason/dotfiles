local g = vim.g
g.vimtex_complete_recursive_bib = 1
g.vimtex_complete_enabled = 1
g.vimtex_quickfix_method = 'pplatex'
g.tex_conceal = ''
g.vimtex_quickfix_mode = 0
g.vimtex_view_forward_search_on_start = 0
g.vimtex_view_method = 'sioyek'
g.vimtex_view_general_options = [[--unique file:@pdf\#src:@line@tex]]
g.vimtex_compiler_latexrun = { options = { '-verbose-cmds', '--latex-args="-synctex=1"', '--bibtex-cmd=biber' } }
-- This must be a dictionary, and {} gets converted to a list
g.vimtex_syntax_conceal_disable = 1

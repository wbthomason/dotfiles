let g:capture_templates = {
      \ 'journal': {'file': '~/wiki/journal/journal.md',
      \             'pattern': '^# ',
      \             'new_snip': '# `!v strftime("%A, %F")`## `!v strftime("%R")` - ${1:title}${0:entry}',
      \             'extend_pattern': '.\+$',
      \             'extend_snip': '## `!v strftime("%R")` - ${1:title}${0:entry}'},
      \ 'research': {'file': '~/wiki/research.md',
      \             'pattern': '^# ',
      \             'new_snip': '# `!v strftime("%A, %F")`## `!v strftime("%R")` - ${1:title}${0:entry}',
      \             'extend_pattern': '.\+$',
      \             'extend_snip': '## `!v strftime("%R")` - ${1:title}${0:entry}'},
      \ 'advisor': {'file': '~/wiki/ross_meetings.md',
      \             'pattern': '^# ',
      \             'new_snip': '# `!v strftime("%A, %F")`${0:meeting_notes}'},
      \}

" Add wiki-ft link concealing
for [s:group, s:rx; s:contained] in [
      \ ['wikiLinkUrl',       'url',         'wikiLinkUrlConceal', 'mkdNonListItemBlock', 'mkdListItem', 'mkdListItemLine'],
      \ ['wikiLinkUrl',       'shortcite', 'mkdNonListItemBlock', 'mkdListItem', 'mkdListItemLine'],
      \ ['wikiLinkWiki',      'wiki',        'wikiLinkWikiConceal', 'mkdNonListItemBlock', 'mkdListItem', 'mkdListItemLine'],
      \ ['wikiLinkRef',       'ref_single', 'mkdNonListItemBlock', 'mkdListItem', 'mkdListItemLine'],
      \ ['wikiLinkRefTarget', 'ref_target',  'wikiLinkUrl', 'mkdNonListItemBlock', 'mkdListItem', 'mkdListItemLine'],
      \ ['wikiLinkRef',       'ref_double',  'wikiLinkRefConceal', 'mkdNonListItemBlock', 'mkdListItem', 'mkdListItemLine'],
      \ ['wikiLinkMd',        'md',          'wikiLinkMdConceal', 'mkdNonListItemBlock', 'mkdListItem', 'mkdListItemLine'],
      \ ['wikiLinkDate',      'date', 'mkdNonListItemBlock', 'mkdListItem', 'mkdListItemLine'],
      \]
  execute 'syntax cluster wikiLink  add=' . s:group
  execute 'syntax match' s:group
        \ '/' . wiki#link#get_matcher_opt(s:rx, 'rx') . '/'
        \ 'display contains=@NoSpell'
        \ . (empty(s:contained) ? '' : ',' . join(s:contained, ','))

  call filter(s:contained, 'v:val !~# ''Conceal''')
  execute 'syntax match' s:group . 'T'
        \ '/' . wiki#link#get_matcher_opt(s:rx, 'rx') . '/'
        \ 'display contained contains=@NoSpell'
        \ . (empty(s:contained) ? '' : ',' . join(s:contained, ','))
endfor

syntax match wikiLinkUrlConceal
      \ `\%(///\=[^/ \t]\+/\)\zs\S\+\ze\%([/#?]\w\|\S\{15}\)`
      \ cchar=~ contained transparent contains=NONE conceal
syntax match wikiLinkWikiConceal /\[\[\%(\/\|#\)\?\%([^\\\]]\{-}|\)\?/
      \ contained transparent contains=NONE conceal
syntax match wikiLinkWikiConceal /\]\]/
      \ contained transparent contains=NONE conceal
syntax match wikiLinkMdConceal /\[/
      \ contained transparent contains=NONE conceal
syntax match wikiLinkMdConceal /\]([^\\]\{-})/
      \ contained transparent contains=NONE conceal
syntax match wikiLinkRefConceal /[\]\[]\@<!\[/
      \ contained transparent contains=NONE conceal
syntax match wikiLinkRefConceal /\]\[[^\\\[\]]\{-}\]/
      \ contained transparent contains=NONE conceal

highlight default link wikiLinkUrl ModeMsg
highlight default link wikiLinkWiki Underlined
highlight default link wikiLinkMd Underlined
highlight default link wikiLinkRef Underlined
highlight default link wikiLinkRefTarget Underlined
highlight default link wikiLinkDate MoreMsg

unlet s:group s:rx s:contained

" To play nice with vim-markdown
syn match  mkdListItem     /^\s*\%([-*+]\|\d\+\.\)\ze\s\+/ contained contains=@wikiLink
syn region mkdListItemLine start="^\s*\%([-*+]\|\d\+\.\)\s\+" end="$" oneline contains=@mkdNonListItem,mkdListItem,@Spell,@wikiLink
syn region mkdNonListItemBlock start="\(\%^\(\s*\([-*+]\|\d\+\.\)\s\+\)\@!\|\n\(\_^\_$\|\s\{4,}[^ ]\|\t+[^\t]\)\@!\)" end="^\(\s*\([-*+]\|\d\+\.\)\s\+\)\@=" contains=@mkdNonListItem,@Spell,@wikiLink

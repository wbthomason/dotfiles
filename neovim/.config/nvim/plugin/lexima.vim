for c in ['(', "'", '"', '{', '[', '`']
  call lexima#add_rule({'at': '\%#\w', 'char': c, 'input': c})
endfor

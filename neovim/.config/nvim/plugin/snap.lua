local snap = require('snap')

snap.register.map('n', '<Leader>s', snap.create {
  prompt = 'Files',
  producer = snap.get('consumer.fzy')(snap.get('producer.git.file')),
  select = snap.get('select.file').select,
  multiselect = snap.get('select.file').multiselect,
  views = {snap.get('preview.file')}
})

snap.register.map('n', '<Leader><Leader>', snap.create {
  prompt = 'Files',
  producer = snap.get('consumer.fzy')(snap.get('producer.fd.file')),
  select = snap.get('select.file').select,
  multiselect = snap.get('select.file').multiselect,
  views = {snap.get('preview.file')}
})

snap.register.map('n', '<Leader>l', snap.create {
  prompt = 'Rg',
  producer = snap.get('consumer.limit')(10000, snap.get('producer.ripgrep.vimgrep')),
  select = snap.get('select.vimgrep').select,
  multiselect = snap.get('select.vimgrep').multiselect,
  views = {snap.get('preview.vimgrep')}
})

snap.register.map('n', '<leader>b', snap.create {
  producer = snap.get('consumer.fzy')(snap.get('consumer.combine')(snap.get('producer.vim.buffer'),
                                                                   snap.get('producer.vim.oldfile'))),
  select = snap.get'select.file'.select,
  multiselect = snap.get'select.file'.multiselect,
  views = {snap.get 'preview.file'}
})

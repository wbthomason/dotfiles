local snap = require('snap')
local fzy = snap.get 'consumer.fzy'
local producer_file = snap.get 'producer.ripgrep.file'
local select_file = snap.get 'select.file'
local preview_file = snap.get 'preview.file'

snap.register.map({"n"}, {"<Leader>s"}, function()
  snap.run {
    producer = fzy(snap.get 'producer.git.file'),
    select = snap.get'select.file'.select,
    multiselect = snap.get'select.file'.multiselect,
    views = {snap.get 'preview.file'}
  }
end)

snap.register.map({'n'}, {'<Leader><Leader>'}, function()
  snap.run({
    prompt = 'Files',
    producer = fzy(producer_file),
    select = select_file.select,
    multiselect = select_file.multiselect,
    views = {preview_file}
  })
end)

snap.register.map({'n'}, {'<Leader>l'}, function()
  snap.run {
    producer = snap.get'consumer.limit'(10000, snap.get'producer.ripgrep.vimgrep'),
    select = snap.get'select.vimgrep'.select,
    multiselect = snap.get'select.vimgrep'.multiselect,
    views = {snap.get 'preview.vimgrep'}
  }
end)

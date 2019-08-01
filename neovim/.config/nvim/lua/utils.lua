local utils = {}

utils.filter_files = function(file_list)
  local filtered_list = {}
  local unique_strings = {}
  local index = 1
  for _, v in pairs(file_list) do
    if unique_strings[v] == nil then
      unique_strings[v] = index
      filtered_list[index] = v
      index = index + 1
    end
  end
  return filtered_list
end

utils.icons = {}
utils.icons.default_symbol = ''
utils.icons.map = {
  ai       = '',
  awk      = '',
  bash     = '',
  bat      = '',
  bmp      = '',
  c        = '',
  cc       = '',
  clj      = '',
  cljc     = '',
  cljs     = '',
  coffee   = '',
  conf     = '',
  cp       = '',
  cpp      = '',
  csh      = '',
  css      = '',
  cxx      = '',
  d        = '',
  dart     = '',
  db       = '',
  diff     = '',
  dump     = '',
  edn      = '',
  ejs      = '',
  erl      = '',
  fish     = '',
  fs       = '',
  fsi      = '',
  fsscript = '',
  fsx      = '',
  gif      = '',
  go       = '',
  h        = '',
  hh       = '',
  hbs      = '',
  hpp      = '',
  hrl      = '',
  hs       = '',
  htm      = '',
  html     = '',
  hxx      = '',
  ico      = '',
  ini      = '',
  java     = '',
  jl       = '',
  jpeg     = '',
  jpg      = '',
  js       = '',
  json     = '',
  jsx      = '',
  ksh      = '',
  less     = '',
  lhs      = '',
  lisp     = 'λ',
  lsp      = 'λ',
  lua      = '',
  markdown = '',
  md       = '',
  ml       = 'λ',
  mli      = 'λ',
  mustache = '',
  php      = '',
  pl       = '',
  pm       = '',
  png      = '',
  pp       = '',
  ps1      = '',
  psb      = '',
  psd      = '',
  py       = '',
  pyc      = '',
  pyd      = '',
  pyo      = '',
  rb       = '',
  rkt      = 'λ',
  rlib     = '',
  rmd      = '',
  rs       = '',
  rss      = '',
  sass     = '',
  scala    = '',
  scss     = '',
  sh       = '',
  slim     = '',
  sln      = '',
  sql      = '',
  styl     = '',
  suo      = '',
  t        = '',
  ts       = '',
  tsx      = '',
  twig     = '',
  txt      = 'e',
  vim      = '',
  vue      = '﵂',
  xul      = '',
  yaml     = '',
  yml      = '',
  zsh      = '',
}

-- Because these extensions are dumb and have symbols
utils.icons.map['c++'] = ''
utils.icons.map['f#'] = ''

utils.icons.lookup = function(file_path)
  local extension = utils.filename_extension(file_path)
  return utils.icons.lookup_filetype(extension)
end

utils.icons.lookup_filetype = function(filetype)
  local icon = utils.icons.map[filetype]
  if icon == nil then
    icon = utils.icons.default_symbol
  end

  return icon
end

utils.filename_extension = function(file_path)
  return file_path:match('%.(%w+)$') or ''
end

utils.iconify = function(path)
  return utils.icons.lookup(path) .. ' ' .. path
end

utils.filter_and_iconify = function(file_list)
  local result = {}
  for _, path in ipairs(utils.filter_files(file_list)) do
    table.insert(result, utils.iconify(path))
  end

  return result
end

return utils

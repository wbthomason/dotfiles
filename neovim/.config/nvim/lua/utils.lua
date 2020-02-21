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
  ai          = '',
  apache      = '',
  awk         = '',
  bash        = '',
  bat         = '',
  bazel       = '',
  bib         = '',
  bmp         = '',
  c           = '',
  cc          = '',
  clisp       = '',
  clj         = '',
  cljc        = '',
  clojure     = '',
  cmake       = '',
  cobol       = '',
  coffee      = ' ',
  config      = '',
  coq         = '',
  cp          = '',
  cpp         = '',
  crystal     = '',
  csh         = '',
  csharp      = '',
  css         = '',
  cuda        = '',
  cxx         = '',
  cython      = '',
  d           = '',
  dart        = '',
  db          = '',
  diff        = '',
  dockerfile  = '',
  dump        = '',
  edn         = '',
  ejs         = '',
  elisp       = '',
  elixir      = '',
  elm         = '',
  erl         = '',
  fish        = '',
  fs          = '',
  fsi         = '',
  fsscript    = '',
  fsx         = '',
  gif         = '',
  git         = '',
  gnu         = '',
  go          = '',
  graphviz    = '',
  h           = '',
  hbs         = '',
  hh          = '',
  hpp         = '',
  hrl         = '',
  hs          = '',
  htm         = '',
  html        = '',
  hxx         = '',
  ico         = '',
  idris       = '',
  ini         = '',
  j           = '',
  jasmine     = '',
  java        = '',
  jl          = '',
  jpeg        = '',
  jpg         = '',
  js          = '',
  json        = '',
  jsx         = '',
  julia       = '⛬',
  jupyter     = '',
  kotlin      = '',
  ksh         = '',
  labview     = '',
  less        = '',
  lhs         = '',
  lisp        = 'λ',
  llvm        = '',
  lsp         = 'λ',
  lua         = '',
  m           = '',
  markdown    = '',
  mathematica = '',
  matlab      = '',
  max         = '',
  md          = '',
  meson       = '',
  ml          = '',
  mli         = '',
  mustache    = '',
  nginx       = '',
  nim         = '',
  nix         = '',
  nvcc        = '',
  nvidia      = '',
  octave      = '',
  opencl      = '',
  org         = '',
  patch       = '',
  perl6       = '',
  php         = '',
  pl          = '',
  png         = '',
  postgresql  = '',
  pp          = '',
  prolog      = '',
  ps          = '',
  ps1         = '',
  psb         = '',
  psd         = '',
  py          = '',
  pyc         = '',
  pyd         = '',
  pyo         = '',
  python      = '',
  rb          = '',
  react       = '',
  reason      = '',
  rkt         = '',
  rlib        = '',
  rmd         = '',
  rs          = '',
  rss         = '',
  ruby        = '',
  rust        = '',
  sass        = '',
  scala       = '',
  scheme      = 'λ',
  scm         = 'λ',
  scrbl       = '',
  scss        = '',
  sh          = '',
  slim        = '',
  sln         = '',
  sql         = '',
  styl        = '',
  suo         = '',
  svg         = '',
  swift       = '',
  t           = '',
  tex         = '',
  ts          = '',
  tsx         = '',
  twig        = '',
  txt         = 'e',
  typescript  = '',
  vim         = '',
  vue         = '﵂',
  xul         = '',
  yaml        = '',
  yml         = '',
  zsh         = '',
}

-- Because these extensions are dumb and have symbols
utils.icons.map['c++'] = ''
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

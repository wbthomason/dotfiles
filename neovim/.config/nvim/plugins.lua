local iron = require('iron')
iron.core.set_config {
  preferred = {
    python  = 'ptipython',
    haskell = 'intero',
    lisp    = 'sbcl',
    ocaml   = 'utop',
    scheme  = 'csi'
  }
}

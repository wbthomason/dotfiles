local iron = require('iron')

iron.core.set_config {
  preferred = {
    python = "ptpython",
    clojure = "lein"
  }
}

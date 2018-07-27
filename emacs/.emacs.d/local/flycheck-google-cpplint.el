;;; flycheck-google-cpplint.el --- Help to comply with the Google C++ Style Guide

;; Copyright (C) 2014  Akiha Senda

;; Author: Akiha Senda <senda.akiha@gmail.com>
;; URL: https://github.com/flycheck/flycheck-google-cpplint/
;; Version: 1.0.1
;; Keywords: flycheck, C, C++
;; Package-Requires: ((flycheck "0.20-cvs1"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is extension for Flycheck.

;; If you're want to write code according to the Google C++ Style Guide,
;; this will help a great deal.
;; http://google-styleguide.googlecode.com/svn/trunk/cppguide.xml

;; I recommend that the package google-c-style also installed with.
;; http://melpa.milkbox.net/#/google-c-style

;; For more infomations, please check the GitHub
;; https://github.com/senda-akiha/flycheck-google-cpplint/

;;;; Setup

;; (eval-after-load 'flycheck
;;   '(progn
;;      (require 'flycheck-google-cpplint)
;;      ;; Add Google C++ Style checker.
;;      ;; In default, syntax checked by Clang and Cppcheck.
;;      (flycheck-add-next-checker 'c/c++-clang
;;                                 'c/c++-googlelint 'append)))

;;; Code:

(require 'flycheck)

(flycheck-def-option-var flycheck-googlelint-verbose nil c/c++-googlelint
  "The verbosity level for Google C++ lint.

verbose=#
  Specify a number 0-5 to restrict errors to certain verbosity levels."
  :type '(string :tag "Verbosity level")
  :safe #'stringp
  :package-version '(flycheck . "0.18"))

(flycheck-def-option-var flycheck-googlelint-filter nil c/c++-googlelint
  "The category-filters for Google C++ lint.

filter=-x,+y,...
  Specify a comma-separated list of category-filters to apply: only
  error messages whose category names pass the filters will be printed.
  (Category names are printed with the message and look like
  '[whitespace/indent]'.)  Filters are evaluated left to right.
  '-FOO' and 'FOO' means 'do not print categories that start with FOO'.
  '+FOO' means 'do print categories that start with FOO'.

  Examples: --filter=-whitespace,+whitespace/braces
            --filter=whitespace,runtime/printf,+runtime/printf_format
            --filter=-,+build/include_what_you_use

  To see a list of all the categories used in cpplint, pass no arg:
    --filter="
  :type '(string :tag "Filtering error messages")
  :safe #'stringp
  :package-version '(flycheck . "0.18"))

(flycheck-def-option-var flycheck-googlelint-root nil c/c++-googlelint
  "The root directory for Google C++ lint.

root=subdir
  The root directory used for deriving header guard CPP variable.
  By default, the header guard CPP variable is calculated as the relative
  path to the directory that contains .git, .hg, or .svn.  When this flag
  is specified, the relative path is calculated from the specified
  directory. If the specified directory does not exist, this flag is
  ignored.

  Examples:
    Assuing that src/.git exists, the header guard CPP variables for
    src/chrome/browser/ui/browser.h are:

    No flag => CHROME_BROWSER_UI_BROWSER_H_
    --root=chrome => BROWSER_UI_BROWSER_H_
    --root=chrome/browser => UI_BROWSER_H_"
  :type '(choice (const :tag "Default root directory" nil)
                 (string :tag "Directory name"))
  :safe #'stringp)

(flycheck-def-option-var flycheck-googlelint-linelength nil c/c++-googlelint
  "The allowed line length for Google C++ lint.

linelength=digits
  This is the allowed line length for the project. The default value is
  80 characters.

  Examples:
    --linelength=120"
  :type '(string :tag "Line length")
  :safe #'stringp
  :package-version '(flycheck . "0.18"))

(flycheck-define-checker c/c++-googlelint
  "A C/C++ style checker using google cpplint.

See URL
`http://google-styleguide.googlecode.com/svn/trunk/cpplint/cpplint.py'."
  :command ("cpplint.py"
            (option "--verbose=" flycheck-googlelint-verbose concat)
            (option "--filter=" flycheck-googlelint-filter concat
                    flycheck-option-comma-separated-list)
            (option "--root=" flycheck-googlelint-root concat)
            (option "--linelength=" flycheck-googlelint-linelength concat)
            source-original)
  :error-patterns
  ((warning line-start (file-name) ":" line ":  " (message) line-end))
  :modes (c-mode c++-mode))

(add-to-list 'flycheck-checkers 'c/c++-googlelint 'append)

(provide 'flycheck-google-cpplint)

;;; flycheck-google-cpplint.el ends here

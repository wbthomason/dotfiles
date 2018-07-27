;;; pddl-mode.el --- A Planning and Domain Definition Language editing mode    -*-coding: iso-8859-1;-*-

;; Copyright (C) 2005 Surendra K Singhi

;; Authors: 2005      Surendra K Singhi <surendra@asu.edu>
;; Keywords: PDDL Planning files 
;; Version: 0.100
;; URL: http://www.public.asu.edu/~sksinghi/PDDL-mode.htm

;;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Purpose:
;;
;; To provide a pleasant mode to browse and edit PDDL files.
;;It provides syntax highlighting with automatic indentation,
;;templates, auto-completion, and a Declaration imenu which
;;list all the actions and the problems in the current file.
;;
;;
;; This mode supports full PDDL 2.2 
;;
;; Installation:
;; 
;; Put in your ~/.emacs:
;;      (add-to-list 'load-path "/lib/emacs/PDDL-mode")
;;      (require 'PDDL-mode)
;;
;; Version 0.100: PDDL-mode released
;; History:
;;
;; This mode was written by Surendra Singhi in February 2005.
;; Special thanks also goes to Stefan Monnier <monnier@iro.umontreal.ca> for
;; helping me with various parts of the code
;; If you have any problems or suggestions or patches specific to the mode
;;please contact the author via email.  
;;
;;

;;; Code:

(require 'easymenu)
(require 'font-lock)
(require 'regexp-opt)
(require 'custom)
(require 'lisp-mode)
(require 'pcomplete)
(require 'imenu)

(defvar PDDL-mode-hook nil)

(defvar PDDL-mode-map
  (let ((PDDL-mode-map (make-sparse-keymap)))
    (define-key PDDL-mode-map [return] 'newline-and-indent)
    (define-key PDDL-mode-map [tab] 'pcomplete)
    (define-key PDDL-mode-map '[(control t) (a)] 'PDDL-mode-tempate-insert-action)
    (define-key PDDL-mode-map '[(control t) (p)] 'PDDL-mode-tempate-insert-problem)
    (define-key PDDL-mode-map '[(control t) (d)] 'PDDL-mode-tempate-insert-domain)
    (define-key PDDL-mode-map '[(control t) (e)] 'PDDL-mode-temp)
    PDDL-mode-map)
  "Keymap for PDDL major mode")

(add-to-list 'auto-mode-alist '("\\.PDDL\\'".PDDL-mode))

(defconst PDDL-font-lock-keywords-1
  (list (cons "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\);+.*$"
	      'font-lock-comment-face)
	(cons "\\(\\W\\|^\\)\\?\\w*" 'font-lock-variable-name-face)
	(cons (regexp-opt '(":requirements" ":types" ":constants" ":predicates"
			    ":action" ":domain" ":parameters" ":effect"
			    ":precondition" ":objects" ":init" ":goal"
			    ":functions" ":duration" ":condition" ":derived"
			    ":metric") t)
	      'font-lock-builtin-face))
  "Minimal highlighting expressions for PDDL mode")

(defconst PDDL-font-lock-keywords-2
  (append PDDL-font-lock-keywords-1
	  (list (cons (regexp-opt '("define" "and" "or" "not" "problem"
				    "domain" "either" "exists" "forall"
				    "when" "assign" "scale-up" "scale-down"
				    "increase" "decrease" "start" "end" "all"
				    "at" "over" "minimize" "maximize"
				    "total-time") 'words)
		      'font-lock-keyword-face)))
  "Additional Keywords to highlight in PDDL mode")

(defconst PDDL-font-lock-keywords-3
  (append PDDL-font-lock-keywords-2
	  (list (cons (regexp-opt '(":strips" ":typing" ":equality" ":adl"
				    ":negative-preconditions" ":durative-actions"
				    ":disjunctive-preconditiorns" ":fluents"
				    ":existential-preconditions"
				    ":derived-predicates"
				    ":universal-preconditions"
				    ":timed-initial-literals") t)
		      'font-lock-constant-face)))
    "Additional Keywords to highlight in PDDL mode")
		       
(defvar PDDL-font-lock-keywords PDDL-font-lock-keywords-3
  "Default highlighting expressions for PDDL mode is maximum.")

(defconst PDDL-mode-syntax-table
   (copy-syntax-table lisp-mode-syntax-table)
  "Syntax table for PDDL mode is same as lisp mode syntax table.")

(define-skeleton PDDL-mode-tempate-insert-action
  "Inserts the template for an action definition."
  nil
  > "(:action " (skeleton-read "action name?") \n
  > ":parameters("_")" \n > ":precondition()" \n > ":effect())")

(define-skeleton PDDL-mode-tempate-insert-domain
  "Inserts the template for a domain definition."
  nil
  > "(define (domain " (skeleton-read "domain name?") ")"\n
  > "(:requirements :strips " _ ")" \n > "(:predicates )" \n > ")")

(define-skeleton PDDL-mode-tempate-insert-problem
  "Inserts the template for a problem definition."
  nil
  >"(define (problem " (skeleton-read "problem name?") ")"\n
  > "(:domain " (skeleton-read "domain name?") ")" \n
  > "(:init " _ ")" \n > "(:goal ))" \n)


(easy-menu-define PDDL-mode-menu PDDL-mode-map
     "Menu for PDDL mode."
     '("PDDL"
       ("Templates" ["Insert domain" PDDL-mode-tempate-insert-domain :active t :keys "C-t d"]
	["Insert problem" PDDL-mode-tempate-insert-problem :active t :keys "C-t p" ]
	["Insert action" PDDL-mode-tempate-insert-action :active t :keys "C-t a"])))

(defvar PDDL-mode-imenu-generic-expression
  '(("Actions" "(\\s-*\\:action\\s-*\\(\\s\w+\\)" 1)
    ("Problems" "(\\s-*problem\\s-*\\(\\s\w+\\)" 1)))

(defvar PDDL-mode-imenu-syntax-alist '(("_-" . "w")))

(defvar PDDL-mode-all-completions 
  '(":strips" ":typing" ":equality" ":adl" ":negative-preconditions" ":durative-actions"
   ":disjunctive-preconditiorns" ":fluents" ":existential-preconditions" ":derived-predicates"
   ":universal-preconditions" ":timed-initial-literals" "define" "and" "or" "not" "problem"
   "domain" "either" "exists" "forall" "when" "assign" "scale-up" "scale-down" "increase"
   "decrease" "start" "end" "all" "at" "over" "minimize" "maximize" "total-time")
  "The list of possible completions.")


(defun pcomplete-PDDL-mode-setup ()
  "Function to setup the pcomplete variables."
 (interactive)
  (set (make-local-variable 'pcomplete-parse-arguments-function)
       'pcomplete-parse-PDDL-mode-arguments)
  (set (make-local-variable 'pcomplete-default-completion-function)
       'pcomplete-PDDL-mode-default-completion))

(defun pcomplete-PDDL-mode-default-completion()
  (pcomplete-here PDDL-mode-all-completions))

(defun pcomplete-parse-PDDL-mode-arguments ()
  (save-excursion
    (let* ((thispt (point))
	   (pt (search-backward-regexp "[ \t\n]" nil t))
	   (ptt (if pt (+ pt 1) thispt)))
      (list
       (list "dummy" (buffer-substring-no-properties ptt thispt))
       (point-min) ptt))))

(define-derived-mode PDDL-mode fundamental-mode "PDDL";  ;lisp-mode "PDDL"   ;
  "Major mode for editing PDDL files"
  (set (make-local-variable 'comment-start) ";")
  ;everything that begins with ';' is comment and remember \; is not a comment but \\; is
  (set (make-local-variable 'comment-start-skip) "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\);+ *")
  (set (make-local-variable 'font-lock-defaults) '(PDDL-font-lock-keywords nil t))
  (set-syntax-table PDDL-mode-syntax-table)
  (set (make-local-variable 'indent-line-function) 'lisp-indent-line)
  (easy-menu-add PDDL-mode-menu PDDL-mode-map)
  (pcomplete-PDDL-mode-setup)
  (setq imenu-generic-expression PDDL-mode-imenu-generic-expression
	imenu-case-fold-search nil
	imenu-syntax-alist PDDL-mode-imenu-syntax-alist)
  (imenu-add-to-menubar "Declarations"))

(provide 'PDDL-mode)
(provide 'pddl-mode)
;;; pddl-mode.el ends here

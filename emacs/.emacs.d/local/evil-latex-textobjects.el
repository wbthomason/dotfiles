;;; evil-latex-textobjects.el --- LaTeX text objects for evil

;; Copyright (C) 2015  Hans-Peter Deifel

;; Author: Hans-Peter Deifel <hpd@hpdeifel.de>
;; Keywords: tex, wp, convenience, vi, evil
;; Version: 1.0-git
;; Package-Requires: ((evil "1.0") (auctex "11.88"))

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
;;;
;;; Provides a minor mode that installs several additional LaTeX
;;; specific text objects for evil-mode:
;;; 
;;;  \	Display math		\[ .. \]
;;;  $	Inline math		$ .. $
;;;  m	TeX macro		\foo{..}
;;;  e	LaTeX environment	\begin{foo}..\end{foo}
;;;
;;; To enable this mode in LaTeX buffers, add this to your init file:
;;;
;;; (require 'evil-latex-textobjects)
;;; (add-hook 'LaTeX-mode-hook 'turn-on-evil-latex-textobjects-mode)

;;; Code:

(require 'evil)
(require 'latex)

(evil-define-text-object evil-latex-textobjects-inner-dollar (count &optional beg end type)
  "Select inner dollar"
  :extend-selection nil
  (evil-select-quote ?$ beg end type count nil))

(evil-define-text-object evil-latex-textobjects-a-dollar (count &optional beg end type)
  "Select a dollar"
  :extend-selection t
  (evil-select-quote ?$ beg end type count t))

(evil-define-text-object evil-latex-textobjects-inner-math (count &optional beg end type)
  "Select innter \\[ \\] or \\( \\)."
  :extend-selection nil
  (evil-select-paren "\\\\\\[\\|\\\\(" "\\\\)\\|\\\\\\]" beg end type count nil))

(evil-define-text-object evil-latex-textobjects-a-math (count &optional beg end type)
  "Select a \\[ \\] or \\( \\)."
  :extend-selection nil
  (evil-select-paren "\\\\\\[\\|\\\\(" "\\\\)\\|\\\\\\]" beg end type count t))

(defun evil-latex-textobjects-macro-beginning ()
  "Return (start . end) of the macro-beginning to the left of point.

If no enclosing macro is found, return nil.
For example for \macro{foo|bar} it returns the start and end of \"\macro{\""
  (let ((beg (TeX-find-macro-start)))
    (when beg
      (save-excursion
        (goto-char beg)
        (forward-char)                  ; backslash
        (skip-chars-forward "A-Za-z@*") ; macro-name
        (when (looking-at "{\\|\\[")
          (forward-char))                ; opening brace
        (cons beg (point))))))

(defun evil-latex-textobjects-macro-end ()
  "Return (start . end) of the end of the enclosing macro.

If no such macro can be found, return nil"
  (let ((end (TeX-find-macro-end)))
    (when end
      (save-excursion
        (goto-char end)
        (when (looking-back "}\\|\\]")
          (backward-char))               ; closing brace
        (cons (point) end)))))

;; TODO Support visual selection
;; TODO Support count

(evil-define-text-object evil-latex-textobjects-a-macro (count &optional beg end type)
  "Select a TeX macro"
  :extend-selection nil
  (let ((beg (evil-latex-textobjects-macro-beginning))
        (end (evil-latex-textobjects-macro-end)))
    (if (and beg end)
        (list (car beg) (cdr end))
      (error "No enclosing macro found"))))

(evil-define-text-object evil-latex-textobjects-inner-macro (count &optional beg end type)
  "Select inner TeX macro"
  :extend-selection nil
  (let ((beg (evil-latex-textobjects-macro-beginning))
        (end (evil-latex-textobjects-macro-end)))
    (cond
     ((or (null beg) (null end))
      (error "No enclosing macro found"))
     ((= (cdr beg) (car end))           ; macro has no content
      (list (1+ (car beg))              ; return macro boundaries excluding \
            (cdr beg)))
     (t (list (cdr beg) (car end))))))

(defun evil-latex-textobjects-env-beginning ()
  "Return (start . end) of the \\begin{foo} to the left of point."
  (let (beg)
    (save-excursion
      (LaTeX-find-matching-begin)       ; we are at backslash
      (setq beg (point))
      (skip-chars-forward "^{")         ; goto opening brace
      (forward-sexp)                    ; goto closing brace
      ;; Count the newline after \begin{foo} to the environment header
      ;; Without this, delete-inner-env would unexpectedly move the end
      ;; to the same line as the beginning
      ;; (when (looking-at "[[:blank:]]*$")
      ;;   (message "Newline")
      ;;   (forward-line 1))
      (cons beg (point)))))

(defun evil-latex-textobjects-env-end ()
  "Return (start . end) of the \\end{foo} to the right of point."
  (let (end)
    (save-excursion
      (LaTeX-find-matching-end)         ; we are at closing brace
      (setq end (point))
      (backward-sexp)                   ; goto opening brace
      (search-backward "\\")            ; goto backslash
      (cons (point) end))))


(evil-define-text-object evil-latex-textobjects-an-env (count &optional beg end type)
  "Select a LaTeX environment"
  :extend-selection nil
  (let ((beg (evil-latex-textobjects-env-beginning))
        (end (evil-latex-textobjects-env-end)))
    (list (car beg) (cdr end))))

(evil-define-text-object evil-latex-textobjects-inner-env (count &optional beg end type)
  "Select a LaTeX environment"
  :extend-selection nil
  (let ((beg (evil-latex-textobjects-env-beginning))
        (end (evil-latex-textobjects-env-end)))
    (list (cdr beg) (car end))))

(defvar evil-latex-textobjects-outer-map (make-sparse-keymap))
(defvar evil-latex-textobjects-inner-map (make-sparse-keymap))

(set-keymap-parent evil-latex-textobjects-outer-map evil-outer-text-objects-map)
(set-keymap-parent evil-latex-textobjects-inner-map evil-inner-text-objects-map)

(define-key evil-latex-textobjects-inner-map "$" 'evil-latex-textobjects-inner-dollar)
(define-key evil-latex-textobjects-outer-map "$" 'evil-latex-textobjects-a-dollar)
(define-key evil-latex-textobjects-inner-map "\\" 'evil-latex-textobjects-inner-math)
(define-key evil-latex-textobjects-outer-map "\\" 'evil-latex-textobjects-a-math)
(define-key evil-latex-textobjects-outer-map "m" 'evil-latex-textobjects-a-macro)
(define-key evil-latex-textobjects-inner-map "m" 'evil-latex-textobjects-inner-macro)
(define-key evil-latex-textobjects-outer-map "e" 'evil-latex-textobjects-an-env)
(define-key evil-latex-textobjects-inner-map "e" 'evil-latex-textobjects-inner-env)

;;;###autoload
(define-minor-mode evil-latex-textobjects-mode
  "Minor mode for latex-specific text objects in evil.

Installs the following additional text objects:
\\<evil-latex-textobjects-outer-map>
  \\[evil-latex-textobjects-a-math]\tDisplay math\t\t\\=\\[ .. \\=\\]
  \\[evil-latex-textobjects-a-dollar]\tInline math\t\t$ .. $
  \\[evil-latex-textobjects-a-macro]\tTeX macro\t\t\\foo{..}
  \\[evil-latex-textobjects-an-env]\tLaTeX environment\t\\begin{foo}..\\end{foo}"
  :keymap (make-sparse-keymap)
  (evil-normalize-keymaps))

(evil-define-key 'operator evil-latex-textobjects-mode-map
  "a" evil-latex-textobjects-outer-map
  "i" evil-latex-textobjects-inner-map)

(evil-define-key 'visual evil-latex-textobjects-mode-map
  "a" evil-latex-textobjects-outer-map
  "i" evil-latex-textobjects-inner-map)

;;;###autoload
(defun turn-on-evil-latex-textobjects-mode ()
  "Enable evil-latex-textobjects-mode in current buffer."
  (interactive "")
  (evil-latex-textobjects-mode 1))

;;;###autoload
(defun turn-off-evil-latex-textobjects-mode ()
  "Disable evil-latex-textobjects-mode in current buffer."
  (interactive "")
  (evil-latex-textobjects-mode -1))


(provide 'evil-latex-textobjects)

;;; evil-latex-textobjects.el ends here

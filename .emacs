;; packages
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(defun require-package (package)
  (setq-default highlight-tabs t)
  "Install given PACKAGE."
  (unless (package-installed-p package)
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))
(package-initialize)

;; Theme
(load-theme 'ample-zen t)

;; Evil mode
(require 'evil)
(evil-mode 1)

;; Evil surround
(require 'evil-surround)
(global-evil-surround-mode 1)

;; Powerline
(require 'powerline)
(powerline-center-evil-theme)

;; Evil args
(require 'evil-args)
;; bind evil-args text objects
(define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
(define-key evil-outer-text-objects-map "a" 'evil-outer-arg)
;; bind evil-forward/backward-args
(define-key evil-normal-state-map "L" 'evil-forward-arg)
(define-key evil-normal-state-map "H" 'evil-backward-arg)
(define-key evil-motion-state-map "L" 'evil-forward-arg)
(define-key evil-motion-state-map "H" 'evil-backward-arg)
;; bind evil-jump-out-args
(define-key evil-normal-state-map "K" 'evil-jump-out-args)

;; Evil visualstar
(global-evil-visualstar-mode)

;; Evil tabs
(global-evil-tabs-mode t)

;; Evil exchange
(require 'evil-exchange)
;; change default key bindings (if you want) HERE
;; (setq evil-exchange-key (kbd "zx"))
(evil-exchange-install)

;; Evil matchit
(require 'evil-matchit)
(global-evil-matchit-mode 1)

;; Evil mark-replace
(require 'evil-mark-replace)

;; Evil nerd commenter
(evilnc-default-hotkeys)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("1db337246ebc9c083be0d728f8d20913a0f46edc0a00277746ba411c149d7fe5" "91faf348ce7c8aa9ec8e2b3885394263da98ace3defb23f07e0ba0a76d427d46" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Helm
(helm-mode 1)

;; Smooth-scroll
(setq scroll-margin 5
      scroll-conservatively 9999
      scroll-step 1)

;; NLinum
;; Preset width nlinum
(add-hook 'nlinum-mode-hook
	  (lambda ()
	    (setq nlinum--width
		  (length (number-to-string
			   (count-lines (point-min) (point-max)))))))
(global-linum-mode 1)

;; Cask
(let ((default-directory "/usr/local/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;; flycheck
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

(after 'flycheck
       (setq flycheck-check-syntax-automatically '(save mode-enabled))
       (setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers))
       (setq flycheck-checkers (delq 'html-tidy flycheck-checkers))
       (setq flycheck-standard-error-navigation nil))

(global-flycheck-mode t)

;; esc quits
(defun minibuffer-keyboard-quit ()
    (interactive)
    (if (and delete-selection-mode transient-mark-mode mark-active)
	(setq deactivate-mark  t)
      (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
      (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)


;; Remove UI
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(setq bell-volume 0)
(define-key global-map (kbd "RET") 'newline-and-indent)
(setq require-final-newline nil)

(global-auto-revert-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(global-hl-line-mode 1)
(setq-default indent-tabs-mode nil)
(setq tab-width 2)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; packages
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))
(defun require-package (package)
  (setq-default highlight-tabs t)
  "Install given PACKAGE."
  (unless (package-installed-p package)
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))
(package-initialize)

(elscreen-start)
;; Theme
(load-theme 'airline-dark t)

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
(let ((default-directory "/usr/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;; flycheck
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)
(eval-after-load 'flycheck
       '(progn (setq flycheck-check-syntax-automatically '(save mode-enabled))
       (setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers))
       (setq flycheck-checkers (delq 'html-tidy flycheck-checkers))
       (setq flycheck-standard-error-navigation nil)))

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

(color-theme-approximate-on)

;; Paste like vim
(define-key evil-normal-state-map ";" 'evil-ex)
(define-key evil-normal-state-map "p" 'paste-newline)
(defun paste-newline (&optional arguments)
  "Pastes like vim does - on a new line."
    (interactive) (evil-ret) (evil-paste-before nil))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("fbcdb6b7890d0ec1708fa21ab08eb0cc16a8b7611bb6517b722eba3891dfc9dd" "878e22a7fe00ca4faba87b4f16bc269b8d2be5409d1c513bb7eda025da7c1cf4" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Company mode
(add-hook 'after-init-hook 'global-company-mode)

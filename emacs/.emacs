(require 'package)
(setq package-archives
      '(("org"       . "http://orgmode.org/elpa/")
        ("gnu"       . "http://elpa.gnu.org/packages/")
        ("melpa"     . "https://melpa.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")))
(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; General Packages
;;; Evil!
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration nil)
  (setq evil-want-fine-undo t)
  (setq evil-want-C-i-jump nil)
  (setq evil-disable-insert-state-bindings t)
  :config
  (evil-mode 1))

(use-package evil-collection :ensure t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode t))

(use-package evil-leader
  :ensure t
  :config
  (global-evil-leader-mode))

(use-package evil-commentary
  :ensure t
  :config
  (evil-commentary-mode))

(use-package evil-matchit
  :ensure t
  :config
  (global-evil-matchit-mode t))

(use-package evil-visual-mark-mode
  :ensure t
  :config
  (evil-visual-mark-mode))

(use-package evil-args
  :ensure t)

(use-package evil-visualstar
  :ensure t
  :config
  (global-evil-visualstar-mode t))

(use-package evil-escape
  :ensure t
  :diminish
  (evil-escape-mode)
  :config
  (evil-escape-mode)
  (setq-default evil-escape-key-sequence "jk"
                evil-escape-delay 0.2)
  (setq evil-escape-unordered-key-sequence t))

(use-package evil-magit :ensure t)

;;; Relative linum
(use-package linum-relative :ensure t :init)
;;; Once Emacs 26 is out
;;; (setq linum-relative-backend 'display-line-numbers-mode)

;;; Indentation guides
(use-package highlight-indent-guides :ensure t
  :init
  :config
  (setq highlight-indent-guides-method 'column)
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

;;; Helm
(use-package helm :ensure t)

;;; Projectile
(use-package projectile :ensure t)

;;; Magit
(use-package magit :ensure t)

;;; General mode
(use-package general
  :ensure t
  :config
  (setq general-override-states '(insert
                                  emacs
                                  hybrid
                                  normal
                                  visual
                                  motion
                                  operator
                                  replace))
  (general-override-mode)
  (general-define-key
   :states '(normal visual motion)
   :keymaps 'override
   "SPC" 'hydra-space/body))

;;; which-key
(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  (setq which-key-sort-order 'which-key-key-order-alpha)
  (setq which-key-popup-type 'side-window
        which-key-side-window-max-width 0.33
        which-key-idle-delay 0.5)
  )

;; Languages
;;; OCaml
;;;; OCP-indent
(eval-and-compile
  (defun opam-share ()
    (substring
     (shell-command-to-string "opam config var share 2> /dev/null")
     0 -1)))

(use-package ocp-indent :defer t :ensure t
  :load-path (lambda () ( concat ( opam-share ) "/emacs/site-lisp") )
  )

;;;; Tuareg
(use-package tuareg :defer t :ensure t
  :config
  (add-hook 'tuareg-mode-hook
            (lambda () (setq indent-line-function 'ocp-indent-line))))

;;; Python
(use-package python-mode :defer t :ensure t)

;;; Markdown
(use-package markdown-mode :defer t :ensure t)

;;; LaTeX
(use-package auctex :defer t :ensure t
  :config
  (setq TeX-PDF-mode   t
        TeX-auto-save  t
        TeX-parse-self t)
  (setq-default TeX-master nil))


;; Theme and Interface

(use-package powerline :ensure t)
(use-package powerline-evil :ensure t)

(use-package airline-themes :ensure t
  :config (load-theme 'airline-distinguished t))

(use-package zenburn-theme :ensure t
  :config (load-theme 'zenburn t))

;; Interface Settings
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(menu-bar-mode -1)
(savehist-mode)
(show-paren-mode)
(column-number-mode)
(global-hl-line-mode)
(linum-relative-global-mode)

                                        ; Misc behavior settings
(setq
 vc-follow-symlinks t
 x-select-enable-clipboard t
 make-backup-files nil
 coding-system-for-read 'utf-8
 coding-system-for-write 'utf-8)

                                        ; Defaults
(setq-default
 show-trailing-whitespace t
 indent-tabs-mode nil
 fill-column 100)

                                        ; Evil settings
(setq evil-insert-state-cursor  '("#268bd2" bar)  ;; blue
      evil-normal-state-cursor  '("#b58900" box)  ;; blue
      evil-visual-state-cursor  '("#cb4b16" box)  ;; orange
      evil-replace-state-cursor '("#859900" hbar) ;; green
      evil-emacs-state-cursor   '("#d33682" box) ;; magenta
      )

                                        ; Start the server
(server-start)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (highlight-indent-guides zenburn-theme zenburn which-key use-package tuareg python-mode projectile powerline-evil ocp-indent mode-icons markdown-mode magit linum-relative indent-guide helm general evil-visualstar evil-visual-mark-mode evil-surround evil-matchit evil-leader evil-escape evil-commentary evil-args color-theme-sanityinc-tomorrow auctex airline-themes))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

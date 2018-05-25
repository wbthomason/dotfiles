(eval-when-compile (require 'cl))
(require 'package)
(setq package-archives
      '(("org"       . "http://orgmode.org/elpa/")
        ("melpa"     . "https://melpa.org/packages/")
        ("gnu"       . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")))
(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(add-to-list 'load-path "~/.emacs.d/local")

;; General Packages
;;; Ivy
(use-package ivy :ensure t
  :diminish ivy-mode
  :init
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (with-eval-after-load 'ivy
    (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)
    (define-key ivy-minibuffer-map (kbd "C-a") 'ivy-read-action)
    (define-key ivy-minibuffer-map (kbd "C-f") 'ivy-toggle-fuzzy)))

(use-package counsel-projectile :ensure t :defer t)
(use-package ivy-bibtex :ensure t :defer t)

;;; Undotree
(use-package undo-tree :ensure t
  :init (setq undo-tree-auto-save-history t))

;;; Git gutter
(use-package git-gutter :ensure t
  :commands (global-git-gutter-mode git-gutter-mode)
  :init
  (progn
    (global-git-gutter-mode t)
    (git-gutter:linum-setup)
    (setq git-gutter:update-interval 2
          git-gutter:modified-sign " "
          git-gutter:added-sign "+"
          git-gutter:deleted-sign "-"
          git-gutter:diff-option "-w"
          git-gutter:hide-gutter t
          git-gutter:ask-p nil
          git-gutter:verbosity 0
          git-gutter:handled-backends '(git hg bzr svn)
          git-gutter:hide-gutter t)))

;;; Rainbow mode
(use-package rainbow-mode :ensure t :defer t)

;;; Interactive align
(use-package ialign :ensure t :defer t)

;;; Popup kill ring
(use-package popup-kill-ring :ensure t)

;;; Smartparens
(use-package smartparens :ensure t
  :init
  (add-hook 'clojure-mode-hook #'smartparens-mode)
  (add-hook 'emacs-lisp-mode-hook #'smartparens-mode)
  (add-hook 'common-lisp-mode-hook #'smartparens-mode)
  (add-hook 'scheme-mode-hook #'smartparens-mode)
  (add-hook 'lisp-mode-hook #'smartparens-mode))

(sp-local-pair 'tuareg-mode "'" nil :actions nil)
(sp-local-pair 'tuareg-mode "`" nil :actions nil)
(sp-with-modes 'emacs-lisp-mode
  ;; disable ', it's the quote character!
  (sp-local-pair "'" nil :actions nil)
  ;; also only use the pseudo-quote inside strings where it
  ;; serves as hyperlink.
  (sp-local-pair "`" "'" :when '(sp-in-string-p sp-in-comment-p)))

;;; Parinfer
(use-package parinfer
  :ensure t
  :init
  (progn
    (setq parinfer-extensions
          '(defaults       ; should be included.
             evil           ; If you use Evil.
             smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
             smart-yank))   ; Yank behavior depend on mode.
    (add-hook 'clojure-mode-hook #'parinfer-mode)
    (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
    (add-hook 'common-lisp-mode-hook #'parinfer-mode)
    (add-hook 'scheme-mode-hook #'parinfer-mode)
    (add-hook 'lisp-mode-hook #'parinfer-mode)
    :config
    (setq parinfer-auto-switch-indent-mode t)))

;;; Evil!
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration nil)
  (setq evil-want-fine-undo t)
  (setq evil-want-C-i-jump nil)
  (setq evil-move-beyond-eol t))

(use-package evil-leader
  :ensure t
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>"))

(use-package evil-collection :ensure t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode t))

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
  (setq-default evil-escape-key-sequence "zx"
                evil-escape-delay 0.2)
  (setq evil-escape-unordered-key-sequence t))

(use-package evil-magit :ensure t)

(use-package evil-terminal-cursor-changer :ensure t
  :unless (display-graphic-p)
  :config (evil-terminal-cursor-changer-activate))

(use-package evil-cleverparens :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook       #'evil-cleverparens-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'evil-cleverparens-mode)
  (add-hook 'ielm-mode-hook             #'evil-cleverparens-mode)
  (add-hook 'lisp-mode-hook             #'evil-cleverparens-mode)
  (add-hook 'lisp-interaction-mode-hook #'evil-cleverparens-mode)
  (add-hook 'scheme-mode-hook           #'evil-cleverparens-mode))

(use-package evil-commentary :ensure t
  :config
  (evil-commentary-mode))

(use-package navigate :ensure t)

(evil-mode 1)

;;; Flycheck
(use-package flycheck :ensure t
  :init
  (global-flycheck-mode)
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'cust-flycheck-bitmap
      (vector #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00011100
              #b00111110
              #b00111110
              #b00111110
              #b00011100
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000)))
  (flycheck-define-error-level 'error
    :severity 2
    :overlay-category 'flycheck-error-overlay
    :fringe-bitmap 'cust-flycheck-bitmap
    :fringe-face 'flycheck-fringe-error)
  (flycheck-define-error-level 'warning
    :severity 1
    :overlay-category 'flycheck-warning-overlay
    :fringe-bitmap 'cust-flycheck-bitmap
    :fringe-face 'flycheck-fringe-warning)
  (flycheck-define-error-level 'info
    :severity 0
    :overlay-category 'flycheck-info-overlay
    :fringe-bitmap 'cust-flycheck-bitmap
    :fringe-face 'flycheck-fringe-info)
  (defun close-flycheck ()
    (interactive)
    (-if-let (window (flycheck-get-error-list-window))
        (quit-window nil window) ())))

(use-package flycheck-pos-tip :ensure t :defer t
  :init
  (with-eval-after-load 'flycheck
    (flycheck-pos-tip-mode)))

;;; Spell checking
(use-package flyspell :ensure t :defer t
  :init
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (add-hook 'text-mode-hook 'flyspell-mode)
  (setq flyspell-issue-message-flag nil))

(use-package flyspell-correct :ensure t
  :commands (flyspell-correct-word-generic
             flyspell-correct-previous-word-generic))

(use-package flyspell-correct-helm :ensure t
  :commands (flyspell-correct-helm)
  :init
  (setq flyspell-correct-interface #'flyspell-correct-helm))

(use-package auto-dictionary :ensure t :defer t
  :init
  (add-hook 'flyspell-mode-hook 'auto-dictionary-mode))


;;; Restart Emacs
(use-package restart-emacs :ensure t)

;;; Relative linum
(use-package linum-relative :ensure t
  :config
  (setq linum-relative-format "%3s ")
  (setq linum-relative-current-symbol ""))
;;; Once Emacs 26 is out
;;; (setq linum-relative-backend 'display-line-numbers-mode)

;;; Indentation guides
(use-package highlight-indent-guides :ensure t
  :init
  :config
  (setq highlight-indent-guides-method 'character)
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

;;; Projectile
(use-package projectile :ensure t
  :init
  (setq-default
   projectile-mode-line
   '(:eval
     (if (file-remote-p default-directory)
         " Pr"
       (format " Pr[%s]" (projectile-project-name))))))
(projectile-mode)

;;; Drag-stuff
(use-package drag-stuff :ensure t
  :config
  (drag-stuff-global-mode t)
  (drag-stuff-define-keys))

;;; Zoom
(use-package zoom :ensure t)

;;; Magit
(use-package magit :ensure t)

;;; which-key
(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  (setq which-key-sort-order 'which-key-key-order-alpha)
  (setq which-key-popup-type 'side-window
        which-key-side-window-max-width 0.33
        which-key-idle-delay 0.5))

;;; Company
(use-package company
  :ensure t
  :config
  (global-company-mode)
  (setq company-backends (delete 'company-semantic company-backends))
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2
        company-require-match nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil)
  (setq company-selection-wrap-around t)
  ;; (define-key company-active-map [?\r] 'company-complete)
  (define-key company-active-map [tab] 'company-select-next)
  (define-key company-active-map [S-tab] 'company-select-previous))

;;; LSP
;; Rust, Python, Javascript, Bash, and PHP work out of the box
                                        ; (use-package eglot :ensure t
                                        ;   :init
                                        ;   (add-to-list 'eglot-server-programs
                                        ;                '(c-mode . ("cquery" "--language-server"))
                                        ;                '(c++-mode . ("cquery" "--language-server"))
                                        ;                '(tuareg-mode . ("ocaml-language-server" "--stdio"))
                                        ;                '(haskell-mode . ("hie" "--lsp"))
                                        ;                '(common-lisp-mode . ("cl-lsp"))))

                                        ; (use-package company-lsp :ensure t
                                        ;   :init
                                        ;   (push 'company-lsp company-backends))
                                        ;
                                        ; (use-package company-quickhelp :ensure t
                                        ;   :config
                                        ;   (company-quickhelp-mode))

(use-package fuzzy :ensure t :defer t)

;; Languages
;;; OCaml
;;;; OCP-indent
(eval-and-compile
  (defun opam-share ()
    (substring
     (shell-command-to-string "opam config var share 2> /dev/null")
     0 -1)))

(use-package ocp-indent :defer t :ensure t
  :load-path (lambda () ( concat ( opam-share ) "/emacs/site-lisp")))

;;;; Tuareg
(use-package tuareg :defer t :ensure t
  :config
  (add-hook 'tuareg-mode-hook
            (lambda () (setq indent-line-function 'ocp-indent-line))))

;;;; Merlin
(use-package merlin :ensure t :defer t
  :init
  (add-hook 'tuareg-mode-hook 'merlin-mode)
  (setq merlin-completion-with-doc t))

;;;; Utop
(use-package utop :ensure t :defer t
  :init
  (add-hook 'tuareg-mode-hook 'utop-minor-mode)
  (setq utop-command "opam config exec -- utop -emacs"))

;;; Python
(use-package python-mode :defer t :ensure t)
(use-package company-jedi :defer t :ensure t
             :init
             (add-hook 'python-mode-hook
                       (lambda ()
                         (add-to-list 'company-backends 'company-jedi)
                         (add-hook 'python-mode-hook 'jedi:setup)))
             :config
             (setq jedi:complete-on-dot t))

(use-package py-yapf :defer t :ensure t
             :init
             (add-hook 'python-mode-hook 'py-yapf-enable-on-save))

(use-package ein :defer t :ensure t
             :init
             (setq ein:use-auto-complete-superpack t))

(use-package py-isort :defer t :ensure t
             :config
             (add-hook 'before-save-hook 'py-isort-before-save))

;;; Markdown
(use-package markdown-mode :defer t :ensure t)
(use-package markdown-toc :defer t :ensure t)

;;; LaTeX
(use-package auctex :defer t :ensure t
  :config
  (setq TeX-PDF-mode   t
        TeX-auto-save  t
        TeX-parse-self t)
  (setq-default TeX-master nil))

(use-package auctex-latexmk :ensure t :defer t)
(use-package company-auctex :ensure t :defer t)
(use-package reftex :defer t
  :init
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX '(nil nil t t t)
        reftex-use-fonts t))

;;; YAML
(use-package yaml-mode :ensure t :defer t)

;;; Fish
(use-package fish-mode :ensure t :defer t)

;;; Bash
(use-package sh-script :defer t)

;;; Scheme
(use-package geiser :ensure t :defer t)

;;; Scala
(use-package scala-mode :ensure t :defer t)

;;; Rust
(use-package cargo :ensure t :defer t)

;;; Racket
(use-package racket-mode :ensure t :defer t)

;;; C++
(use-package modern-cpp-font-lock
  :ensure t)

(use-package company-irony :ensure t :defer t
  :config
  (setq company-irony-ignore-case 'smart)
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)

  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package company-irony-c-headers :ensure t :defer t
  :init
  (add-hook 'c++-mode-hook (lambda ()
                             (add-to-list
                              'company-backends
                              '(company-irony-c-headers company-irony))))
  (add-hook 'c-mode-hook (lambda ()
                           (add-to-list
                            'company-backends
                            '(company-irony-c-headers company-irony)))))

(use-package flycheck-irony :ensure t :defer t
  :init
  (with-eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))

(use-package irony-eldoc :ensure t :defer t
  :init
  (add-hook 'irony-mode-hook #'irony-eldoc))

(use-package clang-format :ensure t :defer t)

;;; Org
(use-package org :ensure t :defer t)
(use-package org-ref :ensure t :defer t)

;;; Bibtex
(use-package biblio :ensure t :defer t)
(use-package biblio-core :ensure t :defer t)

;; Theming and Interface

;;; Powerline
(use-package powerline :ensure t)

(use-package powerline-evil :ensure t)

(use-package airline-themes :ensure t
  :config (load-theme 'airline-distinguished t))

;;; Theme
(use-package base16-theme :ensure t
  :config
  (load-theme 'base16-tomorrow-night t))

;;; Rainbow delimiters
(use-package rainbow-delimiters :ensure t
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;;; Focus
(use-package focus :ensure t)

;;; Golden ratio
(use-package golden-ratio :ensure t
  :init
  (golden-ratio-mode 1)
  (add-to-list 'golden-ratio-extra-commands 'tmux-nav-left)
  (add-to-list 'golden-ratio-extra-commands 'tmux-nav-right)
  (add-to-list 'golden-ratio-extra-commands 'tmux-nav-up)
  (add-to-list 'golden-ratio-extra-commands 'tmux-nav-down))

;; Interface Settings
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(menu-bar-mode -1)
(savehist-mode)
(show-paren-mode)
(column-number-mode)
(linum-relative-global-mode)
(setq inhibit-startup-screen t)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Misc behavior settings
(setq
 vc-follow-symlinks t
 select-enable-clipboard t
 make-backup-files nil
 coding-system-for-read 'utf-8
 coding-system-for-write 'utf-8)
(save-place-mode 1)

;; Defaults
(setq-default
 show-trailing-whitespace t
 indent-tabs-mode nil
 c-default-style "bsd"
 tab-width 2
 c-basic-offset 2
 cperl-indent-level 2)

(setq c-default-style "bsd")
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(use-package dtrt-indent :ensure t
  :init
  (dtrt-indent-mode))

;; Evil settings
(setq evil-insert-state-cursor  '("#268bd2" bar)  ;; blue
      evil-normal-state-cursor  '("#b58900" box)  ;; blue
      evil-visual-state-cursor  '("#cb4b16" box)  ;; orange
      evil-replace-state-cursor '("#859900" hbar) ;; green
      evil-emacs-state-cursor   '("#d33682" box)) ;; magenta

;; Minibuffer quitting with a single ESC
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

;; Toggle split direction
(defun window-toggle-split-direction ()
  "Switch window split from horizontally to vertically, or vice versa.

i.e. change right window to bottom, or change bottom window to right."
  (interactive)
  (require 'windmove)
  (let ((done))
    (dolist (dirs '((right . down) (down . right)))
      (unless done
        (let* ((win (selected-window))
               (nextdir (car dirs))
               (neighbour-dir (cdr dirs))
               (next-win (windmove-find-other-window nextdir win))
               (neighbour1 (windmove-find-other-window neighbour-dir win))
               (neighbour2 (if next-win (with-selected-window next-win
                                          (windmove-find-other-window neighbour-dir next-win)))))
          ;;(message "win: %s\nnext-win: %s\nneighbour1: %s\nneighbour2:%s" win next-win neighbour1 neighbour2)
          (setq done (and (eq neighbour1 neighbour2)
                          (not (eq (minibuffer-window) next-win))))
          (if done
              (let* ((other-buf (window-buffer next-win)))
                (delete-window next-win)
                (if (eq nextdir 'right)
                    (split-window-vertically)
                  (split-window-horizontally))
                (set-window-buffer (windmove-find-other-window neighbour-dir) other-buf))))))))

(defun format-buffer ()
  "Call the appropriate formatter for the current major mode."
  (interactive)
  (cond ((eq major-mode 'c++-mode) (clang-format-buffer))
        ((eq major-mode 'c-mode) (clang-format-buffer))
        ((eq major-mode 'python-mode) (py-yapf-buffer))))

;; Keybindings
(evil-leader/set-key
  "q"  'kill-emacs
  "w"  'save-buffer
  "d"  'kill-buffer-and-window
  "x"  'save-buffers-kill-terminal
  "a"  'previous-buffer
  "s"  'next-buffer
  "k"  'delete-window
  "z=" 'flyspell-auto-correct-word
  "bb" 'ivy-switch-buffer
  "eo" 'flycheck-list-errors
  "ec" 'close-flycheck
  "bl" 'last-buffer
  "bf" 'format-buffer
  "ff" 'counsel-find-file
  "fr" 'counsel-recentf
  "fh" 'counsel-apropos
  "fi" 'counsel-ag
  "fl" 'counsel-locate
  "fp" 'counsel-projectile-switch-project
  "gf" 'counsel-git
  "gs" 'magit-status
  "gc" 'magit-commit
  "gp" 'magit-push
  "gl" 'magit-pull
  "ts" 'window-toggle-split-direction)

;; Start the server
(server-start)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (dtrt-indent golden-ratio focus rainbow-delimiters base16-theme airline-themes powerline-evil powerline org-ref clang-format irony-eldoc flycheck-irony company-irony-c-headers company-irony modern-cpp-font-lock racket-mode cargo scala-mode geiser fish-mode yaml-mode company-auctex auctex-latexmk auctex markdown-toc markdown-mode py-isort ein py-yapf company-jedi python-mode utop merlin tuareg ocp-indent fuzzy company which-key zoom drag-stuff highlight-indent-guides linum-relative restart-emacs auto-dictionary flyspell-correct-helm flyspell-correct flycheck-pos-tip flycheck navigate evil-commentary evil-cleverparens evil-terminal-cursor-changer evil-magit evil-escape evil-visualstar evil-args evil-visual-mark-mode evil-matchit evil-surround evil-collection evil-leader evil parinfer smartparens popup-kill-ring ialign rainbow-mode git-gutter undo-tree ivy-bibtex counsel-projectile ivy use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

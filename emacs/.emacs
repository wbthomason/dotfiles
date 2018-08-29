;;; .emacs --- wbthomason's Emacs config

;;; Commentary:
;; Basic Emacs config, split up into separate packages for easier maintenance

;;; TODO: More consistently use :after
;;; TODO: Put keybindings with relevant packages
;;; TODO: Split into separate files

;;; Code:

;; Prereqs and startup speedup
;; From
;; https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/ and https://github.com/hlissner/doom-emacs/wiki/FAQ#how-is-dooms-startup-so-fast
;; -*- lexical-binding: t; -*-
(defvar gc-cons-threshold-original gc-cons-threshold)
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)
(run-with-idle-timer
 5 nil
 (lambda ()
   (setq gc-cons-threshold (* 2 gc-cons-threshold-original)
         gc-cons-percentage 0.1)
   (setq file-name-handler-alist file-name-handler-alist-original)
   (makunbound 'gc-cons-threshold-original)
   (makunbound 'file-name-handler-alist-original)
   (message "gc-cons-threshold and file-name-handler-alist restored")))

(eval-when-compile (require 'cl))

;; Package management
(require 'package)
(setq package-archives
      '(("org" . "https://orgmode.org/elpa/")
        ("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")))
(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(use-package paradox
  :ensure t
  :config
  (define-key paradox-menu-mode-map (kbd "C-f") #'hydra-paradox-filter/body)
  (paradox-enable))

(use-package no-littering :ensure t)

;; Byte-compile
(setq load-prefer-newer t)
(use-package auto-compile
  :ensure t
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

;; Local packages
(add-to-list 'load-path "~/.emacs.d/local")

;; Path-ish settings
(setenv "PKG_CONFIG_PATH" (concat "/opt/ros/melodic/lib/pkgconfig" ":/usr/local/lib/pkgconfig" ":/usr/local/lib64/pkgconfig/" (getenv "PKG_CONFIG_PATH")))
(setenv "LD_LIBRARY_PATH" (concat "/opt/ros/melodic/lib" ":/usr/local/lib" ":/usr/local/lib64" (getenv "LD_LIBRARY_PATH")))
(setenv "PATH" (concat "/home/wil/.local/bin" ":/home/wil/.cargo/bin" ":/home/wil/.luarocks/bin"
                       ":/home/wil/.roswell/bin" (getenv "PATH")))

;; General Packages
;;; Ivy
(use-package ivy
  :ensure t
  :diminish ivy-mode
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t
        ;; ivy-re-builders-alist '((t . ivy--regex-fuzzy))
        ivy-display-style 'fancy
        ivy-format-function 'ivy-format-function-line)
  (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)
  (define-key ivy-minibuffer-map (kbd "C-a") 'ivy-read-action)
  (define-key ivy-minibuffer-map (kbd "C-f") 'ivy-toggle-fuzzy))

(use-package swiper :ensure t)

(use-package ivy-rich
  :ensure t
  :config
  (setq ivy-virtual-abbreviate 'full
        ivy-rich-path-style 'abbrev)
  (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer))

(use-package ivy-xref
  :ensure t
  :init (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package counsel :ensure t)

(use-package counsel-projectile :ensure t)

;; (use-package ivy-bibtex :ensure t)
(use-package counsel-etags
  :ensure t
  :config
  (add-to-list 'counsel-etags-ignore-directories ".cquery_cached_index")
  (add-to-list 'counsel-etags-ignore-directories "lib")
  (setq tags-revert-without-query t)
  (add-hook 'prog-mode-hook
            (lambda ()
              (add-hook 'after-save-hook 'counsel-etags-virtual-update-tags 'append 'local))))

(use-package wgrep :ensure t)

(use-package amx
  :ensure t
  :config
  (amx-mode))

(use-package ivy-posframe
  :after ivy
  :ensure t
  :config
  (defun ivy-posframe-center-dynamic-size (str)
    "Simple wrapper to dynamically set the width and height for the posframe."
    (setq ivy-posframe-height (floor (* 1.2 ivy-height))
          ivy-posframe-width (floor (/ (window-width) 1.2)))
    (ivy-posframe--display str #'posframe-poshandler-frame-center))
  (setq ivy-display-function #'ivy-posframe-center-dynamic-size)
  (push
   `(ivy-posframe-center-dynamic-size
     :cleanup
     (lambda () (when
               (ivy-posframe-workable-p)
             (posframe-hide ivy-posframe-buffer))))
   ivy-display-functions-props))

;;; Eshell
(use-package esh-autosuggest
  :ensure t
  :hook (eshell-mode . esh-autosuggest-mode))

;;; Git gutter
(use-package git-gutter
  :ensure t
  :config
  (setq git-gutter:modified-sign "＊"
        git-gutter:added-sign "+"
        git-gutter:deleted-sign "～"
        git-gutter:window-width 2
        git-gutter:handled-backends '(git hg bzr svn))
  (set-face-foreground 'git-gutter:modified "#8ec07c") ;; background color
  (set-face-foreground 'git-gutter:added "#b8bb26")
  (set-face-foreground 'git-gutter:deleted "cc241d"))

;;; Eyebrowse
(use-package eyebrowse
  :ensure t
  :after evil
  :config
  (eyebrowse-mode t)
  (eyebrowse-setup-opinionated-keys))

;;; Rainbow mode
(use-package rainbow-mode :ensure t)

;;; Interactive align
(use-package ialign :ensure t)

;;; Popup kill ring
(use-package popup-kill-ring :ensure t)

;;; Lispy
(use-package lispy
  :ensure t
  :hook ((emacs-lisp-mode clojure-mode common-lisp-mode scheme-mode lisp-mode racket-mode) . lispy-mode))

;;; Lispyville
(use-package lispyville
  :ensure t
  :hook (lispy-mode . lispyville-mode)
  :config
  (lispyville-set-key-theme '(operators
                              prettify
                              text-objects
                              atom-movement
                              additional-movement
                              slurp/barf-cp
                              additional
                              escape)))

;;; Parinfer
(use-package parinfer
  :ensure t
  :hook ((clojure-mode emacs-lisp-mode common-lisp-mode scheme-mode racket-mode lisp-mode) . parinfer-mode)
  :config
  (progn
    (setq parinfer-extensions
          '(defaults                    ; should be included.
             evil                       ; If you use Evil.
             smart-tab                  ; C-b & C-f jump positions and smart shift with tab & S-tab.
             smart-yank))               ; Yank behavior depend on mode.
    (setq parinfer-auto-switch-indent-mode t)))

;;; Undo-Tree
(use-package undo-tree :ensure t
  :config (global-undo-tree-mode))

;;; Evil!
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration nil
        evil-ex-visual-char-range t
        evil-want-fine-undo t
        evil-respect-visual-line-mode t
        evil-want-C-i-jump nil
        evil-move-beyond-eol t)
  :config
  (evil-mode t))

;; (use-package targets :ensure t)

(use-package evil-expat
  :ensure t
  :after evil)

(use-package evil-leader
  :ensure t
  :after evil
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>"))

(use-package hydra :ensure t)

;; (use-package evil-mc :ensure t)

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode t))

(use-package evil-matchit
  :ensure t
  :after evil
  :config
  (global-evil-matchit-mode t))

(use-package evil-fringe-mark
  :ensure t
  :after evil
  :config
  (global-evil-fringe-mark-mode))

(use-package evil-args
  :ensure t
  :after evil
  :config
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg))

(use-package evil-visualstar
  :ensure t
  :after evil
  :config
  (global-evil-visualstar-mode t))

(use-package evil-embrace
  :ensure t
  :after evil
  :config
  (evil-embrace-enable-evil-surround-integration))

(use-package evil-escape
  :ensure t
  :after evil
  :diminish
  (evil-escape-mode)
  :config
  (evil-escape-mode)
  (setq-default evil-escape-key-sequence "zx"
                evil-escape-delay 0.2)
  (setq evil-escape-unordered-key-sequence t))

(use-package evil-magit
  :ensure t
  :after (evil magit))

(use-package evil-terminal-cursor-changer
  :ensure t
  :after evil
  :unless (display-graphic-p)
  :config (evil-terminal-cursor-changer-activate))

(use-package evil-commentary
  :ensure t
  :after evil
  :config
  (evil-commentary-mode))

(use-package evil-snipe
  :ensure t
  :diminish evil-snipe-mode
  :after evil
  :init
  (setq evil-snipe-scope 'buffer
        evil-snipe-repeat-scope 'whole-buffer)
  :config
  (evil-snipe-mode t)
  (evil-snipe-override-mode t)
  (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode))

(use-package evil-lion
  :ensure t
  :after evil
  :config
  (evil-lion-mode))

(use-package evil-goggles
  :ensure t
  :after evil
  :config
  (setq evil-goggles-duration 0.05)
  (evil-goggles-mode)
  (evil-goggles-use-diff-refine-faces))

;;; Tmux navigator
(use-package navigate)
(defun tmux-nav-left () (interactive) (tmux-navigate "left"))
(defun tmux-nav-right () (interactive) (tmux-navigate "right"))
(defun tmux-nav-up () (interactive) (tmux-navigate "up"))
(defun tmux-nav-down () (interactive) (tmux-navigate "down"))
(define-key evil-normal-state-map (kbd "C-h") #'tmux-nav-left)
(define-key evil-normal-state-map (kbd "C-j") #'tmux-nav-down)
(define-key evil-normal-state-map (kbd "C-k") #'tmux-nav-up)
(define-key evil-normal-state-map (kbd "C-l") #'tmux-nav-right)

;; (evil-mode 1)

;;; Flycheck
(use-package flycheck :ensure t
  :config
  ;; (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'cust-flycheck-bitmap
      (vector 0
              0
              0
              0
              0
              0
              0
              28
              62
              62
              62
              28
              0
              0
              0
              0
              0)))
  (global-flycheck-mode)
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

(use-package flycheck-pos-tip
  :ensure t
  :config
  (with-eval-after-load 'flycheck
    (flycheck-pos-tip-mode)))

;;; Spell checking
(use-package flyspell
  :ensure t
  :hook ((org-mode LaTeX-mode markdown-mode) . flyspell-mode)

  :config
  (setq flyspell-issue-message-flag nil))

(use-package flyspell-correct
  :ensure t
  :commands (flyspell-correct-word-generic
             flyspell-correct-previous-word-generic))

(use-package auto-dictionary
  :ensure t
  :hook (flyspell-mode . auto-dictionary-mode))

;;; Restart Emacs
(use-package restart-emacs :ensure t)

;;; Indentation guides
(use-package highlight-indent-guides
  :ensure t
  ;; :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))

;;; Projectile
(use-package projectile
  :ensure t
  :config
  (projectile-mode t)
  (setq projectile-enable-caching t)
  (setq-default
   projectile-mode-line
   '(:eval
     (if (file-remote-p default-directory)
         " Pr"
       (format " Pr[%s]" (projectile-project-name)))))
  (push "meson.build" projectile-project-root-files)
  (push "meson.build" projectile-project-root-files-bottom-up))

;;; Magit
(use-package magit
  :ensure t
  :defer 5
  :config
  ;; Redefine the tab key specifically for Magit to fix the overriding of the above keybinding
  (define-key magit-mode-map [tab] #'magit-section-toggle))

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
  :diminish company-mode
  :hook (after-init . global-company-mode)

  :config
  (define-key company-active-map [tab] 'company-select-next)
  (define-key company-active-map [S-tab] 'company-select-previous)
  (define-key company-active-map [C-return] #'company-complete-selection)
  (define-key company-active-map [return] #'company-complete-selection)
  (setq company-backends (delete 'company-semantic company-backends))
  (setq company-backends (delete 'company-dabbrev company-backends))
  (setq company-backends (delete 'company-oddmuse company-backends))
  (setq company-idle-delay 0
        company-frontends '(company-pseudo-tooltip-unless-just-one-frontend company-preview-frontend company-echo-metadata-frontend)
        company-minimum-prefix-length 2
        company-require-match nil
        company-tooltip-align-annotations t
        company-selection-wrap-around t))

;;; NOTE: This package breaks company and the icons are too large. Maybe revisit in the future (7/3/2018)
;; (use-package company-box
;;   :ensure t
;;   :hook (company-mode . company-box-mode))

(use-package company-posframe
  :disabled t
  :after (company desktop)
  :ensure t
  :config
  (push '(company-posframe-mode . nil) desktop-minor-mode-table)
  (company-posframe-mode 1))

;;; All-the-icons
(use-package all-the-icons-ivy
  :ensure t
  :after ivy
  :config (all-the-icons-ivy-setup))

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))
;;; Prescient
(use-package prescient :ensure t)

;; (use-package ivy-prescient
;;   :ensure t
;;   :after ivy
;;   :config (ivy-prescient-mode))

(use-package company-prescient
  :ensure t
  :after company
  :config (company-prescient-mode))

;;; LSP
;; Rust, Python, Javascript, Bash, and PHP work out of the box
;; (use-package eglot :ensure t
;;   :config
;;   (add-to-list 'eglot-server-programs '(tuareg-mode . ("ocaml-language-server" "--stdio")))
;;   (add-to-list 'eglot-server-programs '(haskell-mode . ("hie" "--lsp")))
;;   (add-to-list 'eglot-server-programs '(common-lisp-mode . ("cl-lsp"))))

(use-package lsp-mode
  :ensure t
  :config
  (defun my-set-projectile-root ()
    (when lsp--cur-workspace
      (setq projectile-project-root (lsp--workspace-root lsp--cur-workspace))))
  (add-hook 'lsp-before-open-hook #'my-set-projectile-root)
  (setq lsp-enable-completion-at-point nil))

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode))

(use-package company-lsp :ensure t
  :init
  (setq company-lsp-async t
        company-lsp-cache-candidates t
        company-lsp-enable-snippet t
        company-lsp-enable-recompletion t)
  :config
  (push 'company-lsp company-backends))

(use-package company-quickhelp :ensure t
  :config
  (company-quickhelp-mode))

;; Languages
;; Common Lisp
(use-package slime-company
  :ensure t
  :hook slime-mode)

(use-package slime
  :ensure t
  :hook (common-lisp-mode lisp-mode)
  :init
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (setq slime-contribs '(slime-fancy slime-company slime-asdf))
  :config
  (add-hook 'slime-mode-hook (lambda () (unless (slime-connected-p) (save-excursion (slime))))))


;;; OCaml
;;;; OCP-indent
(eval-and-compile
  (defun opam-share ()
    (substring
     (shell-command-to-string "opam config var share 2> /dev/null")
     0 -1)))

(use-package ocp-indent
  :ensure t
  :hook (tuareg-mode . (lambda () (setq indent-line-function 'ocp-indent-line)))
  :load-path (lambda () (concat (opam-share) "/emacs/site-lisp")))

;;;; Tuareg
(use-package tuareg :ensure t)

;;;; Merlin
(use-package merlin
  :ensure t
  :hook (tuareg-mode . merlin-mode)
  :config
  (setq merlin-completion-with-doc t))

;;;; Utop
(use-package utop
  :ensure t
  :hook (tuareg-mode . utop-minor-mode)
  :config
  (setq utop-command "opam config exec -- utop -emacs"))

(use-package lsp-ocaml
  :ensure t
  :hook ((tuareg-mode caml-mode reason-mode) . lsp-ocaml-enable))


;;; Haskell

(use-package haskell-mode :ensure t)

(use-package ghc
  :ensure t
  :hook (haskell-mode . (lambda () (ghc-init))))

(use-package intero
  :ensure t
  :hook (haskell-mode . intero-mode))

(use-package hindent
  :ensure t
  :hook (haskell-mode . hindent-mode))

;; (use-package haskell-snippets :ensure t)

(use-package lsp-haskell
  :ensure t
  :hook (haskell-mode . lsp-haskell-enable))

(use-package company-cabal
  :ensure t
  :hook (haskell-mode . (lambda () (push 'company-cabal company-backends))))

(use-package company-ghci
  :ensure t
  :hook (haskell-mode . (lambda () (push 'company-ghci company-backends))))

(use-package company-ghc
  :ensure t
  :hook (haskell-mode . (lambda () (push 'company-ghc company-backends))))

(use-package flycheck-haskell
  :ensure t
  :after flycheck
  :hook (flycheck-mode . flycheck-haskell-setup))

(use-package flycheck-ghcmod
  :ensure t
  :after flycheck)

;;; Python
(setenv "PYTHONPATH" "/opt/ros/melodic/lib/python2.7/site-packages")

(use-package python
  :ensure t
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))

(use-package anaconda-mode
  :ensure t
  :diminish anaconda-mode
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode)))

(use-package company-anaconda
  :ensure t
  :after (company anaconda-mode)
  :hook (python-mode . (lambda () (push 'company-anaconda company-backends))))

(use-package company-jedi :ensure t
  :hook (python-mode . (lambda ()
                         (push 'company-jedi company-backends)
                         (add-hook 'python-mode-hook 'jedi:setup)))
  :config
  (setq jedi:complete-on-dot t))

(use-package yapfify
  :ensure t
  :hook (python-mode . yapf-mode))

(use-package ein
  :ensure t
  :init (setq ein:use-auto-complete-superpack t)
  :commands (ein:notebooklist-open))

(use-package py-isort
  :ensure t
  :hook (python-mode . (lambda () (add-hook 'before-save-hook #'py-isort-before-save))))

(use-package lsp-python
  :ensure t
  :commands lsp-python-enable
  :hook (python-mode . lsp-python-enable))

(use-package flycheck-pycheckers
  :ensure t
  :after flycheck
  :hook (flycheck-mode . flycheck-pycheckers-setup))

;;; Markdown
(use-package markdown-mode
  :ensure t
  :init
  (setq markdown-command "pandoc"))

(use-package markdown-toc :ensure t)

;;; LaTeX
;; For biber
(setenv "PATH" (concat (getenv "PATH") ":/usr/bin/vendor_perl/"))
(add-to-list 'exec-path "/usr/bin/vendor_perl/")

(use-package tex
  :ensure auctex
  :hook
  (doc-view-mode . auto-revert-mode)
  (LaTeX-mode . (lambda ()
                  (auto-fill-mode -1)
                  (visual-line-mode)
                  (variable-pitch-mode)))

  :config
  (add-to-list 'TeX-view-program-list
               '("Zathura"
                 ("zathura %o"
                  (mode-io-correlate " --synctex-forward %n:0:%b -x \"emacsclient --socket-name=%sn --no-wait +%{line} %{input}\""))
                 "zathura"))
  (setq TeX-PDF-mode t
        TeX-auto-save t
        TeX-parse-self t
        TeX-electric-sub-and-superscript t
        TeX-electric-math '("\\(" "\\)")
        TeX-quote-after-quote t
        TeX-clean-confirm nil
        TeX-source-correlate-mode t
        TeX-source-correlate-method 'synctex
        TeX-engine 'xetex
        TeX-view-program-selection '((output-pdf "Zathura")))
  (setq-default TeX-master nil))

(use-package auctex-latexmk
  :ensure t
  :after tex
  :config
  (setq auctex-latexmk-inherit-TeX-PDF-mode t
        TeX-command-default "LatexMk")
  (auctex-latexmk-setup))

(use-package company-auctex
  :ensure t
  :after (tex company)
  :hook (LaTeX-mode . company-auctex-init))

(use-package reftex
  :ensure t
  :hook (LaTeX-mode . turn-on-reftex)
  :config
  (setq reftex-plug-into-AUCTeX '(nil nil t t t)
        reftex-use-fonts t))

(use-package company-reftex
  :ensure t
  :hook (LaTeX-mode . (lambda ()
                        (push 'company-reftex-labels company-backends)
                        (push 'company-reftex-citations company-backends))))

(use-package company-math
  :ensure t
  :after company
  :config
  (add-to-list 'company-backends 'company-math-symbols-latex))

;;; YAML
(use-package yaml-mode :ensure t)

;;; HTML/Web
(use-package mhtml-mode
  :ensure t
  :mode "\\.html\\'")

(use-package lsp-html
  :ensure t
  :hook (html-mode . lsp-html-enable))

;; (use-package lsp-css
;;   :ensure t
;;   :hook (css-mode . (lambda () (when (eq major-mode 'css-mode) (lsp-css-enable)))))

;;; Javascript
(use-package lsp-javascript-typescript
  :ensure t
  :hook ((js-mode typescript-mode js3-mode rjsx-mode) . lsp-javascript-typescript-enable))

;;; CMake
(use-package cmake-mode :ensure t)
(use-package cmake-font-lock
  :ensure t
  :hook (cmake-mode . cmake-font-lock-activate)
  :config
  (autoload 'cmake-font-lock-activate "cmake-font-lock" nil t))

;;; ROS
(projectile-register-project-type 'ros-workspace '(".catkin_tools" ".catkin_workspace")
                                  :compile "catkin build")
(projectile-register-project-type 'ros-package '("package.xml")
                                  :compile "catkin build")


;;; PDDL
(use-package pddl-mode)
(add-to-list 'auto-mode-alist '("\\.pddl\\'" . PDDL-mode))

;;; Meson
(use-package meson-mode :ensure t)

;;; TOML
(use-package toml-mode :ensure t)

;;; Lua
(use-package lua-mode :ensure t)
(lsp-define-stdio-client
 lsp-lua
 "lua"
 (lambda () default-directory)
 '("/home/wil/.luarocks/bin/lua-lsp"))
(add-hook 'lua-mode-hook #'lsp-lua-enable)

(use-package company-lua
  :ensure t
  :hook (lua-mode . (lambda () (push 'company-lua company-backends))))

;;; Fish
(use-package fish-mode :ensure t)

;;; Bash
(use-package sh-script :ensure t)
;; (use-package lsp-sh
;;   :ensure t
;;   :hook (sh-mode . lsp-sh-enable))

;;; Scheme
(use-package geiser :ensure t)

;;; Scala
(use-package scala-mode :ensure t)

;;; Go
(use-package lsp-go
  :ensure t
  :hook (go-mode . lsp-go-enable))

;;; Rust
(use-package rust-mode :ensure t)
(use-package cargo :ensure t)
(use-package lsp-rust :ensure t
  :commands lsp-rust-enable
  :hook (rust-mode . lsp-rust-enable)
  :init
  (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls")))

(use-package racer
  :ensure t
  :hook ((rust-mode . racer-mode)
         (racer-mode . eldoc-mode)))

(use-package flycheck-rust
  :ensure t
  :after (rust flycheck)
  :hook (flycheck-mode . flycheck-rust-setup))

;; Racket
(use-package racket-mode
  :ensure t
  :mode "\\.rkt\\'")

(use-package scribble-mode :ensure t)

;; C++
(add-hook 'c++-mode-hook #'electric-pair-mode)
(use-package cc-mode :ensure t)

(use-package google-c-style
  :ensure t
  :hook (c-mode-common-hook . google-set-c-style))

(use-package modern-cpp-font-lock :ensure t)

(use-package irony
  :ensure t
  :hook (((c++-mode c-mode objc-mode) . irony-mode)
         (irony-mode . irony-cdb-autosetup-compile-options)))

(use-package company-irony
  :ensure t
  :hook (irony-mode . company-irony-setup-begin-commands)
  :config
  (setq company-irony-ignore-case 'smart)
  (setq-default irony-cdb-compilation-databases '(irony-cdb-libclang irony-cdb-clang-complete)))

(use-package company-c-headers
  :ensure t
  :hook ((c++-mode c-mode) . (lambda () (push 'company-c-headers company-backends))))

(use-package flycheck-irony
  :ensure t
  :after flycheck
  :hook (flycheck-mode . flycheck-irony-setup))

(use-package flycheck-clang-analyzer
  :ensure t
  :after flycheck
  :hook (flycheck-mode . flycheck-clang-analyzer-setup))

(use-package flycheck-clang-tidy
  ;; :ensure t
  :after flycheck
  :hook (flycheck-mode . flycheck-clang-tidy-setup))

;; Note that I currently just have a local version of this
(use-package flycheck-google-cpplint
  :after flycheck
  :config (setq flycheck-c/c++-googlelint-executable "/usr/bin/cpplint")
  :hook (flycheck-mode . (lambda ()
                           (flycheck-add-next-checker 'c/c++-cppcheck '(warning . c/c++-googlelint)))))

(use-package flycheck-clangcheck
  :ensure t
  :after flycheck
  :config (setq flycheck-c/c++-clangcheck-executable "/usr/bin/clang-check")
  :hook (flycheck-mode . (lambda () (add-to-list 'flycheck-checkers 'c/c++-clangcheck))))

(use-package irony-eldoc
  :ensure t
  :hook (irony-mode . irony-eldoc))

(use-package cquery
  :ensure t
  :commands lsp-cquery-enable
  :hook (c-mode-common . lsp-cquery-enable)
  :config
  (setq cquery-executable "/usr/bin/cquery"
        cquery-extra-init-params '(:index (:comments 2) :completion (:detailedLabel t) :cacheFormat "msgpack")))

(use-package ccls
  :ensure t
  :disabled t
  :hook (c-mode-common . lsp-ccls-enable)
  :config
  (setq ccls-extra-init-params '(:completion (:detailedLabel t) :cacheFormat "binary")))

;; Set a chain of C++ checkers
(add-hook 'c-mode-common-hook (lambda ()
                                (flycheck-add-next-checker 'irony 'c/c++-clang-tidy)
                                (flycheck-add-next-checker 'c/c++-clang-tidy 'c/c++-clangcheck)
                                (flycheck-add-next-checker 'c/c++-clangcheck 'c/c++-cppcheck)
                                (flycheck-add-next-checker 'c/c++-googlelint 'clang-analyzer)))
;;; Deft
(use-package deft
  :ensure t
  :commands (deft)
  :init
  (setq deft-directory "~/wiki/notes"
        deft-extensions '("org" "md")
        deft-text-mode 'org-mode
        deft-recursive t
        deft-use-filename-as-title nil
        deft-use-filter-string-for-filename t
        deft-file-naming-rules '((noslash . "-")
                                 (nospace . "_")
                                 (case-fn . downcase))
        deft-markdown-mode-title-level 1
        deft-org-mode-title-prefix t))

;;; Org
(use-package org :ensure org-plus-contrib
  :hook (org-mode . (lambda ()
                      (linum-relative-mode -1)
                      (auto-fill-mode -1)
                      (set (make-local-variable 'company-backends) '(company-capf company-math-symbols-latex company-files))))
  (org-mode . visual-line-mode)
  :config
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  (setq org-startup-folded nil
        org-startup-indented t
        org-ellipsis "  "
        org-pretty-entities t
        org-hide-emphasis-markers t
        org-log-done 'time
        org-export-with-smart-quotes t
        org-log-redeadline 'time
        org-log-reschedule 'time
        org-fontify-whole-heading-line t
        org-fontify-done-headline t
        org-directory (expand-file-name "~/Dropbox/notes")
        org-default-notes-file (concat org-directory "/notes.org")
        org-agenda-files (mapcar (apply-partially #'concat org-directory) '("/tasks.org"))
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-agenda-text-search-extra-files '(agenda-archives)
        org-blank-before-new-entry (quote ((heading) (plain-list-item)))
        org-fontify-quote-and-verse-blocks t
        org-todo-keywords '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)"))
        org-pretty-entities-include-sub-superscripts t
        org-capture-templates '(("t" "Task Entry" entry (file "tasks.org") "* TODO %? CREATED: %t"))))

(use-package org-autolist
  :ensure t
  :after org
  :hook (org-mode . org-autolist-mode))

(use-package org-variable-pitch
  :ensure t
  :after org
  :hook (org-mode . org-variable-pitch-minor-mode))

(use-package org-bullets
  :ensure t
  :after org
  :hook (org-mode . org-bullets-mode))
;; (setq org-bullets-bullet-list '("◉" "⚫" "○" "►" "◇"))

(use-package org-evil :ensure t :after (evil org))

(use-package org-noter
  :ensure t
  :after org)

(use-package org-journal
  :ensure t
  :after org
  :config
  (setq org-journal-dir "~/wiki/journal/"
        org-journal-find-file #'find-file))

(use-package wc-mode
  :ensure t
  :hook org-mode)

(use-package ox-pandoc
  :ensure t
  :defer 5)

(use-package org-projectile
  :ensure t
  :after (org projectile)
  :config
  (org-projectile-per-project)
  (setq org-projectile-per-project-filepath "TODO.org"))

;;; Bibtex
(use-package biblio :ensure t)
(use-package biblio-core :ensure t)
(setq bibtex-dialect 'biblatex)

;;; Snippets
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode t)
  (advice-add 'company-complete-common :before (lambda () (setq my-company-point (point))))
  (advice-add 'company-complete-common :after (lambda () (when (equal my-company-point (point)) (yas-expand)))))

(use-package yasnippet-snippets :ensure t)

;; Theming and Interface

;;; Prettify
(global-prettify-symbols-mode t)

;; Modeline
(use-package doom-modeline
  :ensure t
  :defer t
  :hook (after-init . doom-modeline-init)
  :config (setq doom-modeline-height 15
                doom-modeline-enable-word-count t))

;;; Font
;;; Fira code
;; This works when using emacs --daemon + emacsclient
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (set-fontset-font t '(57600 . 57711) "Fira Code Symbol")))
;; This works when using emacs without server/client
(set-fontset-font t '(57600 . 57711) "Fira Code Symbol")
;; I haven't found one statement that makes both of the above situations work, so I use both for now

(defconst fira-code-font-lock-keywords-alist
  (mapcar (lambda (regex-char-pair)
            `(,(car regex-char-pair)
              (0 (prog1 ()
                   (compose-region (match-beginning 1)
                                   (match-end 1)
                                   ;; The first argument to concat is a string containing a literal tab
                                   ,(concat "	" (list (decode-char 'ucs (cadr regex-char-pair)))))))))
          '(("\\(www\\)" 57600)
            ("[^/]\\(\\*\\*\\)[^/]" 57601)
            ("\\(\\*\\*\\*\\)" 57602)
            ("\\(\\*\\*/\\)" 57603)
            ("\\(\\*>\\)" 57604)
            ("[^*]\\(\\*/\\)" 57605)
            ("\\(\\\\\\\\\\)" 57606)
            ("\\(\\\\\\\\\\\\\\)" 57607)
            ("\\({-\\)" 57608)
            ;; ("\\(\\[\\]\\)"                #Xe109)
            ("\\(::\\)" 57610)
            ("\\(:::\\)" 57611)
            ("[^=]\\(:=\\)" 57612)
            ("\\(!!\\)" 57613)
            ("\\(!=\\)" 57614)
            ("\\(!==\\)" 57615)
            ("\\(-}\\)" 57616)
            ("\\(--\\)" 57617)
            ("\\(---\\)" 57618)
            ("\\(-->\\)" 57619)
            ("[^-]\\(->\\)" 57620)
            ("\\(->>\\)" 57621)
            ("\\(-<\\)" 57622)
            ("\\(-<<\\)" 57623)
            ("\\(-~\\)" 57624)
            ("\\(#{\\)" 57625)
            ("\\(#\\[\\)" 57626)
            ("\\(##\\)" 57627)
            ("\\(###\\)" 57628)
            ("\\(####\\)" 57629)
            ("\\(#(\\)" 57630)
            ("\\(#\\?\\)" 57631)
            ("\\(#_\\)" 57632)
            ("\\((ly-raw clojure-reader-comment (\\))" 57633)
            ("\\(\\.-\\)" 57634)
            ("\\(\\.=\\)" 57635)
            ("\\(\\.\\.\\)" 57636)
            ("\\(\\.\\.<\\)" 57637)
            ("\\(\\.\\.\\.\\)" 57638)
            ("\\(\\?=\\)" 57639)
            ("\\(\\?\\?\\)" 57640)
            ("\\(;;\\)" 57641)
            ("\\(/\\*\\)" 57642)
            ("\\(/\\*\\*\\)" 57643)
            ("\\(/=\\)" 57644)
            ("\\(/==\\)" 57645)
            ("\\(/>\\)" 57646)
            ("\\(//\\)" 57647)
            ("\\(///\\)" 57648)
            ("\\(&&\\)" 57649)
            ("\\(||\\)" 57650)
            ("\\(||=\\)" 57651)
            ("[^|]\\(|=\\)" 57652)
            ("\\(|>\\)" 57653)
            ("\\(\\^=\\)" 57654)
            ("\\(\\$>\\)" 57655)
            ("\\(\\+\\+\\)" 57656)
            ("\\(\\+\\+\\+\\)" 57657)
            ("\\(\\+>\\)" 57658)
            ("\\(=:=\\)" 57659)
            ("[^!/]\\(==\\)[^>]" 57660)
            ("\\(===\\)" 57661)
            ("\\(==>\\)" 57662)
            ("[^=]\\(=>\\)" 57663)
            ("\\(=>>\\)" 57664)
            ("\\(<=\\)" 57665)
            ("\\(=<<\\)" 57666)
            ("\\(=/=\\)" 57667)
            ("\\(>-\\)" 57668)
            ("\\(>=\\)" 57669)
            ("\\(>=>\\)" 57670)
            ("[^-=]\\(>>\\)" 57671)
            ("\\(>>-\\)" 57672)
            ("\\(>>=\\)" 57673)
            ("\\(>>>\\)" 57674)
            ("\\(<\\*\\)" 57675)
            ("\\(<\\*>\\)" 57676)
            ("\\(<|\\)" 57677)
            ("\\(<|>\\)" 57678)
            ("\\(<\\$\\)" 57679)
            ("\\(<\\$>\\)" 57680)
            ("\\(<!--\\)" 57681)
            ("\\(<-\\)" 57682)
            ("\\(<--\\)" 57683)
            ("\\(<->\\)" 57684)
            ("\\(<\\+\\)" 57685)
            ("\\(<\\+>\\)" 57686)
            ("\\(<=\\)" 57687)
            ("\\(<==\\)" 57688)
            ("\\(<=>\\)" 57689)
            ("\\(<=<\\)" 57690)
            ("\\(<>\\)" 57691)
            ("[^-=]\\(<<\\)" 57692)
            ("\\(<<-\\)" 57693)
            ("\\(<<=\\)" 57694)
            ("\\(<<<\\)" 57695)
            ("\\(<~\\)" 57696)
            ("\\(<~~\\)" 57697)
            ("\\(</\\)" 57698)
            ("\\(</>\\)" 57699)
            ("\\(~@\\)" 57700)
            ("\\(~-\\)" 57701)
            ("\\(~=\\)" 57702)
            ("\\(~>\\)" 57703)
            ("[^<]\\(~~\\)" 57704)
            ("\\(~~>\\)" 57705)
            ("\\(%%\\)" 57706)
            ;; ("\\(x\\)"                   #Xe16b) This ended up being hard to do properly so I'm leaving it out.
            ("[^:=]\\(:\\)[^:=]" 57708)
            ("[^\\+<>]\\(\\+\\)[^\\+<>]" 57709)
            ("[^\\*/<>]\\(\\*\\)[^\\*/<>]" 57711))))

(defun add-fira-code-symbol-keywords ()
  (font-lock-add-keywords nil fira-code-font-lock-keywords-alist))

(add-hook 'prog-mode-hook
          #'add-fira-code-symbol-keywords)
(set-frame-font "Fira Code Retina-11") ;;; set default font
(setq default-frame-alist '((font . "Fira Code Retina-11")))

;;; Theme
(add-to-list 'custom-theme-load-path "~/projects/personal/emacs-nazgul-theme/")
;; (use-package eziam-dark-theme :ensure eziam-theme)

(load-theme 'nazgul t)
;; (load-theme 'tango t)

;;; Relative linum
(use-package linum-relative
  :ensure t
  :config
  (setq linum-relative-format "%4s ")
  (setq linum-relative-current-symbol "")
  (setq linum-relative-backend 'display-line-numbers-mode))

;;; TODO Highlight
(use-package hl-todo :ensure t
  :config
  (global-hl-todo-mode))

;;; Rainbow delimiters
(use-package rainbow-delimiters
  :ensure t
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;;; Formatters
(use-package format-all :ensure t)

;;; Highlight parens
(use-package highlight-parentheses
  :ensure t
  :config
  (global-highlight-parentheses-mode t))

;;; Focus
(use-package focus :ensure t)

;;; Browse kill ring
(use-package browse-kill-ring :ensure t)

;;; Golden ratio
(use-package golden-ratio
  :ensure t
  :config
  (golden-ratio-mode t)
  (add-to-list 'golden-ratio-exclude-modes 'which-key-mode)
  (add-to-list 'golden-ratio-inhibit-functions (lambda () (<= (count-lines (point-min) (point-max)) 20)))
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
 tramp-default-method "ssh"
 vc-follow-symlinks t
 select-enable-clipboard t
 make-backup-files nil
 coding-system-for-read 'utf-8
 coding-system-for-write 'utf-8
 locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))
(save-place-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)

;; Defaults
(setq-default
 show-trailing-whitespace nil
 indent-tabs-mode nil
 c-default-style "bsd"
 python-indent 2
 py-indent-offset 2
 lua-indent-level 2
 python-indent-offset 2
 tab-width 2
 c-basic-offset 2
 cperl-indent-level 2
 fill-column 100
 auto-fill-function 'do-auto-fill
 sentence-end-double-space nil)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

(add-hook 'prog-mode-hook (lambda () (setq-local comment-auto-fill-only-comments t) (auto-fill-mode t)))
(setq c-default-style "bsd")
(setq auto-window-vscroll nil)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(use-package dtrt-indent
  :ensure t
  :init
  (dtrt-indent-mode))

;; Evil settings
(setq evil-insert-state-cursor '("#268bd2" bar) ;; blue
      evil-normal-state-cursor '("#b58900" box) ;; blue
      evil-visual-state-cursor '("#cb4b16" box) ;; orange
      evil-replace-state-cursor '("#859900" hbar) ;; green
      evil-emacs-state-cursor '("#d33682" box)) ;; magenta

;; Minibuffer quitting with a single ESC
(defun minibuffer-keyboard-quit ()
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark t)
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
  "Switch window split from horizontally to vertically, or vice versa. i.e. change right window to bottom, or change bottom window to right."
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

;; Keybindings
(evil-leader/set-key
  "q" #'kill-emacs
  "w" #'save-buffer
  "d" (lambda () (interactive) (kill-buffer nil))
  "x" #'save-buffers-kill-terminal
  "a" #'previous-buffer
  "s" #'next-buffer
  "k" #'delete-window
  "z=" #'flyspell-auto-correct-word

  "bb" #'ivy-switch-buffer
  "bf" #'format-all-buffer

  "eo" #'flycheck-list-errors
  "ec" #'close-flycheck

  "ff" #'counsel-find-file
  "fr" #'counsel-recentf
  "fh" #'counsel-apropos
  "fi" #'counsel-rg
  "fl" #'counsel-locate
  "fp" #'counsel-projectile-switch-project
  "pf" #'counsel-projectile-find-file
  "fg" #'counsel-git
  "ft" #'counsel-etags-find-tag-at-point

  "gs" #'magit-status
  "gc" #'magit-commit
  "gp" #'magit-push
  "gl" #'magit-pull

  "ts" #'window-toggle-split-direction

  "c" #'projectile-compile-project
  "lr" #'xref-find-references
  "ln" #'lsp-rename
  "SPC" #'execute-extended-command
  "ol" #'org-store-link
  "oi" #'org-insert-link
  "oc" #'org-capture
  "oa" #'org-agenda
  "os" #'org-schedule
  "op" #'org-projectile-project-todo-completing-read
  "on" #'deft)

(define-key evil-normal-state-map [tab] #'ivy-switch-buffer)
(define-key evil-insert-state-map [C-tab] #'company-complete)
(add-hook 'server-done-hook 'kill-buffer)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-term-color-vector
   [unspecified "#282828" "#bc5353" "#7f9f7f" "#fddf8d" "#005fa7" "#dc8cc3" "#8cd0d3" nil] t)
 '(custom-safe-themes
   (quote
    ("9b35c097a5025d5da1c97dba45fed027e4fb92faecbd2f89c2a79d2d80975181" "57f95012730e3a03ebddb7f2925861ade87f53d5bbb255398357731a7b1ac0e0" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "075351c6aeaddd2343155cbcd4168da14f54284453b2f1c11d051b2687d6dc48" "e4fe3efbe5098392724aa0be119af539406553f58ef236d1514c8a80ec7ff557" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "242527ce24b140d304381952aa7a081179a9848d734446d913ca8ef0af3cef21" "44247f2a14c661d96d2bff302f1dbf37ebe7616935e4682102b68c0b6cc80095" "c9ddf33b383e74dac7690255dd2c3dfa1961a8e8a1d20e401c6572febef61045" "02956c6f9fc15711d3652ec42ddb43d4ae442da98dba72c7bdd9603525ce82aa" "ef03b74835e14db281cc489faf0d011e1c9255b747ba9c203426c56ed3331197" "058721e6836dfe4d18abbd35820eba7850427f59b9ac7c9c37a5e76f3a405749" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "8f137ccf060af657fbc0c1f7c3d406646ad04ebb8b3e025febc8ef432e958b02" default)))
 '(flycheck-pycheckers-max-line-length 100)
 '(font-latex-fontify-script nil)
 '(ivy-prescient-mode t)
 '(lsp-ui-sideline-delay 2.0)
 '(org-variable-pitch-fixed-font "Fira Code Retina-11")
 '(package-selected-packages
   (quote
    (company-quickhelp org-projectile cquery dtrt-indent golden-ratio browse-kill-ring focus highlight-parentheses format-all rainbow-delimiters hl-todo linum-relative doom-modeline yasnippet-snippets biblio ox-pandoc wc-mode org-journal org-noter org-evil org-bullets org-variable-pitch org-autolist org-plus-contrib deft ccls irony-eldoc flycheck-clangcheck flycheck-clang-analyzer flycheck-irony company-c-headers company-irony irony modern-cpp-font-lock google-c-style scribble-mode racket-mode flycheck-rust racer lsp-rust cargo rust-mode lsp-go scala-mode geiser fish-mode company-lua lua-mode toml-mode meson-mode cmake-font-lock cmake-mode lsp-javascript-typescript lsp-html yaml-mode company-math company-reftex company-auctex auctex-latexmk auctex markdown-toc flycheck-pycheckers lsp-python py-isort ein yapfify company-jedi company-anaconda anaconda-mode flycheck-ghcmod flycheck-haskell company-ghc company-ghci company-cabal lsp-haskell hindent intero ghc haskell-mode lsp-ocaml utop merlin tuareg ocp-indent slime-company company-lsp lsp-ui lsp-mode company-prescient prescient all-the-icons-dired all-the-icons-ivy company-posframe which-key highlight-indent-guides restart-emacs auto-dictionary flyspell-correct flycheck-pos-tip flycheck evil-goggles evil-lion evil-snipe evil-commentary evil-terminal-cursor-changer evil-magit evil-escape evil-embrace evil-visualstar evil-args evil-fringe-mark evil-matchit evil-surround evil-collection evil-leader evil-expat evil undo-tree parinfer lispyville lispy popup-kill-ring ialign rainbow-mode eyebrowse git-gutter esh-autosuggest ivy-posframe amx wgrep counsel-etags counsel-projectile counsel ivy-xref ivy-rich swiper ivy auto-compile use-package)))
 '(paradox-github-token t)
 '(projectile-completion-system (quote ivy)))
;; custom-set-faces was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-goggles-change-face ((t (:inherit diff-refine-removed))))
 '(evil-goggles-delete-face ((t (:inherit diff-refine-removed))))
 '(evil-goggles-paste-face ((t (:inherit diff-refine-added))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit diff-refine-added))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-refine-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-refine-removed))))
 '(evil-goggles-yank-face ((t (:inherit diff-refine-changed))))
 '(font-latex-italic-face ((t (:foreground "OliveDrab" :slant italic :family "RobotoMono Nerd Font"))))
 '(font-latex-math-face ((t (:foreground "burlywood" :height 110 :family "Fira Code Retina"))))
 '(font-latex-sectioning-5-face ((t (:inherit bold :weight bold))))
 '(font-latex-sedate-face ((t (:foreground "LightGray" :height 110 :family "Fira Code Retina"))))
 '(italic ((t (:underline nil :slant italic :family "ETBembo"))))
 '(ivy-confirm-face ((t (:inherit minibuffer-prompt :foreground "lemon chiffon"))))
 '(ivy-current-match ((t (:background "dim gray" :foreground "white smoke"))))
 '(lsp-face-highlight-read ((t (:background "#bc5353" :foreground "#efefef"))))
 '(lsp-face-highlight-textual ((t (:background "#bbbb8d" :foreground "black"))))
 '(lsp-face-highlight-write ((t (:background "#8fb28f" :foreground "#efefef"))))
 '(org-level-1 ((t (:inherit outline-1 :weight bold :height 1.75 :family "Source Sans Pro"))))
 '(org-level-2 ((t (:inherit outline-2 :weight bold :height 1.5 :family "Source Sans Pro"))))
 '(org-level-3 ((t (:inherit outline-3 :weight bold :height 1.25 :family "Source Sans Pro"))))
 '(org-level-4 ((t (:inherit outline-4 :weight bold :height 1.1 :family "Source Sans Pro"))))
 '(org-level-5 ((t (:inherit outline-5 :weight bold :family "Source Sans Pro"))))
 '(org-level-6 ((t (:inherit outline-6 :weight bold :family "Source Sans Pro"))))
 '(org-level-7 ((t (:inherit outline-7 :weight bold :family "Source Sans Pro"))))
 '(org-level-8 ((t (:inherit outline-8 :weight bold :family "Source Sans Pro"))))
 '(telephone-line-evil-emacs ((t (:inherit telephone-line-evil :background "#dc8cc3"))))
 '(telephone-line-evil-insert ((t (:inherit telephone-line-evil :background "#8fb28f"))))
 '(telephone-line-evil-motion ((t (:inherit telephone-line-evil :background "#005fa7"))))
 '(telephone-line-evil-normal ((t (:inherit telephone-line-evil :background "#bc5353"))))
 '(telephone-line-evil-visual ((t (:inherit telephone-line-evil :background "#ffbf8f"))))
 '(variable-pitch ((t (:family "Linux Libertine" :height 135)))))
;; custom-set-faces was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
(provide '.emacs)
;;; .emacs ends here

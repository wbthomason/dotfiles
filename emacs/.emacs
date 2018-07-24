;;; TODO: https://github.com/bling/fzf.el
;;; TODO: More consistently use :after
;;; TODO: Put keybindings with relevant packages
;;; TODO: Split into separate files

;;; Code:

;; From https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
(setq gc-cons-threshold-original gc-cons-threshold)
(setq gc-cons-threshold (* 1024 1024 100))
(setq file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)
(run-with-idle-timer
 5 nil
 (lambda ()
   (setq gc-cons-threshold (* 2 gc-cons-threshold-original))
   (setq file-name-handler-alist file-name-handler-alist-original)
   (makunbound 'gc-cons-threshold-original)
   (makunbound 'file-name-handler-alist-original)
   (message "gc-cons-threshold and file-name-handler-alist restored")))

(eval-when-compile (require 'cl))

(require 'package)
(setq package-archives
      '(("org"       . "https://orgmode.org/elpa/")
        ("melpa"     . "https://melpa.org/packages/")
        ("gnu"       . "https://elpa.gnu.org/packages/")))
(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; Byte-compile
(setq load-prefer-newer t)
(package-initialize)
(use-package auto-compile
  :ensure t
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

;; Local packages
(add-to-list 'load-path "~/.emacs.d/local")

;; Path-ish settings
(setenv "PKG_CONFIG_PATH" (concat "/opt/ros/melodic/lib/pkgconfig" ":/usr/local/lib/pkgconfig" ":/usr/local/lib64/pkgconfig/"(getenv "PKG_CONFIG_PATH")))
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
        ivy-re-builders-alist '((t . ivy--regex-fuzzy))
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
        ivy-rich-switch-buffer-align-virtual-buffer t)
  (setq ivy-rich-path-style 'abbrev)
  (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer))

(use-package ivy-xref
  :ensure t
  :init (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package counsel :ensure t)

(use-package counsel-projectile :ensure t)
;; (use-package ivy-bibtex :ensure t)

(use-package wgrep :ensure t)

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

;;; Smartparens
;; (use-package smartparens :ensure t
;;   :hook ((clojure-mode emacs-lisp-mode common-lisp-mode scheme-mode lisp-mode c++-mode) . smartparens-mode)
;;   :config
;;   (sp-local-pair 'tuareg-mode "'" nil :actions nil)
;;   (sp-local-pair 'tuareg-mode "`" nil :actions nil)
;;   (sp-with-modes 'c++-mode
;;     (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET"))))
;;   (sp-local-pair 'c++-mode "/*" "*/" :post-handlers '((" | " "SPC")
;;                                                       ("* ||\n[i]" "RET")))
;;   (sp-with-modes '(clojure-mode emacs-lisp-mode common-lisp-mode scheme-mode lisp-mode)
;;     ;; disable ', it's the quote character!
;;     (sp-local-pair "'" nil :actions nil)
;;     ;; also only use the pseudo-quote inside strings where it
;;     ;; serves as hyperlink.
;;     (sp-local-pair "`" "'" :when '(sp-in-string-p sp-in-comment-p))))

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
  :hook ((clojure-mode emacs-lisp-mode common-lisp-mode scheme-mode lisp-mode) . parinfer-mode)
  :config
  (progn
    (setq parinfer-extensions
          '(defaults       ; should be included.
             evil           ; If you use Evil.
             smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
             smart-yank))   ; Yank behavior depend on mode.
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

(use-package evil-magit :ensure t)

(use-package evil-terminal-cursor-changer
  :ensure t
  :after evil
  :unless (display-graphic-p)
  :config (evil-terminal-cursor-changer-activate))

;; (use-package evil-cleverparens :ensure t
;;   :hook ((emacs-lisp-mode
;;           eval-expression-minibuffer-setup
;;           ielm-mode
;;           lisp-mode
;;           lisp-interaction-mode
;;           scheme-mode) . evil-cleverparens-mode)
;;   :config
;;   (require 'evil-cleverparens-text-objects))

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
  (setq evil-goggles-duration 0.050)
  (evil-goggles-mode)
  (evil-goggles-use-diff-refine-faces))

;;; Tmux navigator
(use-package navigate :ensure t)
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
  (setq flycheck-check-syntax-automatically '(save idle-change))
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

(use-package flycheck-pos-tip
  :ensure t
  :config
  (with-eval-after-load 'flycheck
    (flycheck-pos-tip-mode)))

;;; Spell checking
;; (use-package flyspell
;;   :ensure t
;;   :hook (text-mode . flyspell-mode)

;;   :config
;;   (setq flyspell-issue-message-flag nil))

;; (use-package flyspell-correct
;;   :ensure t
;;   :commands (flyspell-correct-word-generic
;;              flyspell-correct-previous-word-generic))

;; (use-package auto-dictionary
;;   :ensure t
;;   :hook (flyspell-mode . auto-dictionary-mode))

;;; Restart Emacs
(use-package restart-emacs :ensure t)

;;; Indentation guides
(use-package highlight-indent-guides
  :ensure t
  ;; :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))

;;; Projectile
(use-package projectile :ensure t
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
(use-package magit :ensure t)
(use-package magit-todos :ensure t)

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
  (add-to-list 'company-backends 'company-capf)
  (define-key company-active-map [tab] 'company-select-next)
  (define-key company-active-map [S-tab] 'company-select-previous)
  (setq company-backends (delete 'company-semantic company-backends))
  (setq company-idle-delay 0.2
        ;; company-frontends '(company-pseudo-tooltip-unless-just-one-frontend company-preview-frontend company-echo-metadata-frontend)
        company-minimum-prefix-length 2
        company-require-match nil
        company-tooltip-align-annotations t
        company-show-numbers t
        company-selection-wrap-around t))

;;; NOTE: This package breaks company and the icons are too large. Maybe revisit in the future (7/3/2018)
;; (use-package company-box
;;   :ensure t
;;   :hook (company-mode . company-box-mode))

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

(use-package ivy-prescient
  :ensure t
  :after ivy
  :config (ivy-prescient-mode))

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
  (add-hook 'lsp-before-open-hook #'my-set-projectile-root))

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode))

(use-package company-lsp :ensure t
  :init
  (setq company-lsp-async t
        company-lsp-cache-candidates t
        company-lsp-enable-recompletion t)
  :config
  (push 'company-lsp company-backends))

(use-package company-quickhelp :ensure t
  :config
  (company-quickhelp-mode))

;; Languages
;; Common Lisp
(use-package slime-company :ensure t)
(use-package slime
  :ensure t
  :hook (slime-mode . (lambda () (unless (slime-connected-p) (save-excursion (slime)))))
  :init
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (setq slime-contribs '(slime-fancy slime-company slime-asdf)))


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
  :config
  (add-to-list 'company-backends 'company-cabal))

(use-package company-ghci
  :ensure t
  :config
  (push 'company-ghci company-backends))

(use-package company-ghc
  :ensure t
  :config
  (add-to-list 'company-backends 'company-ghc))

(use-package flycheck-haskell
  :ensure t
  :after flycheck
  :hook (flycheck-mode . flycheck-haskell-setup))

(use-package flycheck-ghcmod
  :ensure t
  :after flycheck)

;;; Python
(setenv "PYTHONPATH" "/opt/ros/melodic/lib/python2.7/site-packages")

(use-package python :ensure t)
(use-package anaconda-mode
  :ensure t
  :diminish anaconda-mode
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode)))

(use-package company-anaconda
  :ensure t
  :after (company anaconda-mode)
  :hook (python-mode . (lambda () (add-to-list 'company-backends 'company-anaconda))))

(use-package company-jedi  :ensure t
  :hook (python-mode . (lambda ()
                         (add-to-list 'company-backends 'company-jedi)
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
  :hook (doc-view-mode . auto-revert-mode)
  :config
  (add-to-list 'TeX-view-program-list
               '("Zathura"
                 ("zathura %o"
                  (mode-io-correlate " --synctex-forward %n:0:%b -x \"emacsclient --socket-name=%sn --no-wait +%{line} %{input}\""))
                 "zathura"))
  (setq TeX-PDF-mode   t
        TeX-auto-save  t
        TeX-parse-self t
        TeX-engine 'xetex
        TeX-source-correlate-mode t
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
  :after (auctex company)
  :config
  (company-auctex-init))

(use-package reftex
  :ensure t
  :hook (LaTeX-mode . turn-on-reftex)
  :config
  (setq reftex-plug-into-AUCTeX '(nil nil t t t)
        reftex-use-fonts t))

(use-package company-reftex
  :ensure t
  :config
  (add-to-list 'company-backends 'company-reftex-labels)
  (add-to-list 'company-backends 'company-reftex-citations))

(use-package company-math
  :ensure t
  :after company
  :config
  (add-to-list 'company-backends 'company-math-symbols-latex))

;;; YAML
(use-package yaml-mode :ensure t)

;;; HTML/Web
(use-package mhtml-mode :ensure t)
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

;;; Racket
(use-package racket-mode :ensure t)

(use-package scribble-mode :ensure t)

;;; C++
(add-hook 'c++-mode-hook #'electric-pair-mode)
(use-package cc-mode :ensure t)

(use-package modern-cpp-font-lock :ensure t)

(use-package company-irony
  :ensure t
  :hook ((irony-mode . company-irony-setup-begin-commands)
         (irony-mode . irony-cdb-autosetup-compile-options)
         ((c++mode c-mode objc-mode) . irony-mode))
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

(use-package irony-eldoc
  :ensure t
  :hook (irony-mode . irony-eldoc))

(use-package cquery
  :ensure t
  :commands lsp-cquery-enable
  :hook (c-mode-common . cquery//enable)
  :config
  (setq cquery-executable "/usr/bin/cquery")
  :init
  (defun cquery//enable ()
    (condition-case nil
        (lsp-cquery-enable)
      (user-error nil))))

;; (use-package ccls
;;   :ensure t
;;   :hook (c-mode-common . lsp-ccls-enable)
;;   :config
;;   (setq ccls-executable "/usr/bin/ccls"))

;;; Deft
(use-package deft
  :ensure t
  :commands (deft)
  :config
  (setq deft-directory "~/wiki/notes"
        deft-extensions '("md" "org")))

;;; Org
(use-package org :ensure org-plus-contrib
  :hook (org-mode . (lambda ()
                      (setq buffer-face-mode-face '(:family "ETBembo" :height 120))
                      (buffer-face-mode)
                      (linum-relative-mode -1)))
  :config
  (setq org-startup-folded nil
        org-startup-indented t
        org-ellipsis "  "
        org-pretty-entities t
        org-hide-emphasis-markers t
        org-log-done 'time
        org-fontify-whole-heading-line t
        org-fontify-done-headline t
        org-directory (expand-file-name "~/Dropbox/notes")
        org-default-notes-file (concat org-directory "/notes.org")
        org-agenda-files (concat org-directory "/agenda.org")
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-fontify-quote-and-verse-blocks t
        org-todo-keywords '((sequence "☛ TODO(t)" "|" "✔ DONE(d)") (sequence "⚑ WAITING(w)" "|") (sequence "|" "✘ CANCELED(c)"))
        org-pretty-entities-include-sub-superscripts t))

(define-key global-map "\C-cc" 'org-capture)

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
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("◉" "⚫" "○" "►" "◇")))

(use-package org-evil :ensure t :after (evil org))

(use-package org-noter :ensure t :after org)

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

;;; Telephone Line
(use-package telephone-line
  :ensure t
  :config
  (setq telephone-line-primary-left-separator 'telephone-line-cubed-left
        telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left
        telephone-line-primary-right-separator 'telephone-line-cubed-right
        telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)
  (setq telephone-line-height 15)
  (telephone-line-mode t))

;;; Font
;;; Fira code
;; This works when using emacs --daemon + emacsclient
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")))
;; This works when using emacs without server/client
(set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")
;; I haven't found one statement that makes both of the above situations work, so I use both for now

(defconst fira-code-font-lock-keywords-alist
  (mapcar (lambda (regex-char-pair)
            `(,(car regex-char-pair)
              (0 (prog1 ()
                   (compose-region (match-beginning 1)
                                   (match-end 1)
                                   ;; The first argument to concat is a string containing a literal tab
                                   ,(concat "	" (list (decode-char 'ucs (cadr regex-char-pair)))))))))
          '(("\\(www\\)"                   #Xe100)
            ("[^/]\\(\\*\\*\\)[^/]"        #Xe101)
            ("\\(\\*\\*\\*\\)"             #Xe102)
            ("\\(\\*\\*/\\)"               #Xe103)
            ("\\(\\*>\\)"                  #Xe104)
            ("[^*]\\(\\*/\\)"              #Xe105)
            ("\\(\\\\\\\\\\)"              #Xe106)
            ("\\(\\\\\\\\\\\\\\)"          #Xe107)
            ("\\({-\\)"                    #Xe108)
            ;; ("\\(\\[\\]\\)"                #Xe109)
            ("\\(::\\)"                    #Xe10a)
            ("\\(:::\\)"                   #Xe10b)
            ("[^=]\\(:=\\)"                #Xe10c)
            ("\\(!!\\)"                    #Xe10d)
            ("\\(!=\\)"                    #Xe10e)
            ("\\(!==\\)"                   #Xe10f)
            ("\\(-}\\)"                    #Xe110)
            ("\\(--\\)"                    #Xe111)
            ("\\(---\\)"                   #Xe112)
            ("\\(-->\\)"                   #Xe113)
            ("[^-]\\(->\\)"                #Xe114)
            ("\\(->>\\)"                   #Xe115)
            ("\\(-<\\)"                    #Xe116)
            ("\\(-<<\\)"                   #Xe117)
            ("\\(-~\\)"                    #Xe118)
            ("\\(#{\\)"                    #Xe119)
            ("\\(#\\[\\)"                  #Xe11a)
            ("\\(##\\)"                    #Xe11b)
            ("\\(###\\)"                   #Xe11c)
            ("\\(####\\)"                  #Xe11d)
            ("\\(#(\\)"                    #Xe11e)
            ("\\(#\\?\\)"                  #Xe11f)
            ("\\(#_\\)"                    #Xe120)
            ("\\(#_(\\)"                   #Xe121)
            ("\\(\\.-\\)"                  #Xe122)
            ("\\(\\.=\\)"                  #Xe123)
            ("\\(\\.\\.\\)"                #Xe124)
            ("\\(\\.\\.<\\)"               #Xe125)
            ("\\(\\.\\.\\.\\)"             #Xe126)
            ("\\(\\?=\\)"                  #Xe127)
            ("\\(\\?\\?\\)"                #Xe128)
            ("\\(;;\\)"                    #Xe129)
            ("\\(/\\*\\)"                  #Xe12a)
            ("\\(/\\*\\*\\)"               #Xe12b)
            ("\\(/=\\)"                    #Xe12c)
            ("\\(/==\\)"                   #Xe12d)
            ("\\(/>\\)"                    #Xe12e)
            ("\\(//\\)"                    #Xe12f)
            ("\\(///\\)"                   #Xe130)
            ("\\(&&\\)"                    #Xe131)
            ("\\(||\\)"                    #Xe132)
            ("\\(||=\\)"                   #Xe133)
            ("[^|]\\(|=\\)"                #Xe134)
            ("\\(|>\\)"                    #Xe135)
            ("\\(\\^=\\)"                  #Xe136)
            ("\\(\\$>\\)"                  #Xe137)
            ("\\(\\+\\+\\)"                #Xe138)
            ("\\(\\+\\+\\+\\)"             #Xe139)
            ("\\(\\+>\\)"                  #Xe13a)
            ("\\(=:=\\)"                   #Xe13b)
            ("[^!/]\\(==\\)[^>]"           #Xe13c)
            ("\\(===\\)"                   #Xe13d)
            ("\\(==>\\)"                   #Xe13e)
            ("[^=]\\(=>\\)"                #Xe13f)
            ("\\(=>>\\)"                   #Xe140)
            ("\\(<=\\)"                    #Xe141)
            ("\\(=<<\\)"                   #Xe142)
            ("\\(=/=\\)"                   #Xe143)
            ("\\(>-\\)"                    #Xe144)
            ("\\(>=\\)"                    #Xe145)
            ("\\(>=>\\)"                   #Xe146)
            ("[^-=]\\(>>\\)"               #Xe147)
            ("\\(>>-\\)"                   #Xe148)
            ("\\(>>=\\)"                   #Xe149)
            ("\\(>>>\\)"                   #Xe14a)
            ("\\(<\\*\\)"                  #Xe14b)
            ("\\(<\\*>\\)"                 #Xe14c)
            ("\\(<|\\)"                    #Xe14d)
            ("\\(<|>\\)"                   #Xe14e)
            ("\\(<\\$\\)"                  #Xe14f)
            ("\\(<\\$>\\)"                 #Xe150)
            ("\\(<!--\\)"                  #Xe151)
            ("\\(<-\\)"                    #Xe152)
            ("\\(<--\\)"                   #Xe153)
            ("\\(<->\\)"                   #Xe154)
            ("\\(<\\+\\)"                  #Xe155)
            ("\\(<\\+>\\)"                 #Xe156)
            ("\\(<=\\)"                    #Xe157)
            ("\\(<==\\)"                   #Xe158)
            ("\\(<=>\\)"                   #Xe159)
            ("\\(<=<\\)"                   #Xe15a)
            ("\\(<>\\)"                    #Xe15b)
            ("[^-=]\\(<<\\)"               #Xe15c)
            ("\\(<<-\\)"                   #Xe15d)
            ("\\(<<=\\)"                   #Xe15e)
            ("\\(<<<\\)"                   #Xe15f)
            ("\\(<~\\)"                    #Xe160)
            ("\\(<~~\\)"                   #Xe161)
            ("\\(</\\)"                    #Xe162)
            ("\\(</>\\)"                   #Xe163)
            ("\\(~@\\)"                    #Xe164)
            ("\\(~-\\)"                    #Xe165)
            ("\\(~=\\)"                    #Xe166)
            ("\\(~>\\)"                    #Xe167)
            ("[^<]\\(~~\\)"                #Xe168)
            ("\\(~~>\\)"                   #Xe169)
            ("\\(%%\\)"                    #Xe16a)
            ;; ("\\(x\\)"                   #Xe16b) This ended up being hard to do properly so I'm leaving it out.
            ("[^:=]\\(:\\)[^:=]"           #Xe16c)
            ("[^\\+<>]\\(\\+\\)[^\\+<>]"   #Xe16d)
            ("[^\\*/<>]\\(\\*\\)[^\\*/<>]" #Xe16f))))

(defun add-fira-code-symbol-keywords ()
  (font-lock-add-keywords nil fira-code-font-lock-keywords-alist))

(add-hook 'prog-mode-hook
          #'add-fira-code-symbol-keywords)
(set-frame-font "-*-fira code retina-medium-*-*-*-12-*-*-*-*-0-iso10646-1") ;;; set default font
(setq default-frame-alist '((font . "-*-fira code retina-medium-*-*-*-12-*-*-*-*-0-iso10646-1")))

;;; Theme
;; (use-package spacemacs-common :ensure spacemacs-theme
;;   :config
;;   (add-hook 'after-init-hook (lambda () (load-theme 'spacemacs-light t))))

(add-to-list 'custom-theme-load-path "~/projects/personal/emacs-nazgul-theme/")
(load-theme 'nazgul t)

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
  "q"  #'kill-emacs
  "w"  #'save-buffer
  "d"  (lambda () (interactive) (kill-buffer nil))
  "x"  #'save-buffers-kill-terminal
  "a"  #'previous-buffer
  "s"  #'next-buffer
  "k"  #'delete-window
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

  "gs" #'magit-status
  "gc" #'magit-commit
  "gp" #'magit-push
  "gl" #'magit-pull

  "ts" #'window-toggle-split-direction

  "c"  #'projectile-compile-project
  "lr" #'xref-find-references
  "ln" #'lsp-rename
  "SPC" #'execute-extended-command)

(define-key evil-normal-state-map [tab] 'ivy-switch-buffer)
(define-key evil-insert-state-map [C-tab] 'company-complete)
(add-hook 'server-done-hook 'kill-buffer)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#bcbcbc" "#d70008" "#5faf00" "#875f00" "#268bd2" "#800080" "#008080" "#5f5f87"])
 '(ansi-term-color-vector
   [unspecified "#282828" "#bc5353" "#7f9f7f" "#fddf8d" "#005fa7" "#dc8cc3" "#8cd0d3" nil] t)
 '(custom-safe-themes
   (quote
    ("02956c6f9fc15711d3652ec42ddb43d4ae442da98dba72c7bdd9603525ce82aa" "ef03b74835e14db281cc489faf0d011e1c9255b747ba9c203426c56ed3331197" "058721e6836dfe4d18abbd35820eba7850427f59b9ac7c9c37a5e76f3a405749" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "8f137ccf060af657fbc0c1f7c3d406646ad04ebb8b3e025febc8ef432e958b02" default)))
 '(flycheck-pycheckers-max-line-length 100)
 '(lsp-ui-sideline-delay 2.0)
 '(package-selected-packages
   (quote
    (deft scribble-mode yasnippet-snippets yasnippet company-ocp-index magit-todos flycheck-pycheckers yapfify yaml-mode which-key wgrep utop use-package tuareg toml-mode telephone-line slime-company scala-mode restart-emacs rainbow-mode rainbow-delimiters racket-mode racer python-mode py-isort popup-kill-ring parinfer org-variable-pitch org-plus-contrib org-noter org-evil org-bullets org-autolist ocp-indent navigate modern-cpp-font-lock meson-mode merlin markdown-toc lsp-ui lsp-rust lsp-python lsp-ocaml lsp-javascript-typescript lsp-html lsp-haskell lsp-go lispyville linum-relative ivy-xref ivy-rich ivy-prescient irony-eldoc intero ialign hl-todo hindent highlight-parentheses highlight-indent-guides golden-ratio git-gutter geiser format-all focus flyspell-correct flycheck-rust flycheck-pos-tip flycheck-irony flycheck-haskell flycheck-ghcmod fish-mode eyebrowse evil-visualstar evil-terminal-cursor-changer evil-snipe evil-matchit evil-magit evil-lion evil-leader evil-goggles evil-fringe-mark evil-expat evil-escape evil-embrace evil-commentary evil-collection evil-args esup esh-autosuggest elpy ein dtrt-indent cquery counsel-projectile company-reftex company-quickhelp company-prescient company-math company-lua company-lsp company-jedi company-irony company-ghci company-ghc company-cabal company-c-headers company-auctex company-anaconda cmake-font-lock cargo browse-kill-ring biblio auto-package-update auto-dictionary auto-compile auctex-latexmk all-the-icons-ivy all-the-icons-dired)))
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
 '(font-latex-sectioning-5-face ((t (:inherit bold :weight bold))))
 '(italic ((t (:underline nil :slant italic :family "ETBembo"))))
 '(ivy-confirm-face ((t (:inherit minibuffer-prompt :foreground "lemon chiffon"))))
 '(ivy-current-match ((t (:background "dim gray" :foreground "white smoke"))))
 '(lsp-face-highlight-read ((t (:background "#bc5353" :foreground "#efefef"))))
 '(lsp-face-highlight-textual ((t (:background "#bbbb8d" :foreground "black"))))
 '(lsp-face-highlight-write ((t (:background "#8fb28f" :foreground "#efefef"))))
 '(telephone-line-evil-emacs ((t (:inherit telephone-line-evil :background "#dc8cc3"))))
 '(telephone-line-evil-insert ((t (:inherit telephone-line-evil :background "#8fb28f"))))
 '(telephone-line-evil-motion ((t (:inherit telephone-line-evil :background "#005fa7"))))
 '(telephone-line-evil-normal ((t (:inherit telephone-line-evil :background "#bc5353"))))
 '(telephone-line-evil-visual ((t (:inherit telephone-line-evil :background "#ffbf8f")))))
;; custom-set-faces was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
(provide '.emacs)
;;; .emacs ends here

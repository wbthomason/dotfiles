;;; TODO: https://github.com/bling/fzf.el
;;; https://brainlessdeveloper.com/2017/12/27/making-emacs-work-like-my-vim-setup/
;;; Faster start due to 26 feature?
;;; https://tvraman.github.io/emacspeak/blog/emacs-start-speed-up.html

;;; Code:

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

;; Auto Package Update
(use-package auto-package-update :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;; General Packages
;;; Ivy
(use-package ivy :ensure t
  :diminish ivy-mode
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-display-style 'fancy)
  (setq ivy-format-function 'ivy-format-function-line)
  (with-eval-after-load 'ivy
    (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)
    (define-key ivy-minibuffer-map (kbd "C-a") 'ivy-read-action)
    (define-key ivy-minibuffer-map (kbd "C-f") 'ivy-toggle-fuzzy)))

(use-package ivy-rich :ensure t
  :config
  (setq ivy-virtual-abbreviate 'full
        ivy-rich-switch-buffer-align-virtual-buffer t)
  (setq ivy-rich-path-style 'abbrev)
  (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer))

(use-package ivy-xref :ensure t
  :init (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package counsel-projectile :ensure t)
(use-package ivy-bibtex :ensure t)

;;; Eshell
(use-package esh-autosuggest :ensure t
  :hook (eshell-mode . esh-autosuggest-mode))

;;; Undotree
(use-package undo-tree :ensure t
  :init (setq undo-tree-auto-save-history t))

;;; PDF
; (use-package pdf-tools
;  :config
;  (pdf-tools-install)
;  (setq-default pdf-view-display-size 'fit-page)
;  (setq pdf-annot-activate-created-annotations t)
;  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))

;;; Git gutter
(use-package git-gutter :ensure t
  :init
  (global-git-gutter-mode t)
  (setq git-gutter:modified-sign "＊"
        git-gutter:added-sign "+"
        git-gutter:deleted-sign "～"
        git-gutter:window-width 2
        git-gutter:handled-backends '(git hg bzr svn))
  (set-face-foreground 'git-gutter:modified "#8ec07c") ;; background color
  (set-face-foreground 'git-gutter:added "#b8bb26")
  (set-face-foreground 'git-gutter:deleted "cc241d"))

;;; Rainbow mode
(use-package rainbow-mode :ensure t)

;;; Interactive align
(use-package ialign :ensure t)

;;; Popup kill ring
(use-package popup-kill-ring :ensure t)

;;; Smartparens
(use-package smartparens :ensure t
  :init
  (add-hook 'clojure-mode-hook #'smartparens-mode)
  (add-hook 'emacs-lisp-mode-hook #'smartparens-mode)
  (add-hook 'common-lisp-mode-hook #'smartparens-mode)
  (add-hook 'scheme-mode-hook #'smartparens-mode)
  (add-hook 'lisp-mode-hook #'smartparens-mode)
  (add-hook 'c++-mode-hook #'smartparens-mode)
  :config
  (sp-local-pair 'tuareg-mode "'" nil :actions nil)
  (sp-local-pair 'tuareg-mode "`" nil :actions nil)
  (sp-with-modes 'emacs-lisp-mode
    ;; disable ', it's the quote character!
    (sp-local-pair "'" nil :actions nil)
    ;; also only use the pseudo-quote inside strings where it
    ;; serves as hyperlink.
    (sp-local-pair "`" "'" :when '(sp-in-string-p sp-in-comment-p))))

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
  :after evil
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>"))

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

(use-package evil-visual-mark-mode
  :ensure t
  :after evil
  :config
  (evil-visual-mark-mode))

(use-package evil-args
  :ensure t
  :after evil
  :config
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg))

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

(use-package evil-snipe :ensure t
  :diminish evil-snipe-mode
  :init
  (setq evil-snipe-scope 'buffer
        evil-snipe-repeat-scope 'whole-buffer)
  :config
  (evil-snipe-mode t)
  (evil-snipe-override-mode t)
  (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode))

(use-package evil-lion :ensure t
  :config
  (evil-lion-mode))

(use-package evil-goggles :ensure t
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

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

(use-package flycheck-pos-tip :ensure t
  :init
  (with-eval-after-load 'flycheck
    (flycheck-pos-tip-mode)))

;;; Spell checking
(use-package flyspell :ensure t
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

(use-package auto-dictionary :ensure t
  :init
  (add-hook 'flyspell-mode-hook 'auto-dictionary-mode))


;;; Restart Emacs
(use-package restart-emacs :ensure t)

;;; Relative linum
(use-package linum-relative :ensure t
  :config
  (setq linum-relative-format "%3s ")
  (setq linum-relative-current-symbol "")
  (setq linum-relative-backend 'display-line-numbers-mode))

;;; Indentation guides
(use-package highlight-indent-guides :ensure t
  :init
  :config
  (setq highlight-indent-guides-method 'character)
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

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
  :diminish company-mode
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (add-to-list 'company-backends 'company-capf)
  (define-key company-active-map [tab] 'company-select-next)
  (define-key company-active-map [S-tab] 'company-select-previous)
  (setq company-backends (delete 'company-semantic company-backends))
  :init
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2
        company-require-match nil
        company-tooltip-align-annotations t
        company-show-numbers            t
        company-tooltip-limit 20
        company-selection-wrap-around t))

;;; Snippets
(use-package yasnippet :ensure t
  :config (setq company-backends
                (mapcar
                 (lambda (backend)
                   (if (and (listp backend) (member 'company-yasnippet backend))
                       backend
                     (append
                      (if (consp backend) backend (list backend))
                      '(:with company-yasnippet)))) company-backends)))
(use-package yasnippet-snippets :ensure t)

;;; LSP
;; Rust, Python, Javascript, Bash, and PHP work out of the box
;; (use-package eglot :ensure t
;;   :config
;;   (add-to-list 'eglot-server-programs '(tuareg-mode . ("ocaml-language-server" "--stdio")))
;;   (add-to-list 'eglot-server-programs '(haskell-mode . ("hie" "--lsp")))
;;   (add-to-list 'eglot-server-programs '(common-lisp-mode . ("cl-lsp"))))

(use-package lsp-mode :ensure t
  :config
  (defun my-set-projectile-root ()
    (when lsp--cur-workspace
      (setq projectile-project-root (lsp--workspace-root lsp--cur-workspace))))
  (add-hook 'lsp-before-open-hook #'my-set-projectile-root))

(use-package lsp-ui :ensure t
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

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

(use-package fuzzy :ensure t)

;; Languages
;;; OCaml
;;;; OCP-indent
(eval-and-compile
  (defun opam-share ()
    (substring
     (shell-command-to-string "opam config var share 2> /dev/null")
     0 -1)))

(use-package ocp-indent  :ensure t
  :load-path (lambda () ( concat ( opam-share ) "/emacs/site-lisp")))

;;;; Tuareg
(use-package tuareg  :ensure t
  :config
  (add-hook 'tuareg-mode-hook
            (lambda () (setq indent-line-function 'ocp-indent-line))))

;;;; Merlin
(use-package merlin :ensure t
  :init
  (add-hook 'tuareg-mode-hook 'merlin-mode)
  (setq merlin-completion-with-doc t))

;;;; Utop
(use-package utop :ensure t
  :init
  (add-hook 'tuareg-mode-hook 'utop-minor-mode)
  (setq utop-command "opam config exec -- utop -emacs"))

(use-package lsp-ocaml :ensure t
  :config
  (add-hook 'tuareg-mode-hook #'lsp-ocaml-enable)
  (add-hook 'caml-mode-hook #'lsp-ocaml-enable)
  (add-hook 'reason-mode-hook #'lsp-ocaml-enable))

;;; Haskell

(use-package haskell-mode :ensure t)

(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

(use-package intero :ensure t
  :config
  (add-hook 'haskell-mode-hook 'intero-mode))

(use-package hindent :ensure t
  :config
  (add-hook 'haskell-mode-hook #'hindent-mode))

(use-package lsp-haskell :ensure t
  :config
  (add-hook 'haskell-mode-hook #'lsp-haskell-enable))

(use-package company-cabal :ensure t
  :config
  (add-to-list 'company-backends 'company-cabal))

(use-package company-ghci :ensure t
  :config
  (push 'company-ghci company-backends))

(use-package company-ghc :ensure t
  :config
  (add-to-list 'company-backends 'company-ghc))

(use-package flycheck-haskell :ensure t
  :config
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)))

;;; Python
(use-package python-mode  :ensure t)
(use-package company-jedi  :ensure t
  :init
  (add-hook 'python-mode-hook
            (lambda ()
              (add-to-list 'company-backends 'company-jedi)
              (add-hook 'python-mode-hook 'jedi:setup)))
  :config
  (setq jedi:complete-on-dot t))

(use-package py-yapf  :ensure t
  :init
  (add-hook 'python-mode-hook 'py-yapf-enable-on-save))

(use-package ein  :ensure t
  :init
  (setq ein:use-auto-complete-superpack t))

(use-package py-isort  :ensure t
  :config
  (add-hook 'before-save-hook 'py-isort-before-save))

(use-package lsp-python :ensure t
  :config
  (add-hook 'python-mode-hook #'lsp-python-enable))

;;; Markdown
(use-package markdown-mode  :ensure t)
(use-package markdown-toc  :ensure t)

;;; LaTeX
(use-package auctex  :ensure t
  :config
  (setq TeX-PDF-mode   t
        TeX-auto-save  t
        TeX-parse-self t)
  (setq-default TeX-master nil))

(use-package auctex-latexmk :ensure t)
(use-package company-auctex :ensure t)
(use-package reftex
  :init
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX '(nil nil t t t)
        reftex-use-fonts t))

;;; YAML
(use-package yaml-mode :ensure t)

;;; Meson
(use-package meson-mode :ensure t  :config)

;;; TOML
(use-package toml-mode :ensure t)

;;; Lua
(use-package lua-mode :ensure t)
(lsp-define-stdio-client
 lsp-lua-mode
 "lua"
 (lambda () default-directory)
 '("/home/wil/.luarocks/bin/lua-lsp"))
(add-hook 'lua-mode #'lsp-lua-mode-enable)

(use-package company-lua :ensure t
  :config
  (add-hook 'lua-mode (push 'company-lua company-backends)))

;;; Fish
(use-package fish-mode :ensure t)

;;; Bash
(use-package sh-script)

;;; Scheme
(use-package geiser :ensure t)

;;; Scala
(use-package scala-mode :ensure t)

;;; Rust
(use-package rust-mode :ensure t)
(use-package cargo :ensure t)
(use-package lsp-rust :ensure t
  :init
  (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls"))
  :config
  (add-hook 'rust-mode-hook #'lsp-rust-enable))

(use-package racer :ensure t
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode))

(use-package flycheck-rust :ensure t
  :config
  (with-eval-after-load 'rust-mode
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

;;; Racket
(use-package racket-mode :ensure t)

;;; C++
(use-package modern-cpp-font-lock :ensure t)

(use-package company-irony :ensure t
  :config
  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
  (setq company-irony-ignore-case 'smart)
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  (setq-default irony-cdb-compilation-databases '(irony-cdb-libclang irony-cdb-clang-complete))
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package company-c-headers :ensure t
  :init
  (add-hook 'c++-mode-hook (lambda () (push 'company-c-headers company-backends)))
  (add-hook 'c-mode-hook (lambda () (push 'company-c-headers company-backends))))

(use-package flycheck-irony :ensure t
  :init
  (with-eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))

(use-package irony-eldoc :ensure t
  :init
  (add-hook 'irony-mode-hook #'irony-eldoc))

(use-package clang-format :ensure t)

(use-package rtags :ensure t
  :config
  (add-hook 'kill-emacs-hook 'rtags-quit-rdm))

(use-package company-rtags :ensure t
  :config
  (setq rtags-autostart-diagnostics t)
  (rtags-diagnostics)
  (setq rtags-completions-enabled t)
  (push 'company-rtags company-backends))

(use-package cquery :ensure t
  :commands lsp-cquery-enable
  :config
  (setq cquery-executable "/usr/bin/cquery")
  :init
  (defun cquery//enable ()
    (condition-case nil
        (lsp-cquery-enable)
      (user-error nil)))
  (add-hook 'c-mode-common-hook #'cquery//enable))

;;; Org
(use-package org :ensure t)
(use-package org-ref :ensure t)

;;; Bibtex
(use-package biblio :ensure t)
(use-package biblio-core :ensure t)

;; Theming and Interface

;;; Powerline
(use-package powerline :ensure t)

(use-package powerline-evil :ensure t)

(use-package airline-themes :ensure t
  :config (load-theme 'airline-distinguished t))

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
            ("\\(\\[\\]\\)"                #Xe109)
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
(set-frame-font "Fira Code-9") ;;; set default font
(setq default-frame-alist '((font . "Fira Code-9")))
(set-face-font 'font-lock-comment-face "-pyrs-RobotoMono Nerd Font-normal-italic-normal-*-*-*-*-*-*-0-iso10646-1")

;;; Theme
;; (use-package grayscale-theme :ensure t
;;   :config
;;   (load-theme 'grayscale t))
(add-to-list 'custom-theme-load-path "~/projects/personal/emacs-nazgul-theme/")
(load-theme 'nazgul t)

;;; TODO Highlight
(use-package hl-todo :ensure t
  :config
  (global-hl-todo-mode))

;;; Rainbow delimiters
(use-package rainbow-delimiters :ensure t
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;;; Highlight parens
(use-package highlight-parentheses :ensure t
  :config
  (global-highlight-parentheses-mode t))

;;; Focus
(use-package focus :ensure t)

;;; Golden ratio
(use-package golden-ratio :ensure t
  :diminish golden-ratio-mode
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
(fset 'yes-or-no-p 'y-or-n-p)

;; Defaults
(setq-default
 show-trailing-whitespace nil
 indent-tabs-mode nil
 c-default-style "bsd"
 python-indent 2
 lua-indent-level 2
 python-indent-offset 2
 tab-width 2
 c-basic-offset 2
 cperl-indent-level 2
 fill-column 100
 auto-fill-function 'do-auto-fill
 sentence-end-double-space nil)

(setq c-default-style "bsd")
(setq auto-window-vscroll nil)
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
  "Switch window split from horizontally to vertically, or vice
versa. i.e. change right window to bottom, or change bottom window to
right."
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

(defun switch-to-last-buffer ()
  "Switch to the previously used buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

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
  "bl" 'switch-to-last-buffer
  "bf" 'format-buffer
  "ff" 'counsel-find-file
  "fr" 'counsel-recentf
  "fh" 'counsel-apropos
  "fi" 'counsel-rg
  "fl" 'counsel-locate
  "fp" 'counsel-projectile-switch-project
  "fg" 'counsel-git
  "gs" 'magit-status
  "gc" 'magit-commit
  "gp" 'magit-push
  "gl" 'magit-pull
  "ts" 'window-toggle-split-direction
  "c"  'projectile-compile-project)
(define-key evil-normal-state-map [backspace] 'ivy-switch-buffer)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-todo-keyword-faces
   (quote
    (("HOLD" . "#e6e6e6")
     ("TODO" . "#e6e6e6")
     ("NEXT" . "#e6e6e6")
     ("THEM" . "#e6e6e6")
     ("PROG" . "#e6e6e6")
     ("OKAY" . "#e6e6e6")
     ("DONT" . "#e6e6e6")
     ("FAIL" . "#e6e6e6")
     ("DONE" . "#e6e6e6")
     ("NOTE" . "#e6e6e6")
     ("KLUDGE" . "#e6e6e6")
     ("HACK" . "#e6e6e6")
     ("FIXME" . "#e6e6e6")
     ("XXX" . "#e6e6e6")
     ("XXXX" . "#e6e6e6")
     ("???" . "#e6e6e6"))))
 '(lsp-ui-sideline-delay 2.0)
 '(package-selected-packages
   (quote
    (hl-todo auto-package-update yasnippet-snippets company-lua lua-mode meson-mode zoom yaml-mode which-key utop use-package tuareg toml-mode scala-mode restart-emacs rainbow-mode rainbow-delimiters racket-mode racer python-mode py-yapf py-isort powerline-evil popup-kill-ring parinfer org-ref ocp-indent navigate modern-cpp-font-lock merlin markdown-toc lsp-ui lsp-rust lsp-python lsp-ocaml lsp-haskell linum-relative ivy-bibtex irony-eldoc intero ialign hindent highlight-indent-guides grayscale-theme golden-ratio git-gutter geiser fuzzy focus flyspell-correct-helm flycheck-rust flycheck-pos-tip flycheck-irony flycheck-haskell fish-mode evil-visualstar evil-visual-mark-mode evil-terminal-cursor-changer evil-surround evil-snipe evil-matchit evil-magit evil-leader evil-escape evil-commentary evil-collection evil-cleverparens evil-args ein eglot dtrt-indent drag-stuff cquery counsel-projectile company-rtags company-quickhelp company-lsp company-jedi company-irony company-ghci company-ghc company-cabal company-c-headers company-auctex clang-format cargo auto-dictionary auctex-latexmk airline-themes))))
 '(projectile-completion-system (quote ivy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cquery-code-lens-face ((t (:inherit shadow :background "dim gray"))))
 '(hl-todo ((t (:foreground "white" :weight bold))))
 '(lsp-ui-sideline-symbol ((t (:background "dim gray" :foreground "grey" :box (:line-width -1 :color "grey") :height 0.99))))
 '(lsp-ui-sideline-symbol-info ((t (:background "gray0" :slant italic :height 0.99)))))
 '(evil-goggles-change-face ((t (:inherit diff-removed))))
 '(evil-goggles-delete-face ((t (:inherit diff-removed))))
 '(evil-goggles-paste-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
 '(evil-goggles-yank-face ((t (:inherit diff-changed))))
 '(ivy-confirm-face ((t (:inherit minibuffer-prompt :foreground "lemon chiffon"))))
 '(ivy-current-match ((t (:background "dim gray" :foreground "white smoke"))))
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

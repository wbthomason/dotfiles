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
(setq load-prefer-newer t)
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

(eval-when-compile (require 'use-package))
(require 'bind-key)

(use-package paradox
  :ensure t
  :config
  (setq paradox-execute-asynchronously t)
  (define-key paradox-menu-mode-map (kbd "M-f") #'hydra-paradox-filter/body)
  (paradox-enable))

(use-package no-littering :ensure t)

;; Byte-compile
(use-package auto-compile
  :ensure t
  :config
  (setq auto-compile-display-buffer nil
        auto-compile-mode-line-counter t)
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
(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t
        ivy-display-style 'fancy
        ivy-height 200
        ivy-use-selectable-prompt t)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
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

(use-package counsel-tramp :ensure t)

(use-package wgrep :ensure t)

(use-package amx
  :ensure t
  :config
  (amx-mode))

(use-package ivy-posframe
  :after ivy
  :disabled t
  :ensure t
  :config
  (defun ivy-posframe-center-dynamic-size (str)
    "Simple wrapper to dynamically set the width and height for the posframe."
    (setq ivy-height (min 100 (length ivy--all-candidates)))
    ;; (setq ivy-posframe-height (max 5 (min ivy-height (floor (* 0.85 (frame-height)))))
    ;;       ivy-posframe-width (floor (/ (frame-width) 1.2)))
    (setq ivy-posframe-width (floor (/ (frame-width) 1.2)))
    (ivy-posframe--display str #'posframe-poshandler-frame-center))
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-center-dynamic-size)))
  (ivy-posframe-mode 1)
  ;; (push
  ;;  `(ivy-posframe-center-dynamic-size
  ;;    :cleanup
  ;;    (lambda () (when
  ;;              (ivy-posframe-workable-p)
  ;;            (posframe-hide ivy-posframe-buffer))))
  ;;  ivy-display-functions-props)
  )

;;; Eshell
(use-package esh-autosuggest
  :ensure t
  :hook (eshell-mode . esh-autosuggest-mode))

;;; Git gutter
(use-package git-gutter
  :ensure t
  :config
  (setq git-gutter:modified-sign "＊"
        git-gutter:added-sign "＋"
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

(use-package lisp-extra-font-lock
  :ensure t
  :config (lisp-extra-font-lock-global-mode 1))

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
(setq evil-want-keybinding nil)
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t
        evil-ex-visual-char-range t
        evil-want-fine-undo t
        evil-respect-visual-line-mode t
        evil-want-C-i-jump nil
        evil-move-beyond-eol t)
  :config
  (evil-mode t))

;; (use-package targets
;;   :config
;;   (targets-define-composite-to anyblock
;;     (("(" ")" pair)
;;      ("[" "]" pair)
;;      ("{" "}" pair)
;;      ("<" ">" pair)
;;      ("\"" "\"" quote)
;;      ("'" "'" quote)
;;      ("`" "`" quote)
;;      ("“" "”" quote))
;;     :bind t
;;     :keys "b")
;;   (targets-define-composite-to pair-delimiter
;;     (("(" ")" pair)
;;      ("[" "]" pair)
;;      ("{" "}" pair)
;;      ("<" ">" pair))
;;     :bind t
;;     :next-key nil
;;     :last-key nil
;;     :keys "d")
;;   (targets-setup t
;;                  :inside-key nil
;;                  :around-key nil
;;                  :remote-key nil))

(use-package evil-expat
  :ensure t
  :after evil)

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

(use-package evil-textobj-anyblock
  :ensure t
  :after evil
  :config
  (define-key evil-inner-text-objects-map "b" 'evil-textobj-anyblock-inner-block)
  (define-key evil-outer-text-objects-map "b" 'evil-textobj-anyblock-a-block))

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
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))

;;; Projectile
(use-package projectile
  :ensure t
  :config
  (projectile-mode t)
  (setq projectile-enable-caching t)
  (setq projectile-git-submodule-command nil)
  (setq-default
   projectile-mode-line
   '(:eval
     (if (file-remote-p default-directory)
         " Pr"
       (format " Pr[%s]" (projectile-project-name)))))
  (push "Cargo.toml" projectile-project-root-files)
  (push "Cargo.toml" projectile-project-root-files-bottom-up)
  (push "meson.build" projectile-project-root-files)
  (push "meson.build" projectile-project-root-files-bottom-up)
  (push "latexmkrc" projectile-project-root-files)
  (push "latexmkrc" projectile-project-root-files-bottom-up))



;; Gitignore
(use-package gitignore-mode :ensure t)
(use-package gitignore-templates :ensure t)

;; Git config
(use-package gitconfig-mode :ensure t)

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
(defvar company-standard-backends '(company-semantic
                                    company-files
                                    (company-dabbrev-code
                                     company-gtags
                                     company-etags
                                     company-keywords)))
(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :config
  (define-key company-active-map [tab] 'company-select-next)
  (define-key company-active-map [S-tab] 'company-select-previous)
  (define-key company-active-map [C-return] #'company-complete-selection)
  (define-key company-active-map [return] #'company-complete-selection)
  (setq company-backends (append '(company-capf) company-standard-backends))
  (setq company-idle-delay 0
        company-echo-delay 0
        company-frontends '(company-posframe-unless-just-one-frontend company-preview-frontend company-echo-metadata-frontend)
        company-minimum-prefix-length 2
        company-require-match nil
        company-dabbrev-downcase nil
        company-tooltip-limit 20
        company-tooltip-align-annotations t
        company-selection-wrap-around t))


;;; NOTE: This package is slow and buggy. Maybe revisit in the future (8/29/2018)
;; (use-package company-box
;;   :disabled t
;;   :ensure t
;;   :hook (company-mode . company-box-mode))

(use-package company-posframe
  :after (company desktop)
  :ensure t
  :config
  (push '(company-posframe-mode . nil) desktop-minor-mode-table)
  (company-posframe-mode 1))

(use-package prescient :ensure t)

(use-package ivy-prescient
  :ensure t
  :after ivy
  :config (ivy-prescient-mode))

(use-package company-prescient
  :ensure t
  :after company
  :config (company-prescient-mode))

;;; All-the-icons
(use-package all-the-icons-ivy
  :ensure t
  :after ivy
  :config (all-the-icons-ivy-setup))

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

;;; LSP
;; Rust, Python, Javascript, Bash, and PHP work out of the box
(use-package eglot
  :disabled t
  :ensure t
  :config
  (add-to-list 'eglot-server-programs '((tuareg-mode caml-mode reason-mode) . ("ocaml-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(lua-mode . ("/home/wil/.luarocks/bin/lua-lsp")))
  (add-hook 'c-mode-common-hook 'eglot-ensure)
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure)
  (add-hook 'tuareg-mode-hook 'eglot-ensure)
  (add-hook 'caml-mode-hook 'eglot-ensure)
  (add-hook 'reason-mode-hook 'eglot-ensure)
  (add-hook 'javascript-mode-hook 'eglot-ensure)
  (add-hook 'rust-mode-hook 'eglot-ensure)
  (add-hook 'lua-mode-hook 'eglot-ensure)
  (add-hook 'python-mode-hook 'eglot-ensure)
  (add-hook 'haskell-mode-hook 'eglot-ensure))

(use-package lsp
  :ensure lsp-mode
  :init
  (setq lsp-prefer-flymake nil)
  :config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "/home/wil/.luarocks/bin/lua-lsp")
                    :major-modes '(lua-mode)
                    :server-id 'lua-lsp))
  (add-hook 'c-mode-common-hook 'lsp)
  (add-hook 'c-mode-hook 'lsp)
  (add-hook 'c++-mode-hook 'lsp)
  (add-hook 'tuareg-mode-hook 'lsp)
  (add-hook 'caml-mode-hook 'lsp)
  (add-hook 'reason-mode-hook 'lsp)
  (add-hook 'javascript-mode-hook 'lsp)
  (add-hook 'rust-mode-hook 'lsp)
  (add-hook 'lua-mode-hook 'lsp)
  (add-hook 'python-mode-hook 'lsp)
  (add-hook 'haskell-mode-hook 'lsp)
  (defun my-set-projectile-root ()
    (when lsp--cur-workspace
      (setq projectile-project-root (lsp--workspace-root lsp--cur-workspace))))
  ;; (add-hook 'lsp-before-open-hook #'my-set-projectile-root)
  (setq lsp-enable-completion-at-point nil))

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode))

(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-error-list)

;;; Snippets
(use-package yasnippet
  :after company
  :ensure t
  :config
  (yas-global-mode t)
  (advice-add 'company-complete-common :before (lambda () (setq my-company-point (point))))
  (advice-add 'company-complete-common :after (lambda () (when (equal my-company-point (point)) (yas-expand)))))

(use-package company-lsp
  :ensure t
  :commands company-lsp
  :init
  (setq company-lsp-async t
        company-lsp-cache-candidates t
        company-lsp-enable-snippet t
        company-lsp-enable-recompletion t)
  :config
  (push 'company-lsp company-backends))

;; (use-package company-quickhelp :ensure t
;;   :disabled t
;;   :config
;;   (company-quickhelp-mode))

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

(defun setup-ocaml-company ()
  "Setup OCaml-specific company backends."
  (set (make-local-variable 'company-backends)
       (append '((company-capf merlin-company-backend)) company-standard-backends)))
(add-hook 'tuareg-mode-hook 'setup-ocaml-company)

;;; Haskell

(use-package haskell-mode :ensure t)

(use-package ghc
  :ensure t
  :hook (haskell-mode . (lambda () (ghc-init))))

(use-package intero
  :ensure t
  :defer t)

(use-package hindent
  :ensure t
  :hook (haskell-mode . hindent-mode))

;; (use-package haskell-snippets :ensure t)

(use-package company-cabal :ensure)

(use-package company-ghci :ensure t)

(use-package company-ghc :ensure t)

(use-package flycheck-haskell
  :ensure t
  :after flycheck
  :hook (flycheck-mode . flycheck-haskell-setup))

(use-package flycheck-ghcmod
  :ensure t
  :after flycheck)

(defun setup-haskell-company ()
  "Setup Haskell-specific company backends."
  (set (make-local-variable 'company-backends)
       (append '((intero-company company-capf company-ghci company-ghc company-cabal))
               company-standard-backends)))
(add-hook 'haskell-mode-hook 'intero-mode)
(add-hook 'haskell-mode-hook 'setup-haskell-company)

;;; Python
(setenv "PYTHONPATH" "/opt/ros/melodic/lib/python3.7/site-packages")

(use-package python
  :ensure t
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))

;; (use-package anaconda-mode
;;   :ensure t
;;   :disabled t
;;   :hook ((python-mode . anaconda-mode)
;;          (python-mode . anaconda-eldoc-mode)))

;; (use-package company-anaconda
;;   :ensure t
;;   :disabled t
;;   :hook (python-mode . (lambda () (push 'company-anaconda company-backends))))

;; (use-package company-jedi
;;   :ensure t
;;   :disabled t
;;   :hook (python-mode . (lambda ()
;;                          (push 'company-jedi company-backends)
;;                          (add-hook 'python-mode-hook 'jedi:setup)))
;;   :config
;;   (setq jedi:complete-on-dot t))

(use-package yapfify
  :ensure t
  :hook (python-mode . yapf-mode))

(use-package ein
  :ensure t
  :init (setq ein:use-auto-complete-superpack t)
  :commands (ein:notebooklist-login))

(use-package py-isort
  :ensure t
  :hook (python-mode . (lambda () (add-hook 'before-save-hook #'py-isort-before-save))))

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

(use-package latex
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :hook
  (LaTeX-mode . (lambda ()
                  (prettify-symbols-mode)
                  (LaTeX-math-mode)
                  ;; (auto-fill-mode -1)
                  ;; (variable-pitch-mode)
                  ;; (visual-line-mode)
                  (setq TeX-command-default "LatexMk")))

  :config
  (add-to-list 'TeX-view-program-list
               '("Zathura"
                 ("zathura %o"
                  (mode-io-correlate " --synctex-forward %n:0:%b -x \"emacsclient --socket-name=%sn --no-wait +%{line} %{input}\""))
                 "zathura"))
  (setq-default TeX-master nil)
  (setq TeX-parse-self t)
  (setq
   TeX-PDF-mode t
   TeX-auto-save t
   TeX-save-query nil
   TeX-electric-sub-and-superscript nil
   ;; TeX-electric-math '("\\(" "\\)")
   ;; TeX-electric-escape t
   TeX-quote-after-quote t
   TeX-clean-confirm nil
   TeX-source-correlate-mode t
   TeX-source-correlate-method 'synctex
   TeX-source-correlate-start-server t
   TeX-engine 'xetex
   TeX-view-program-selection '((output-pdf "Zathura"))))

(use-package auctex-latexmk
  :ensure t
  :after latex
  :config
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  (auctex-latexmk-setup))

(use-package company-auctex
  :ensure t
  :after (latex company)
  :hook (LaTeX-mode . company-auctex-init))

(use-package reftex
  :ensure t
  :after latex
  :hook (LaTeX-mode . turn-on-reftex)
  :config
  (setq reftex-plug-into-AUCTeX t
        reftex-use-fonts t)
  (TeX-add-style-hook
   "cleveref"
   (lambda ()
     (if (boundp 'reftex-ref-style-alist)
         (add-to-list
          'reftex-ref-style-alist
          '("Cleveref" "cleveref"
            (("\\cref" ?c) ("\\Cref" ?C) ("\\cpageref" ?d) ("\\Cpageref" ?D))))
       (reftex-ref-style-activate "Cleveref"))
     (TeX-add-symbols
      '("cref" TeX-arg-ref)
      '("Cref" TeX-arg-ref)
      '("cpageref" TeX-arg-ref)
      '("Cpageref" TeX-arg-ref)))))

(use-package company-reftex :ensure t :after company)

(use-package company-math :ensure t :after company)

(use-package evil-latex-textobjects
  :hook (LaTeX-mode . turn-on-evil-latex-textobjects-mode))

(defun setup-LaTeX-company ()
  "Setup LaTeX-specific company backends."
  (set (make-local-variable 'company-backends)
       (append '(company-reftex-labels
                 company-reftex-citations
                 company-files
                 (company-auctex
                  company-math-symbols-latex
                  company-latex-commands))
               company-standard-backends)))
(add-hook 'LaTeX-mode-hook 'setup-LaTeX-company)
;;; YAML
(use-package yaml-mode :ensure t)

;;; HTML/Web
(use-package mhtml-mode
  :ensure t
  :mode "\\.html\\'")

;; XML
(use-package nxml-mode
  :config
  (setq nxml-sexp-element-flag t))

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

(use-package company-lua :ensure t)

(defun setup-lua-company ()
  "Setup Lua-specific company backends."
  (set (make-local-variable 'company-backends)
       (append '((company-lua company-capf))
               company-standard-backends)))
(add-hook 'lua-mode-hook 'setup-lua-company)

;;; Fish
(use-package fish-mode :ensure t)

;;; Bash
(use-package sh-script :ensure t)

;;; Scheme
(use-package geiser :ensure t)

;;; Scala
(use-package scala-mode :ensure t)

;;; Rust
(use-package rust-mode :ensure t)
(use-package cargo :ensure t)

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

(use-package modern-cpp-font-lock
  :ensure t
  :config
  (modern-c++-font-lock-global-mode))

;; (use-package irony
;;   :ensure t
;;   :disabled t
;;   :hook (((c++-mode c-mode objc-mode) . irony-mode)
;;          (irony-mode . irony-cdb-autosetup-compile-options)))

;; (use-package company-irony
;;   :ensure t
;;   :disabled t
;;   :hook (irony-mode . company-irony-setup-begin-commands)
;;   :config
;;   (setq company-irony-ignore-case 'smart)
;;   (setq-default irony-cdb-compilation-databases '(irony-cdb-libclang irony-cdb-clang-complete)))

(use-package company-c-headers :ensure t)

(use-package flycheck-irony
  :ensure t
  :after flycheck
  :hook (flycheck-mode . flycheck-irony-setup))

(use-package flycheck-clang-analyzer
  :ensure t
  :disabled t
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
  :config
  (setq flycheck-c/c++-clangcheck-executable "/usr/bin/clang-check")
  (setq flycheck-clangcheck-build-path "build")
  :hook (flycheck-mode . (lambda () (add-to-list 'flycheck-checkers 'c/c++-clangcheck))))

;; (use-package irony-eldoc
;;   :ensure t
;;   :disabled t
;;   :hook (irony-mode . irony-eldoc))

(use-package ccls :ensure t)

;; Set a chain of C++ checkers
(add-hook 'c-mode-common-hook (lambda ()
                                (flycheck-add-next-checker 'irony 'c/c++-clang-tidy)
                                (flycheck-add-next-checker 'c/c++-clang-tidy 'c/c++-cppcheck)
                                ;; (flycheck-add-next-checker 'c/c++-clangcheck 'c/c++-cppcheck)
                                ;; Keeps throwing errors right now...
                                ;; (flycheck-add-next-checker 'c/c++-googlelint 'clang-analyzer) ;;
                                ))
(defun setup-c++-company ()
  "Setup C++-specific company backends."
  (set (make-local-variable 'company-backends)
       (append '((company-c-headers company-capf))
               company-standard-backends)))
(add-hook 'c++-mode-hook 'setup-c++-company)
(add-hook 'c-mode-hook 'setup-c++-company)

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
  (setq org-startup-folded t
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
        org-capture-templates '(("j" "Journal Entry" entry (file+olp+datetree "~/wiki/journal.org") "* %(substring (current-time-string) 11 19) %?\n%i" :empty-lines-after 1 :unnarrowed t)
                                ("r" "Research Note" entry (file "phd.org") "** Note: %^{Title?}\n:CREATED: %T\n%?")
                                ("t" "Task Entry" entry (file "tasks.org") "* TODO %?\n:CREATED: %T"))))

(use-package ox-clip
  :ensure t
  :after org)

(use-package mixed-pitch
  :ensure t
  :hook (text-mode . mixed-pitch-mode))

(use-package org-autolist
  :ensure t
  :after org
  :hook (org-mode . org-autolist-mode))

;; (use-package org-variable-pitch
;;   :disabled t
;;   :ensure t
;;   :after org
;;   :hook (org-mode . org-variable-pitch-minor-mode))

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
        org-journal-enable-agenda-integration t
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

;; Theming and Interface

;;; Prettify
(global-prettify-symbols-mode t)

;; Modeline
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config (setq doom-modeline-enable-word-count t
                doom-modeline-lsp t
                doom-modeline-height 3
                doom-modeline-icon t
                doom-modeline-major-mode-icon t
                doom-modeline-major-mode-color-icon t))

;;; Font
;;; Fira code
;; This works when using emacs --daemon + emacsclient
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (set-fontset-font t '(57600 . 57711) "Fira Code Symbol")))
;; This works when using emacs without server/client
(set-fontset-font t '(57600 . 57711) "Fira Code Symbol")
;; I haven't found one statement that makes both of the above situations work, so I use both for now

(use-package fira-code-mode
  :config (add-hook 'prog-mode-hook 'fira-code-mode))

(set-frame-font "Fira Code Retina-11") ;;; set default font
(setq default-frame-alist '((font . "Fira Code Retina-11")))

;;; Theme
(add-to-list 'custom-theme-load-path "~/projects/personal/emacs-nazgul-theme/")
(use-package twilight-bright-theme
  :ensure t
  :disabled)
;; (use-package base16-theme
;;   :ensure t
;;   :config
;;   (load-theme 'base16-gruvbox-dark-soft t))

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
(use-package focus
  :ensure t
  :config
  (setq focus-mode-to-thing '((prog-mode . defun) (text-mode . paragraph))))

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
(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
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
 rust-indent-offset 2
 tab-width 2
 c-basic-offset 2
 cperl-indent-level 2
 fill-column 100
 auto-fill-function 'do-auto-fill
 sentence-end-double-space nil)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

(add-hook 'prog-mode-hook (lambda () (setq-local comment-auto-fill-only-comments t) (auto-fill-mode t)))
(setq xref-prompt-for-identifier '(not xref-find-definitions xref-find-definitions-other-window xref-find-definitions-other-frame xref-find-references))
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
  "gc" #'magit-commit-create
  "gp" #'magit-push-other
  "gl" #'magit-pull-branch

  "ts" #'window-toggle-split-direction

  "c" #'projectile-compile-project
  "lr" #'xref-find-references
  "ln" #'lsp-rename
  "la" #'lsp-execute-code-action
  "li" #'lsp-ui-sideline-toggle-symbols-info
  "lo" #'counsel-imenu
  "SPC" #'execute-extended-command
  "ol" #'org-store-link
  "oi" #'org-insert-link
  "oc" #'org-capture
  "oa" #'org-agenda
  "os" #'org-schedule
  "op" #'org-projectile-project-todo-completing-read
  "on" #'deft)

(define-key evil-normal-state-map (kbd "-") #'ivy-switch-buffer)
(define-key evil-insert-state-map (kbd "C-TAB") #'company-complete)
(add-hook 'server-done-hook 'kill-buffer)

;; Useful functions
(defun toggle-terminal ()
  "Toggle font and company-mode settings for terminal vs GUI use."
  (interactive)
  (if (display-graphic-p)
      (progn
        (fira-code-mode--enable)
        (setq company-frontends '(company-posframe-unless-just-one-frontend company-preview-frontend company-echo-metadata-frontend)))
    (progn
      (fira-code-mode--disable)
      (setq company-frontends '(company-pseudo-tooltip-unless-just-one-frontend company-preview-frontend company-echo-metadata-frontend)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-item-indent 0)
 '(TeX-newline-function (quote newline-and-indent))
 '(async-bytecomp-allowed-packages (quote (all)))
 '(company-reftex-citations-regexp
   "\\\\\\(?:foot\\|auto\\|smart\\|text\\)?cite\\(?:t\\)?[^[{]*\\(?:\\[[^]]*\\]\\)*{\\(?:[^},]*,\\)*\\([^},]*\\)")
 '(company-reftex-labels-regexp "\\\\\\(?:eq\\|auto\\|c\\)?ref{\\([^}]*\\)\\=")
 '(custom-safe-themes
   (quote
    ("099c44618d7660548701d4f495a8c23a85103bc7b87fec33c9db4cd099a4adaf" "9fcac3986e3550baac55dc6175195a4c7537e8aa082043dcbe3f93f548a3a1e0" "e1ad20f721b90cc8e1f57fb8150f81e95deb7ecdec2062939389a4b66584c0cf" "d890583c83cb36550c2afb38b891e41992da3b55fecd92e0bb458fb047d65fb3" "834dd2f8d07bee7897b8119afa1f97aa24f1864ba232eb769c3236ea236ca99a" "f97e1d3abc6303757e38130f4003e9e0d76026fc466d9286d661499158a06d99" "2757944f20f5f3a2961f33220f7328acc94c88ef6964ad4a565edc5034972a53" "9399db70f2d5af9c6e82d4f5879b2354b28bc7b5e00cc8c9d568e5db598255c4" "9b35c097a5025d5da1c97dba45fed027e4fb92faecbd2f89c2a79d2d80975181" "57f95012730e3a03ebddb7f2925861ade87f53d5bbb255398357731a7b1ac0e0" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "075351c6aeaddd2343155cbcd4168da14f54284453b2f1c11d051b2687d6dc48" "e4fe3efbe5098392724aa0be119af539406553f58ef236d1514c8a80ec7ff557" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "242527ce24b140d304381952aa7a081179a9848d734446d913ca8ef0af3cef21" "44247f2a14c661d96d2bff302f1dbf37ebe7616935e4682102b68c0b6cc80095" "c9ddf33b383e74dac7690255dd2c3dfa1961a8e8a1d20e401c6572febef61045" "02956c6f9fc15711d3652ec42ddb43d4ae442da98dba72c7bdd9603525ce82aa" "ef03b74835e14db281cc489faf0d011e1c9255b747ba9c203426c56ed3331197" "058721e6836dfe4d18abbd35820eba7850427f59b9ac7c9c37a5e76f3a405749" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "8f137ccf060af657fbc0c1f7c3d406646ad04ebb8b3e025febc8ef432e958b02" default)))
 '(flycheck-pycheckers-max-line-length 100)
 '(font-latex-fontify-script nil)
 '(font-latex-user-keyword-classes
   (quote
    (("NoteCmds"
      ("wtnote" "wttodo" "rknote")
      font-lock-constant-face command)
     ("RefCmds"
      ("secref" "appref" "tblref" "figref" "listingref" "linref" "defref" "lemref" "thmref" "eqnref" "algoref" "bftt" "kw")
      font-lock-constant-face command))))
 '(ivy-prescient-mode t)
 '(lsp-ui-doc-enable nil)
 '(lsp-ui-doc-include-signature t)
 '(lsp-ui-doc-position (quote top))
 '(lsp-ui-sideline-delay 2.0)
 '(mixed-pitch-fixed-pitch-faces
   (quote
    (diff-added diff-context diff-file-header diff-function diff-header diff-hunk-header diff-removed font-latex-math-face font-latex-sedate-face font-latex-warning-face font-latex-sectioning-5-face font-lock-builtin-face font-lock-comment-delimiter-face font-lock-constant-face font-lock-function-name-face font-lock-keyword-face font-lock-negation-char-face font-lock-preprocessor-face font-lock-regexp-grouping-backslash font-lock-regexp-grouping-construct font-lock-string-face font-lock-type-face font-lock-variable-name-face markdown-code-face markdown-gfm-checkbox-face markdown-inline-code-face markdown-language-info-face markdown-language-keyword-face markdown-math-face message-header-name message-header-to message-header-cc message-header-newsgroups message-header-xheader message-header-subject message-header-other mu4e-header-key-face mu4e-header-value-face mu4e-link-face mu4e-contact-face mu4e-compose-separator-face mu4e-compose-header-face org-block org-block-begin-line org-block-end-line org-document-info-keyword org-code org-latex-and-related org-checkbox org-meta-line org-table org-verbatim)))
 '(org-variable-pitch-fixed-font "Fira Code Retina-11")
 '(package-selected-packages
   (quote
    (ivy-prescient lsp-treemacs ox-clip emacs-counsel-tramp evil-textobj-anyblock base16-theme gitconfig-mode gitignore-templates gitignore-mode white-theme eziam-theme eink-theme twilight-bright-theme flx fira-code-mode prescient lsp-ui kaolin-themes kaolin-theme paradox yasnippet-snippets yapfify yaml-mode which-key wgrep wc-mode utop use-package tuareg toml-mode slime-company scribble-mode scala-mode restart-emacs rainbow-mode rainbow-delimiters racket-mode racer py-isort popup-kill-ring parinfer ox-pandoc org-projectile org-plus-contrib org-noter org-journal org-evil org-bullets org-autolist olivetti ocp-indent no-littering modern-cpp-font-lock mixed-pitch meson-mode merlin markdown-toc lispyville lisp-extra-font-lock linum-relative ivy-xref ivy-rich ivy-posframe irony-eldoc intero ialign hl-todo hindent highlight-parentheses highlight-indent-guides google-c-style golden-ratio git-gutter geiser format-all focus flyspell-correct flycheck-rust flycheck-pycheckers flycheck-pos-tip flycheck-irony flycheck-haskell flycheck-ghcmod flycheck-clangcheck flycheck-clang-analyzer fish-mode eyebrowse evil-visualstar evil-terminal-cursor-changer evil-snipe evil-matchit evil-magit evil-lion evil-leader evil-goggles evil-fringe-mark evil-expat evil-escape evil-embrace evil-commentary evil-collection evil-args esh-autosuggest ein eglot dtrt-indent doom-modeline deft counsel-projectile counsel-etags company-reftex company-quickhelp company-prescient company-posframe company-math company-lua company-lsp company-jedi company-irony company-ghci company-ghc company-cabal company-c-headers company-auctex company-anaconda cmake-font-lock ccls cargo browse-kill-ring biblio auto-dictionary auto-compile auctex-latexmk amx all-the-icons-ivy all-the-icons-dired)))
 '(paradox-execute-asynchronously t)
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
 '(fixed-pitch ((t (:family "Fira Code Retina" :height 110))))
 '(font-latex-bold-face ((t (:inherit bold))))
 '(font-latex-italic-face ((t (:foreground "OliveDrab" :slant italic :height 110 :family "RobotoMono Nerd Font"))))
 '(font-latex-math-face ((t (:foreground "burlywood"))))
 '(font-latex-sectioning-5-face ((t (:inherit bold :weight bold))))
 '(font-latex-sedate-face ((t (:foreground "LightGray"))))
 '(italic ((t (:slant italic))))
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
(provide '\.emacs)
;;; .emacs ends here

;;; Startup speedup
;; From https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/ and https://github.com/hlissner/doom-emacs/wiki/FAQ#how-is-dooms-startup-so-fast
;; -*- lexical-binding: t; -*-
(setq gc-cons-threshold 100000000
      gc-cons-percentage 0.6)
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq load-prefer-newer t)
(run-with-idle-timer
 5 nil
 (lambda ()
   (setq file-name-handler-alist file-name-handler-alist-original)
   (makunbound 'file-name-handler-alist-original)
   (message "file-name-handler-alist restored")))

;;; Local packages
(add-to-list 'load-path "~/.emacs.d/local")

;;; Bootstrap straight.el
(setq
 straight-use-package-by-default t
 straight-check-for-modifications '(check-on-save find-when-checking))
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; Use use-package
(straight-use-package 'use-package)

;;; git credentials
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))

;;; Clean .emacs.d
(use-package no-littering)

;;; Byte-compile
(use-package auto-compile
  :config
  (setq auto-compile-display-buffer nil
        auto-compile-mode-line-counter t)
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

;;; Evil!
(use-package evil
  :init (setq
         evil-want-keybinding nil
         evil-want-integration t
         evil-ex-visual-char-range t
         evil-want-fine-undo t
         evil-respect-visual-line-mode t
         evil-move-beyond-eol t)
  :config (evil-mode t))
(use-package evil-expat :after evil)
(use-package evil-collection
  :after evil
  :init (setq evil-collection-company-use-tng t
              evil-collection-outline-bind-tab-p t
              evil-collection-setup-minibuffer nil)
  :config (evil-collection-init))
(use-package evil-surround
  :after evil
  :config (global-evil-surround-mode t))
(use-package evil-matchit
  :after evil
  :config (global-evil-matchit-mode t))
(use-package evil-fringe-mark
  :after evil
  :config (global-evil-fringe-mark-mode))
(use-package evil-args
  :after evil
  :bind (:map evil-inner-text-objects-map
              ("a" . evil-inner-arg)
              :map evil-outer-text-objects-map
              ("a" . evil-outer-arg)))
(use-package evil-visualstar
  :after evil
  :config (global-evil-visualstar-mode t))
(use-package evil-embrace
  :after evil
  :config (evil-embrace-enable-evil-surround-integration))
(use-package evil-escape
  :after evil
  :config (evil-escape-mode)
  (setq-default evil-escape-key-sequence "zx"
                evil-escape-delay 0.2)
  (setq evil-escape-unordered-key-sequence t))
(use-package evil-magit :after (evil magit))
(use-package evil-commentary
  :after evil
  :config (evil-commentary-mode))
(use-package evil-snipe
  :after evil
  :init (setq evil-snipe-scope 'buffer
              evil-snipe-repeat-scope 'whole-buffer)
  :config (evil-snipe-mode t)
  (evil-snipe-override-mode t)
  (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode))
(use-package evil-lion
  :after evil
  :config (evil-lion-mode))
(use-package evil-goggles
  :after evil
  :config (setq evil-goggles-duration 0.05)
  (evil-goggles-mode)
  (evil-goggles-use-diff-refine-faces))
(use-package evil-textobj-anyblock
  :after evil
  :bind (:map evil-inner-text-objects-map
              ("b" . evil-textobj-anyblock-inner-block)
              :map evil-outer-text-objects-map
              ("b" . evil-textobj-anyblock-a-block)))

;;; Ivy
(use-package ivy
  :bind (:map ivy-minibuffer-map
              ([escape] . 'minibuffer-keyboard-quit)
              ("C-a" . ivy-read-action)
              ("C-f" . ivy-toggle-fuzzy))
  :config (setq ivy-use-virtual-buffers t
                enable-recursive-minibuffers t
                ivy-display-style 'fancy
                ivy-count-format "(%d/%d) "
                ivy-height 50
                ivy-use-selectable-prompt t)
  (ivy-mode 1))
(use-package ivy-posframe
  :config (setq ivy-posframe-display-functions-alist
                '((swiper . ivy-posframe-display-at-point)
                  (complete-symbol . ivy-posframe-display-at-point)
                  (t . ivy-posframe-display-at-frame-center)))
  (ivy-posframe-mode 1))
(use-package fuz
  :config (unless (require 'fuz-core nil t)
            (fuz-build-and-load-dymod)))
(use-package swiper)
(use-package ivy-hydra)
(use-package ivy-rich
  :config (setq ivy-virtual-abbreviate 'full
                ivy-rich-path-style 'abbrev)
  (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer))
(use-package counsel
  :config (counsel-mode))
(use-package counsel-projectile)
(use-package counsel-tramp)
(use-package ivy-prescient
  :after ivy
  :config (ivy-prescient-mode))
(use-package ivy-fuz
  :demand t
  :after ivy
  :config (setq ivy-sort-matches-functions-alist '((t . ivy-fuz-sort-fn))
                ivy-re-builders-alist '((t . ivy-fuz-regex-fuzzy)))
  (add-to-list 'ivy-highlight-functions-alist '(ivy-fuz-regex-fuzzy . ivy-fuz-highlight-fn)))

;;; wgrep
(use-package wgrep)

;;; amx
(use-package amx :config (amx-mode))

;;; Eshell
(use-package esh-autosuggest :hook (eshell-mode . esh-autosuggest-mode))

;;; Git gutter
(use-package diff-hl :config (global-diff-hl-mode))
(use-package git-gutter
  :disabled t
  :config (setq git-gutter:modified-sign "＊"
                git-gutter:added-sign "＋"
                git-gutter:deleted-sign "～"
                git-gutter:window-width 2
                git-gutter:handled-backends '(git hg bzr svn))
  (set-face-foreground 'git-gutter:modified "#8ec07c") ;; background color
  (set-face-foreground 'git-gutter:added "#b8bb26")
  (set-face-foreground 'git-gutter:deleted "cc241d")
  (global-git-gutter-mode))

;;; Eyebrowse
(use-package eyebrowse
  :after evil
  :config (eyebrowse-mode t)
  (eyebrowse-setup-opinionated-keys))

;;; Rainbow mode
(use-package rainbow-mode
  :config (rainbow-mode))

;;; Interactive align
(use-package ialign)

;;; Lispy
(use-package lispy :hook ((emacs-lisp-mode clojure-mode common-lisp-mode scheme-mode lisp-mode racket-mode) . lispy-mode))
(use-package lisp-extra-font-lock :config (lisp-extra-font-lock-global-mode 1))

;;; Lispyville
(use-package lispyville
  :hook (lispy-mode . lispyville-mode)
  :config (lispyville-set-key-theme '(operators
                                      prettify
                                      text-objects
                                      atom-movement
                                      additional-movement
                                      slurp/barf-cp
                                      additional
                                      escape)))

;;; Parinfer
(use-package parinfer
  :hook ((clojure-mode emacs-lisp-mode common-lisp-mode scheme-mode racket-mode lisp-mode) . parinfer-mode)
  :config (progn
            (setq parinfer-extensions
                  '(defaults            ; should be included.
                     evil               ; If you use Evil.
                     smart-tab          ; C-b & C-f jump positions and smart shift with tab & S-tab.
                     smart-yank))       ; Yank behavior depend on mode.
            (setq parinfer-auto-switch-indent-mode t)))

;;; Undo-Tree
(use-package undo-tree :config (global-undo-tree-mode))

;;; Spell checking
(use-package flyspell
  :hook ((org-mode LaTeX-mode markdown-mode) . flyspell-mode)
  :config (setq flyspell-issue-message-flag nil))

(use-package flyspell-correct
  :commands (flyspell-correct-word-generic
             flyspell-correct-previous-word-generic))

(use-package auto-dictionary :hook (flyspell-mode . auto-dictionary-mode))

;;; Restart Emacs
(use-package restart-emacs)

;;; Indentation guides
(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :config (setq highlight-indent-guides-method 'character))

;;; Projectile
(use-package projectile
  :config (projectile-mode t)
  (setq projectile-enable-caching t
        projectile-git-submodule-command nil)
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
(use-package gitignore-mode)

;; Git config
(use-package gitconfig-mode)

;;; Magit
(use-package magit
  :defer 5
  :bind (:map magit-mode-map
              ([?\t] . magit-section-toggle)))

;;; which-key
(use-package which-key
  :config (which-key-mode)
  (which-key-setup-side-window-bottom)
  (setq which-key-sort-order 'which-key-key-order-alpha
        which-key-popup-type 'side-window
        which-key-side-window-max-width 0.33
        which-key-idle-delay 0.5))

;;; Company
(defvar company-standard-backends '(company-lsp
                                    company-semantic
                                    company-files
                                    company-keywords
                                    company-capf))
(use-package company
  :hook (after-init . global-company-mode)
  :config (setq company-backends company-standard-backends
                company-idle-delay 0
                company-echo-delay 0
                company-minimum-prefix-length 2
                company-require-match nil
                company-dabbrev-downcase nil
                company-tooltip-limit 20
                company-tooltip-align-annotations t
                company-selection-wrap-around t))
(use-package company-box
  :hook (company-mode . company-box-mode))
(use-package company-posframe
  :disabled t
  :after (company desktop)
  :config (company-posframe-mode 1))

;;; Prescient
(use-package prescient :config (prescient-persist-mode))
(use-package company-prescient
  :after company
  :config (company-prescient-mode))

;;; All-the-icons
(use-package all-the-icons-ivy
  :after ivy
  :config (all-the-icons-ivy-setup))
(use-package all-the-icons-dired :hook (dired-mode . all-the-icons-dired-mode))

;;; LSP
(use-package lsp-mode
  :init (setq read-process-output-max (* 1024 1024))
  :config (lsp-register-client
           (make-lsp-client :new-connection (lsp-stdio-connection "lua-language-server")
                            :major-modes '(lua-mode)
                            :priority -1
                            :server-id 'lua-language-server))
  :commands (lsp lsp-deferred)
  :hook (c-mode-common . lsp-deferred)
  (c-mode . lsp-deferred)
  (c++-mode . lsp-deferred)
  (tuareg-mode . lsp-deferred)
  (caml-mode . lsp-deferred)
  (css-mode . lsp-deferred)
  (mhtml-mode . lsp-deferred)
  (reason-mode . lsp-deferred)
  (javascript-mode . lsp-deferred)
  (rust-mode . lsp-deferred)
  (lua-mode . lsp-deferred)
  (julia-mode . lsp-deferred)
  (haskell-mode . lsp-deferred)
  (tex-mode . lsp-deferred)
  (latex-mode . lsp-deferred))

(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-treemacs :commands lsp-treemacs-error-list)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-python-ms
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp-deferred))))
(use-package lsp-latex
  :straight (:host github :repo "ROCKTAKEY/lsp-latex"))
(use-package lsp-julia)
(use-package company-lsp
  :commands company-lsp
  :init (setq company-lsp-async t
              company-lsp-cache-candidates 'auto
              company-lsp-enable-snippet t
              company-lsp-enable-recompletion t))

;;; DAP
(use-package dap-mode
  :config (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1))
(require 'dap-python)
(require 'dap-gdb-lldb)

;;; Julia
(use-package julia-mode)

;;; Common Lisp
(use-package slime-company :hook slime-mode)
(use-package slime
  :hook (common-lisp-mode lisp-mode)
  :init (setq inferior-lisp-program "/usr/bin/sbcl")
  (setq slime-contribs '(slime-fancy slime-company slime-asdf))
  :config (add-hook 'slime-mode-hook (lambda () (unless (slime-connected-p) (save-excursion (slime))))))

;;; OCaml
(eval-and-compile
  (defun opam-share ()
    (substring
     (shell-command-to-string "opam config var share 2> /dev/null")
     0 -1)))
(use-package ocp-indent
  :hook (tuareg-mode . (lambda () (setq indent-line-function 'ocp-indent-line)))
  :load-path (lambda () (concat (opam-share) "/emacs/site-lisp")))
(use-package tuareg)
(use-package merlin
  :hook (tuareg-mode . merlin-mode)
  :config (setq merlin-completion-with-doc t))
(use-package utop
  :hook (tuareg-mode . utop-minor-mode)
  :config (setq utop-command "opam config exec -- utop -emacs"))

;;; Python
(setenv "PYTHONPATH" "/opt/ros/melodic/lib/python3.7/site-packages")
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))
(use-package py-isort :hook (python-mode . (lambda () (add-hook 'before-save-hook #'py-isort-before-save))))

;;; Markdown
(use-package markdown-mode)

;;; LaTeX
;; For biber
(setenv "PATH" (concat (getenv "PATH") ":/usr/bin/vendor_perl/"))
(add-to-list 'exec-path "/usr/bin/vendor_perl/")
(use-package latex
  :straight auctex
  :mode ("\\.tex\\'" . latex-mode)
  :hook (LaTeX-mode . (lambda ()
                        (prettify-symbols-mode)
                        (LaTeX-math-mode)
                        ;; (auto-fill-mode -1)
                        ;; (variable-pitch-mode)
                        ;; (visual-line-mode)
                        (setq TeX-command-default "LatexMk")))
  :config (add-to-list 'TeX-view-program-list
                       '("Zathura"
                         ("zathura %o"
                          (mode-io-correlate " --synctex-forward %n:0:%b -x \"emacsclient --socket-name=%sn --no-wait +%{line} %{input}\""))
                         "zathura"))
  (setq-default TeX-master nil)
  (setq
   TeX-parse-self t
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
   TeX-engine 'pdflatex
   TeX-view-program-selection '((output-pdf "Zathura"))))

(use-package auctex-latexmk
  :after latex
  :config (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  (auctex-latexmk-setup))

(use-package evil-latex-textobjects
  :straight (evil-latex-textobjects :host github :repo "hpdeifel/evil-latex-textobjects")
  :hook (LaTeX-mode . turn-on-evil-latex-textobjects-mode))

;;; YAML
(use-package yaml-mode)

;;; HTML/Web
(use-package mhtml-mode :mode "\\.html\\'")

;; XML
(use-package nxml-mode
  :straight nil
  :config (setq nxml-sexp-element-flag t))

;;; CMake
(use-package cmake-mode)
(use-package cmake-font-lock
  :hook (cmake-mode . cmake-font-lock-activate)
  :config (autoload 'cmake-font-lock-activate "cmake-font-lock" nil t))

;;; ROS
(projectile-register-project-type 'ros-workspace '(".catkin_tools" ".catkin_workspace")
                                  :compile "catkin build")
(projectile-register-project-type 'ros-package '("package.xml")
                                  :compile "catkin build")

;;; PDDL
(use-package pddl-mode)
(add-to-list 'auto-mode-alist '("\\.pddl\\'" . PDDL-mode))

;;; Meson
(use-package meson-mode)

;;; TOML
(use-package toml-mode)

;;; Lua
(use-package lua-mode)

;;; Bash
(use-package sh-script)

;;; Scheme
(use-package geiser)

;;; Rust
(use-package rust-mode)
(use-package cargo)

;; Racket
(use-package racket-mode :mode "\\.rkt\\'")
(use-package scribble-mode)

;; C++
(add-hook 'c++-mode-hook #'electric-pair-mode)
(use-package cc-mode)
(use-package modern-cpp-font-lock :config (modern-c++-font-lock-global-mode))

;;; Deft
(use-package deft
  :commands (deft)
  :init (setq deft-directory "~/wiki/notes"
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
(use-package org :straight org-plus-contrib
  :hook (org-mode . (lambda ()
                      (linum-relative-mode -1)
                      (auto-fill-mode -1)
                      (set (make-local-variable 'company-backends) '(company-capf company-math-symbols-latex company-files))))
  (org-mode . visual-line-mode)
  :config (font-lock-add-keywords 'org-mode
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
        org-blank-before-new-entry '((heading) (plain-list-item))
        org-fontify-quote-and-verse-blocks t
        org-todo-keywords '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)"))
        org-pretty-entities-include-sub-superscripts t
        org-capture-templates '(("j" "Journal Entry" entry (file+olp+datetree "~/wiki/journal.org") "* %(substring (current-time-string) 11 19) %?\n%i" :empty-lines-after 1 :unnarrowed t)
                                ("r" "Research Note" entry (file "phd.org") "** Note: %^{Title?}\n:CREATED: %T\n%?")
                                ("t" "Task Entry" entry (file "tasks.org") "* TODO %?\n:CREATED: %T"))))
(use-package ox-clip :after org)
(use-package mixed-pitch :hook (text-mode . mixed-pitch-mode))
(use-package org-autolist
  :after org
  :hook (org-mode . org-autolist-mode))
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))
(use-package org-evil :after (evil org))
(use-package org-noter :after org)
(use-package org-journal
  :after org
  :config (setq org-journal-dir "~/wiki/journal/"
                org-journal-enable-agenda-integration t
                org-journal-find-file #'find-file))
(use-package wc-mode :hook org-mode)
(use-package org-projectile
  :after (org projectile)
  :config (org-projectile-per-project)
  (setq org-projectile-per-project-filepath "TODO.org"))

;;; Prettify
(global-prettify-symbols-mode t)

;; Modeline
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config (setq doom-modeline-enable-word-count t
                doom-modeline-lsp t
                doom-modeline-height 3
                doom-modeline-icon t
                doom-modeline-major-mode-icon t
                doom-modeline-major-mode-color-icon t))

;;; Font
;; Fira code
;; This works when using emacs --daemon + emacsclient
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (set-fontset-font t '(57600 . 57711) "Fira Code Symbol")))
;; This works when using emacs without server/client
(set-fontset-font t '(57600 . 57711) "Fira Code Symbol")
;; I haven't found one statement that makes both of the above situations work, so I use both for now

(use-package fira-code-mode
  :straight nil
  :config (add-hook 'prog-mode-hook 'fira-code-mode))
(set-frame-font "Fira Code Retina-11") ;;; set default font
(setq default-frame-alist '((font . "Fira Code Retina-11")))

;;; Theme
(use-package base16-theme :config (load-theme 'base16-gruvbox-dark-hard t))

;;; Relative linum
(use-package linum-relative
  :config (setq linum-relative-format "%4s ")
  (setq linum-relative-current-symbol "")
  (setq linum-relative-backend 'display-line-numbers-mode))

;;; TODO Highlight
(use-package hl-todo
  :config (global-hl-todo-mode))

;;; Formatters
(use-package format-all)

;;; Highlight parens
(use-package highlight-parentheses
  :config (global-highlight-parentheses-mode t))

;;; Focus
(use-package focus
  :config (setq focus-mode-to-thing '((prog-mode . defun) (text-mode . paragraph))))

;;; Browse kill ring
(use-package browse-kill-ring)

;;; Keybindings
(use-package general
  :after evil
  :config
  (general-evil-setup t)
  (nmap
    "-" 'ivy-switch-buffer
    "+" 'counsel-git
    "_" 'counsel-find-file
    "C-TAB" 'company-complete)
  (nmap :prefix "SPC"
    "q" 'kill-emacs
    "w" 'save-buffer
    "x" 'save-buffers-kill-terminal
    "d" (lambda () (interactive) (kill-buffer nil))
    "gs" 'magit-status
    "gp" 'magit-push-other
    "gl" 'magit-pull-branch
    "SPC" 'counsel-M-x
    "f" 'format-all-buffer
    "h" 'counsel-apropos
    "p" 'counsel-projectile-switch-project
    "ln" 'lsp-rename
    "lf" 'lsp-format-buffer))

;;; Interface Settings
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(menu-bar-mode -1)
(savehist-mode)
(show-paren-mode)
(column-number-mode)
(linum-relative-global-mode)
(setq inhibit-startup-screen t)

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
 rust-indent-offset 2
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
  :init (dtrt-indent-mode))

;;; doc-view mode
(setq doc-view-continuous t)

;;; Evil settings
(setq evil-insert-state-cursor '("#268bd2" bar) ;; blue
      evil-normal-state-cursor '("#b58900" box) ;; blue
      evil-visual-state-cursor '("#cb4b16" box) ;; orange
      evil-replace-state-cursor '("#859900" hbar) ;; green
      evil-emacs-state-cursor '("#d33682" box)) ;; magenta

;;; happy-hacking-theme.el --- A version of happy-hacking by yorickpeterse

;; Author: Wil Thomason <wbthomason@cs.cornell.edu>
;; Keywords: themes
;; Version: {{VERSION}}

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

;;
;;; Credit:

;; Colour selection by yorickpeterse:
;; https://gitlab.com/yorickpeterse/happy_hacking.vim

;; Adapted from the sourcerer theme by gilbertw1
;; https://github.com/gilbertw1/sourcerer-emacs

;;; Code:

(deftheme happy-hacking)

(let ((class '((class color) (min-colors 89)))
      (background   "#222222")
      (current-line "#2b2b2b")
      (highlight    "#777777")
      (selection    "#aaaaaa")
      (contrast-bg  "#aaaaaa")
      (foreground   "#c2c2b0")
      (comment      "#5c5d56")
      (red          "#f05e48")
      (orange       "#faa166")
      (yellow       "#fad566")
      (green        "#8daf67")
      (brightgreen  "#537D01")
      (aqua         "#b3ebbf")
      (blue         "#81a2c7")
      (offwhite     "#f3f2cc")
      (purple       "#f77ebd")
      (gray         "#aaaaaa"))

  (custom-theme-set-faces
    'happy-hacking
    `(default ((,class (:foreground ,offwhite :background ,background))))
    `(bold ((,class (:weight bold))))
    `(bold-italic ((,class (:slant italic :weight bold))))
    `(underline ((,class (:underline t))))
    `(italic ((,class (:slant italic))))
    `(font-lock-builtin-face ((,class (:foreground ,orange :slant italic))))
    `(font-lock-comment-delimiter-face ((,class (:foreground ,comment :slant italic))))
    `(font-lock-comment-face ((,class (:foreground ,comment :slant italic))))
    `(font-lock-doc-face ((,class (:foreground ,green))))
    `(font-lock-doc-string-face ((,class (:foreground ,yellow))))
    `(font-lock-function-name-face ((,class (:foreground ,yellow))))
    `(font-lock-keyword-face ((,class (:foreground ,red))))
    `(font-lock-negation-char-face ((,class (:foreground "#afafaf"))))
    `(font-lock-reference-face ((,class (:foreground ,foreground))))
    `(font-lock-constant-face ((,class (:foreground ,aqua))))
    `(font-lock-preprocessor-face ((,class (:foreground ,brightgreen))))
    `(font-lock-regexp-grouping-backslash ((,class (:foreground ,yellow))))
    `(font-lock-regexp-grouping-construct ((,class (:foreground ,purple))))
    `(font-lock-string-face ((,class (:foreground ,green))))
    `(font-lock-type-face ((,class (:foreground ,blue))))
    `(font-lock-variable-name-face ((,class (:foreground ,foreground))))
    `(font-lock-warning-face ((,class (:weight bold :foreground ,orange))))
    `(shadow ((,class (:foreground ,comment))))
    `(success ((,class (:foreground ,brightgreen))))
    `(error ((,class (:foreground ,red :weight bold))))
    `(warning ((,class (:foreground ,orange))))

      ;; Powerline
    `(powerline-active1 ((,class (:foreground ,foreground :background ,highlight))))
    `(powerline-active2 ((,class (:foreground ,foreground :background ,contrast-bg))))

      ;; Powerline-evil
    `(powerline-evil-base-face ((,class (:inherit mode-line :foreground ,background))))
    `(powerline-evil-emacs-face ((,class (:inherit powerline-evil-base-face :background ,purple))))
    `(powerline-evil-insert-face ((,class (:inherit powerline-evil-base-face :background ,blue))))
    `(powerline-evil-motion-face ((,class (:inherit powerline-evil-base-face :background ,orange))))
    `(powerline-evil-normal-face ((,class (:inherit powerline-evil-base-face :background ,aqua))))
    `(powerline-evil-operator-face ((,class (:inherit powerline-evil-base-face :background ,brightgreen))))
    `(powerline-evil-replace-face ((,class (:inherit powerline-evil-base-face :background ,red))))
    `(powerline-evil-visual-face ((,class (:inherit powerline-evil-base-face :background ,yellow))))

    ;; spaceline
    `(spaceline-evil-emacs ((,class (:foreground ,background :background ,blue))))
    `(spaceline-evil-insert ((,class (:foreground ,background :background "#ff9800"))))
    `(spaceline-evil-motion ((,class (:foreground ,background :background ,purple))))
    `(spaceline-evil-normal ((,class (:foreground ,background :background ,aqua))))
    `(spaceline-evil-operator ((,class (:foreground ,background :background ,red))))
    `(spaceline-evil-replace ((,class (:foreground ,background :background ,red))))
    `(spaceline-evil-visual ((,class (:foreground ,background :background ,offwhite))))

    ;; web mode
    `(web-mode-variable-name-face ((,class (:foreground ,purple))))
    `(web-mode-css-property-name-face ((,class (:foreground ,purple))))
    `(web-mode-function-name-face ((,class (:foreground ,blue))))
    `(web-mode-filter-face ((,class (:foreground ,blue))))
    `(web-mode-filter-face ((,class (:foreground ,blue))))
    `(web-mode-html-attr-name-face ((,class (:foreground ,purple))))
    `(web-mode-html-tag-face ((,class (:foreground ,blue))))

    ;; Flycheck
    `(flycheck-error ((,class (:underline (:style wave :color ,red)))))
    `(flycheck-info ((,class (:underline (:style wave :color ,aqua)))))
    `(flycheck-warning ((,class (:underline (:style wave :color ,orange)))))
    `(flycheck-fringe-error ((,class (:foreground ,red))))
    `(flycheck-fringe-info ((,class (:foreground ,aqua))))
    `(flycheck-fringe-warning ((,class (:foreground ,orange))))

    ;; Flymake
    `(flymake-warnline ((,class (:underline (:style wave :color ,orange) :background ,background))))
    `(flymake-errline ((,class (:underline (:style wave :color ,red) :background ,background))))

    ;; Flyspell
    `(flyspell-incorrect ((,class (:underline (:style wave :color ,red)))))

    ;; Clojure errors
    `(clojure-test-failure-face ((,class (:background nil :inherit flymake-warnline))))
    `(clojure-test-error-face ((,class (:background nil :inherit flymake-errline))))
    `(clojure-test-success-face ((,class (:background nil :foreground nil :underline ,green))))

    ;; EDTS errors
    `(edts-face-warning-line ((,class (:background nil :inherit flymake-warnline))))
    `(edts-face-warning-mode-line ((,class (:background nil :foreground ,orange :weight bold))))
    `(edts-face-error-line ((,class (:background nil :inherit flymake-errline))))
    `(edts-face-error-mode-line ((,class (:background nil :foreground ,red :weight bold))))

    ;; For Brian Carper's extended clojure syntax table
    `(clojure-keyword ((,class (:foreground ,yellow))))
    `(clojure-parens ((,class (:foreground ,foreground))))
    `(clojure-braces ((,class (:foreground ,green))))
    `(clojure-brackets ((,class (:foreground ,yellow))))
    `(clojure-double-quote ((,class (:foreground ,aqua :background nil))))
    `(clojure-special ((,class (:foreground ,blue))))
    `(clojure-java-call ((,class (:foreground ,purple))))

    ;; Rainbow-delimiters
    `(rainbow-delimiters-depth-1-face ((,class (:foreground ,foreground))))
    `(rainbow-delimiters-depth-2-face ((,class (:foreground ,aqua))))
    `(rainbow-delimiters-depth-3-face ((,class (:foreground ,yellow))))
    `(rainbow-delimiters-depth-4-face ((,class (:foreground ,brightgreen))))
    `(rainbow-delimiters-depth-5-face ((,class (:foreground ,blue))))
    `(rainbow-delimiters-depth-6-face ((,class (:foreground ,foreground))))
    `(rainbow-delimiters-depth-7-face ((,class (:foreground ,aqua))))
    `(rainbow-delimiters-depth-8-face ((,class (:foreground ,yellow))))
    `(rainbow-delimiters-depth-9-face ((,class (:foreground ,green))))
    `(rainbow-delimiters-unmatched-face ((,class (:foreground ,red))))

    ;; MMM-mode
    `(mmm-code-submode-face ((,class (:background ,contrast-bg))))
    `(mmm-comment-submode-face ((,class (:inherit font-lock-comment-face))))
    `(mmm-output-submode-face ((,class (:background ,contrast-bg))))

    ;; Search
    `(match ((,class (:foreground ,gray :background ,background :inverse-video t))))
    `(isearch ((,class (:foreground ,yellow :background ,background :inverse-video t))))
    `(isearch-lazy-highlight-face ((,class (:foreground ,gray :background ,background :inverse-video t))))
    `(isearch-fail ((,class (:background ,background :inherit font-lock-warning-face :inverse-video t))))

    ;; Anzu
    `(anzu-mode-line ((,class (:foreground ,orange))))
    `(anzu-replace-highlight ((,class (:inherit isearch-lazy-highlight-face))))
    `(anzu-replace-to ((,class (:inherit isearch))))

    ;; IDO
    `(ido-subdir ((,class (:foreground ,purple))))
    `(ido-first-match ((,class (:foreground ,orange))))
    `(ido-only-match ((,class (:foreground ,green))))
    `(ido-indicator ((,class (:foreground ,red :background ,background))))
    `(ido-virtual ((,class (:foreground ,comment))))

    ;; flx-ido
    `(flx-highlight-face ((,class (:inherit nil :foreground ,yellow :weight bold :underline nil))))

    ;; Ivy
    `(ivy-confirm-face ((,class (:foreground ,green))))
    `(ivy-current-match ((,class (:background ,contrast-bg))))
    `(ivy-match-required-face ((,class (:foreground ,red))))
    `(ivy-remote ((,class (:foreground ,blue))))
    `(ivy-subdir ((,class (:foreground ,orange))))
    `(ivy-virtual ((,class (:foreground ,purple))))

    ;; which-function
    `(which-func ((,class (:foreground ,blue :background nil :weight bold))))

    ;; Emacs interface
    `(cursor ((,class (:background "#626262"))))
    `(fringe ((,class (:background "#3A3A3A" :foreground "#878787"))))
    `(linum ((,class (:background "#3A3A3A" :foreground "#878787" :italic nil :underline nil))))
    `(vertical-border ((,class (:foreground ,contrast-bg))))
    `(border ((,class (:background ,contrast-bg :foreground ,highlight))))
    `(border-glyph (nil))
    `(highlight ((,class (:inverse-video nil :background ,highlight))))
    `(gui-element ((,class (:background ,contrast-bg))))
    `(mode-line ((,class (:background ,contrast-bg :weight normal))))
    `(mode-line-buffer-id ((,class (:foreground ,purple :background nil))))
    `(mode-line-inactive ((,class (:inherit mode-line
                                    :foreground ,comment
                                    :background ,highlight :weight normal))))
    `(mode-line-emphasis ((,class (:foreground ,foreground :slant italic))))
    `(mode-line-highlight ((,class (:foreground ,purple :box nil :weight bold))))
    `(minibuffer-prompt ((,class (:foreground ,blue))))
    `(region ((,class (:background ,selection :inverse-video nil))))
    `(secondary-selection ((,class (:background ,highlight))))

    `(header-line ((,class (:inherit mode-line-inactive :foreground ,aqua :background nil))))

    `(trailing-whitespace ((,class (:background ,orange :foreground ,yellow))))
    `(whitespace-empty ((,class (:foreground ,orange :background ,yellow))))
    `(whitespace-hspace ((,class (:background ,contrast-bg))))
    `(whitespace-indentation ((,class (:background ,contrast-bg))))
    `(whitespace-line ((,class (:background ,contrast-bg))))
    `(whitespace-newline ((,class (:background ,contrast-bg))))
    `(whitespace-space ((,class (:background ,contrast-bg))))
    `(whitespace-space-after-tab ((,class (:background ,contrast-bg))))
    `(whitespace-space-before-tab ((,class (:background ,contrast-bg))))
    `(whitespace-tab ((,class (:background ,contrast-bg))))
    `(whitespace-trailing ((,class (:background ,contrast-bg))))

    ;; Parenthesis matching (built-in)))
    `(show-paren-match ((,class (:background ,gray :foreground ,background))))
    `(show-paren-mismatch ((,class (:background ,red :foreground ,background))))

    ;; Smartparens paren matching
    `(sp-show-pair-match-face ((,class (:foreground nil :background nil :inherit show-paren-match))))
    `(sp-show-pair-mismatch-face ((,class (:foreground nil :background nil :inherit show-paren-mismatch))))

    ;; Parenthesis matching (mic-paren)))
    `(paren-face-match ((,class (:foreground nil :background nil :inherit show-paren-match))))
    `(paren-face-mismatch ((,class (:foreground nil :background nil :inherit show-paren-mismatch))))
    `(paren-face-no-match ((,class (:foreground nil :background nil :inherit show-paren-mismatch))))

    ;; Parenthesis dimming (parenface)))
    `(paren-face ((,class (:foreground ,comment :background nil))))

    `(sh-heredoc ((,class (:foreground nil :inherit font-lock-string-face :weight normal))))
    `(sh-quoted-exec ((,class (:foreground nil :inherit font-lock-preprocessor-face))))
    `(slime-highlight-edits-face ((,class (:weight bold))))
    `(slime-repl-input-face ((,class (:weight normal :underline nil))))
    `(slime-repl-prompt-face ((,class (:underline nil :weight bold :foreground ,purple))))
    `(slime-repl-result-face ((,class (:foreground ,green))))
    `(slime-repl-output-face ((,class (:foreground ,blue :background ,background))))
    `(slime-repl-inputed-output-face ((,class (:foreground ,comment))))

    `(csv-separator-face ((,class (:foreground ,orange))))

    `(diff-added ((,class (:foreground ,green))))
    `(diff-changed ((,class (:foreground ,purple))))
    `(diff-removed ((,class (:foreground ,orange))))
    `(diff-header ((,class (:foreground ,aqua :background nil))))
    `(diff-file-header ((,class (:foreground ,blue :background nil))))
    `(diff-hunk-header ((,class (:foreground ,purple))))
    `(diff-refine-added ((,class (:inherit diff-added :inverse-video t))))
    `(diff-refine-removed ((,class (:inherit diff-removed :inverse-video t))))

    `(diff-hl-insert ((,class (:background ,green))))
    `(diff-hl-change ((,class (:background ,blue))))
    `(diff-hl-delete ((,class (:background ,orange))))
    `(diff-hl-unknown ((,class (:background ,purple))))

    `(ediff-even-diff-A ((,class (:foreground nil :background nil :inverse-video t))))
    `(ediff-even-diff-B ((,class (:foreground nil :background nil :inverse-video t))))
    `(ediff-odd-diff-A  ((,class (:foreground ,comment :background nil :inverse-video t))))
    `(ediff-odd-diff-B  ((,class (:foreground ,comment :background nil :inverse-video t))))

    `(eldoc-highlight-function-argument ((,class (:foreground ,green :weight bold))))

    ;; macrostep
    `(macrostep-expansion-highlight-face ((,class (:inherit highlight :foreground nil))))

    ;; undo-tree
    `(undo-tree-visualizer-default-face ((,class (:foreground ,foreground))))
    `(undo-tree-visualizer-current-face ((,class (:foreground ,green :weight bold))))
    `(undo-tree-visualizer-active-branch-face ((,class (:foreground ,red))))
    `(undo-tree-visualizer-register-face ((,class (:foreground ,yellow))))

    ;; dired+
    `(diredp-compressed-file-suffix ((,class (:foreground ,blue))))
    `(diredp-deletion ((,class (:inherit error :inverse-video t))))
    `(diredp-deletion-file-name ((,class (:inherit error))))
    `(diredp-date-time ((,class (:foreground ,blue))))
    `(diredp-dir-heading ((,class (:foreground ,green :weight bold))))
    `(diredp-dir-name ((,class (:foreground ,aqua))))
    `(diredp-dir-priv ((,class (:foreground ,aqua :background nil))))
    `(diredp-exec-priv ((,class (:foreground ,orange :background nil))))
    `(diredp-executable-tag ((,class (:foreground ,red :background nil))))
    `(diredp-file-name ((,class (:foreground ,yellow))))
    `(diredp-file-suffix ((,class (:foreground ,green))))
    `(diredp-flag-mark ((,class (:foreground ,green :inverse-video t))))
    `(diredp-flag-mark-line ((,class (:background nil :inherit highlight))))
    `(diredp-ignored-file-name ((,class (:foreground ,comment))))
    `(diredp-link-priv ((,class (:background nil :foreground ,purple))))
    `(diredp-mode-line-flagged ((,class (:foreground ,red))))
    `(diredp-mode-line-marked ((,class (:foreground ,green))))
    `(diredp-no-priv ((,class (:background nil))))
    `(diredp-number ((,class (:foreground ,yellow))))
    `(diredp-other-priv ((,class (:background nil :foreground ,purple))))
    `(diredp-rare-priv ((,class (:foreground ,red :background nil))))
    `(diredp-read-priv ((,class (:foreground ,green :background nil))))
    `(diredp-symlink ((,class (:foreground ,purple))))
    `(diredp-write-priv ((,class (:foreground ,yellow :background nil))))

    ;; Magit
    `(magit-blame-heading ((,class (:background ,highlight :foreground ,orange))))
    `(magit-blame-date ((,class (:foreground ,red))))
    `(magit-header-line ((,class (:inherit nil :weight bold))))
    `(magit-dimmed ((,class (:foreground ,comment))))
    `(magit-hash ((,class (:foreground ,comment))))
    `(magit-tag ((,class (:foreground ,yellow))))
    `(magit-branch-local ((,class (:foreground ,aqua))))
    `(magit-branch-remote ((,class (:foreground ,green))))
    `(magit-branch-current ((,class (:foreground ,blue))))
    `(magit-refname ((,class (:inherit comment))))
    `(magit-signature-good ((,class (:inherit success))))
    `(magit-signature-bad ((,class (:inherit error))))
    `(magit-signature-untrusted ((,class (:foreground ,aqua))))
    `(magit-signature-unmatched ((,class (:foreground ,aqua))))
    `(magit-cherry-equivalent ((,class (:foreground ,purple))))

    `(magit-log-graph ((,class (:foreground ,comment))))
    `(magit-log-author ((,class (:foreground ,orange))))
    `(magit-log-date ((,class (:foreground ,blue))))

    `(magit-process-ok ((,class (:inherit success))))
    `(magit-process-ng ((,class (:inherit error))))
    `(magit-section-heading ((,class (:foreground ,yellow :weight bold))))
    `(magit-section-heading-selection ((,class (:foreground ,orange :weight bold))))
    `(magit-section-highlight ((,class (:inherit highlight))))

    ;; git-gutter
    `(git-gutter:modified ((,class (:foreground ,purple :weight bold))))
    `(git-gutter:added ((,class (:foreground ,green :weight bold))))
    `(git-gutter:deleted ((,class (:foreground ,red :weight bold))))
    `(git-gutter:unchanged ((,class (:background ,yellow))))

    ;; git-gutter-fringe
    `(git-gutter-fr:modified ((,class (:foreground ,purple :weight bold))))
    `(git-gutter-fr:added ((,class (:foreground ,green :weight bold))))
    `(git-gutter-fr:deleted ((,class (:foreground ,red :weight bold))))

    ;; guide-key
    `(guide-key/prefix-command-face ((,class (:foreground ,blue))))
    `(guide-key/highlight-command-face ((,class (:foreground ,green))))
    `(guide-key/key-face ((,class (:foreground ,comment))))

    `(link ((,class (:foreground nil :underline t))))
    `(widget-button ((,class (:underline t))))
    `(widget-field ((,class (:background ,contrast-bg :box (:line-width 1 :color ,foreground)))))

    ;; Compilation (most faces politely inherit from 'success, 'error, 'warning etc.)))
    `(compilation-column-number ((,class (:foreground ,yellow))))
    `(compilation-line-number ((,class (:foreground ,yellow))))
    `(compilation-message-face ((,class (:foreground ,blue))))
    `(compilation-mode-line-exit ((,class (:foreground ,green))))
    `(compilation-mode-line-fail ((,class (:foreground ,red))))
    `(compilation-mode-line-run ((,class (:foreground ,blue))))

    ;; Grep
    `(grep-context-face ((,class (:foreground ,comment))))
    `(grep-error-face ((,class (:foreground ,red :weight bold :underline t))))
    `(grep-hit-face ((,class (:foreground ,blue))))
    `(grep-match-face ((,class (:foreground nil :background nil :inherit match))))

    `(regex-tool-matched-face ((,class (:foreground nil :background nil :inherit match))))

    ;; mark-multiple
    `(mm/master-face ((,class (:inherit region :foreground nil :background nil))))
    `(mm/mirror-face ((,class (:inherit region :foreground nil :background nil))))

    ;; helm
    `(helm-buffer-saved-out ((,class (:inherit warning))))
    `(helm-buffer-size ((,class (:foreground ,yellow))))
    `(helm-buffer-not-saved ((,class (:foreground ,orange))))
    `(helm-buffer-process ((,class (:foreground ,aqua))))
    `(helm-buffer-directory ((,class (:foreground ,blue))))
    `(helm-ff-dotted-directory ((,class (:foreground ,comment))))
    `(helm-ff-dotted-symlink-directory ((,class (:foreground ,comment))))
    `(helm-ff-directory ((,class (:foreground ,aqua))))
    `(helm-candidate-number ((,class (:foreground ,red))))
    `(helm-match ((,class (:inherit match))))
    `(helm-selection ((,class (:inherit highlight))))
    `(helm-separator ((,class (:foreground ,purple))))
    `(helm-source-header ((,class (:weight bold :foreground ,orange :height 1.44))))

    ;; company
    `(company-preview ((,class (:foreground ,comment :background ,contrast-bg))))
    `(company-preview-common ((,class (:inherit company-preview :foreground ,red))))
    `(company-preview-search ((,class (:inherit company-preview :foreground ,blue))))
    `(company-tooltip ((,class (:background ,contrast-bg))))
    `(company-tooltip-selection ((,class (:background ,highlight))))
    `(company-tooltip-common ((,class (:inherit company-tooltip :foreground ,red))))
    `(company-tooltip-common-selection ((,class (:inherit company-tooltip-selection :foreground ,red))))
    `(company-tooltip-search ((,class (:inherit company-tooltip :foreground ,blue))))
    `(company-tooltip-annotation ((,class (:inherit company-tooltip :foreground ,green))))
    `(company-scrollbar-bg ((,class (:inherit 'company-tooltip :background ,highlight))))
    `(company-scrollbar-fg ((,class (:background ,contrast-bg))))
    `(company-echo-common ((,class (:inherit company-echo :foreground ,red))))

    `(org-agenda-structure ((,class (:foreground ,purple))))
    `(org-agenda-date ((,class (:foreground ,blue :underline nil))))
    `(org-agenda-done ((,class (:foreground ,green))))
    `(org-agenda-dimmed-todo-face ((,class (:foreground ,comment))))
    `(org-block ((,class (:foreground ,orange))))
    `(org-code ((,class (:foreground ,yellow))))
    `(org-column ((,class (:background ,contrast-bg))))
    `(org-column-title ((,class (:inherit org-column :weight bold :underline t))))
    `(org-date ((,class (:foreground ,blue :underline t))))
    `(org-document-info ((,class (:foreground ,aqua))))
    `(org-document-info-keyword ((,class (:foreground ,green))))
    `(org-document-title ((,class (:weight bold :foreground ,orange :height 1.44))))
    `(org-done ((,class (:foreground ,green))))
    `(org-ellipsis ((,class (:foreground ,comment))))
    `(org-footnote ((,class (:foreground ,aqua))))
    `(org-formula ((,class (:foreground ,red))))
    `(org-hide ((,class (:foreground ,background :background ,background))))
    `(org-link ((,class (:foreground ,blue :underline t))))
    `(org-scheduled ((,class (:foreground ,green))))
    `(org-scheduled-previously ((,class (:foreground ,aqua))))
    `(org-scheduled-today ((,class (:foreground ,green))))
    `(org-special-keyword ((,class (:foreground ,orange))))
    `(org-table ((,class (:foreground ,purple))))
    `(org-todo ((,class (:foreground ,red))))
    `(org-upcoming-deadline ((,class (:foreground ,orange))))
    `(org-warning ((,class (:weight bold :foreground ,red))))

    `(markdown-url-face ((,class (:inherit link))))
    `(markdown-link-face ((,class (:foreground ,blue :underline t))))

      ;; hl-line-mode
    `(hl-sexp-face ((,class (:background ,contrast-bg))))
    `(highlight-symbol-face ((,class (:inherit isearch-lazy-highlight-face))))
    `(highlight-80+ ((,class (:background ,contrast-bg))))

      ;; Hydra
    `(hydra-face-blue ((,class (:foreground ,blue))))
    `(hydra-face-teal ((,class (:foreground ,aqua))))
    `(hydra-face-pink ((,class (:foreground ,purple))))
    `(hydra-face-red ((,class (:foreground ,red))))
    `(hydra-face-amaranth ((,class (:foreground ,orange))))

      ;; Python-specific overrides
    `(py-builtins-face ((,class (:foreground ,orange :weight normal))))

      ;; js2-mode
    `(js2-warning ((,class (:underline ,orange))))
    `(js2-error ((,class (:foreground nil :underline ,red))))
    `(js2-external-variable ((,class (:foreground ,purple))))
    `(js2-function-param ((,class (:foreground ,blue))))
    `(js2-instance-member ((,class (:foreground ,blue))))
    `(js2-private-function-call ((,class (:foreground ,red))))
      ;; js2-mode additional attributes for better syntax highlight in javascript
    `(js2-jsdoc-tag ((,class (:foreground ,aqua))))
    `(js2-jsdoc-type ((,class (:foreground ,orange))))
    `(js2-jsdoc-value ((,class (:foreground ,orange))))
    `(js2-function-call ((,class (:foreground ,foreground))))
    `(js2-object-property ((,class (:foreground ,foreground))))
    `(js2-private-member ((,class (:foreground ,purple))))
    `(js2-jsdoc-html-tag-name ((,class (:foreground ,orange))))
    `(js2-jsdoc-html-tag-delimiter ((,class (:foreground ,orange))))


      ;; js3-mode
    `(js3-warning-face ((,class (:underline ,orange))))
    `(js3-error-face ((,class (:foreground nil :underline ,red))))
    `(js3-external-variable-face ((,class (:foreground ,purple))))
    `(js3-function-param-face ((,class (:foreground ,blue))))
    `(js3-jsdoc-tag-face ((,class (:foreground ,orange))))
    `(js3-jsdoc-type-face ((,class (:foreground ,aqua))))
    `(js3-jsdoc-value-face ((,class (:foreground ,yellow))))
    `(js3-jsdoc-html-tag-name-face ((,class (:foreground ,blue))))
    `(js3-jsdoc-html-tag-delimiter-face ((,class (:foreground ,green))))
    `(js3-instance-member-face ((,class (:foreground ,blue))))
    `(js3-private-function-call-face ((,class (:foreground ,red))))

      ;; coffee-mode
    `(coffee-mode-class-name ((,class (:foreground ,orange :weight bold))))
    `(coffee-mode-function-param ((,class (:foreground ,purple))))

      ;; nxml
    `(nxml-name-face ((,class (:foreground unspecified :inherit font-lock-constant-face))))
    `(nxml-attribute-local-name-face ((,class (:foreground unspecified :inherit font-lock-variable-name-face))))
    `(nxml-ref-face ((,class (:foreground unspecified :inherit font-lock-preprocessor-face))))
    `(nxml-delimiter-face ((,class (:foreground unspecified :inherit font-lock-keyword-face))))
    `(nxml-delimited-data-face ((,class (:foreground unspecified :inherit font-lock-string-face))))
    `(rng-error-face ((,class (:underline ,red))))

      ;; RHTML
    `(erb-delim-face ((,class (:background ,contrast-bg))))
    `(erb-exec-face ((,class (:background ,contrast-bg :weight bold))))
    `(erb-exec-delim-face ((,class (:background ,contrast-bg))))
    `(erb-out-face ((,class (:background ,contrast-bg :weight bold))))
    `(erb-out-delim-face ((,class (:background ,contrast-bg))))
    `(erb-comment-face ((,class (:background ,contrast-bg :weight bold :slant italic))))
    `(erb-comment-delim-face ((,class (:background ,contrast-bg))))

      ;; Message-mode
    `(message-header-other ((,class (:foreground nil :background nil :weight normal))))
    `(message-header-subject ((,class (:inherit message-header-other :weight bold :foreground ,yellow))))
    `(message-header-to ((,class (:inherit message-header-other :weight bold :foreground ,orange))))
    `(message-header-cc ((,class (:inherit message-header-to :foreground nil))))
    `(message-header-name ((,class (:foreground ,blue :background nil))))
    `(message-header-newsgroups ((,class (:foreground ,aqua :background nil :slant normal))))
    `(message-separator ((,class (:foreground ,purple))))

      ;; Jabber
    `(jabber-chat-prompt-local ((,class (:foreground ,yellow))))
    `(jabber-chat-prompt-foreign ((,class (:foreground ,orange))))
    `(jabber-chat-prompt-system ((,class (:foreground ,yellow :weight bold))))
    `(jabber-chat-text-local ((,class (:foreground ,yellow))))
    `(jabber-chat-text-foreign ((,class (:foreground ,orange))))
    `(jabber-chat-text-error ((,class (:foreground ,red))))

    `(jabber-roster-user-online ((,class (:foreground ,green))))
    `(jabber-roster-user-xa ((,class (:foreground ,comment))))
    `(jabber-roster-user-dnd ((,class (:foreground ,yellow))))
    `(jabber-roster-user-away ((,class (:foreground ,orange))))
    `(jabber-roster-user-chatty ((,class (:foreground ,purple))))
    `(jabber-roster-user-error ((,class (:foreground ,red))))
    `(jabber-roster-user-offline ((,class (:foreground ,comment))))

    `(jabber-rare-time-face ((,class (:foreground ,comment))))
    `(jabber-activity-face ((,class (:foreground ,purple))))
    `(jabber-activity-personal-face ((,class (:foreground ,aqua))))

      ;; Outline
    `(outline-1 ((,class (:inherit nil :foreground ,blue))))
    `(outline-2 ((,class (:inherit nil :foreground ,purple))))
    `(outline-3 ((,class (:inherit nil :foreground ,aqua))))
    `(outline-4 ((,class (:inherit nil :foreground ,offwhite))))
    `(outline-5 ((,class (:inherit nil :foreground ,orange))))
    `(outline-6 ((,class (:inherit nil :foreground ,blue))))
    `(outline-7 ((,class (:inherit nil :foreground ,purple))))
    `(outline-8 ((,class (:inherit nil :foreground ,aqua))))
    `(outline-9 ((,class (:inherit nil :foreground ,yellow))))

      ;; Ledger-mode
    `(ledger-font-comment-face ((,class (:inherit font-lock-comment-face))))
    `(ledger-font-occur-narrowed-face ((,class (:inherit font-lock-comment-face :invisible t))))
    `(ledger-font-occur-xact-face ((,class (:inherit highlight))))
    `(ledger-font-payee-cleared-face ((,class (:foreground ,green))))
    `(ledger-font-payee-uncleared-face ((,class (:foreground ,aqua))))
    `(ledger-font-posting-date-face ((,class (:foreground ,orange))))
    `(ledger-font-posting-amount-face ((,class (:foreground ,foreground))))
    `(ledger-font-posting-account-cleared-face ((,class (:foreground ,blue))))
    `(ledger-font-posting-account-face ((,class (:foreground ,purple))))
    `(ledger-font-posting-account-pending-face ((,class (:foreground ,yellow))))
    `(ledger-font-xact-highlight-face ((,class (:inherit highlight))))
    `(ledger-occur-narrowed-face ((,class (:inherit font-lock-comment-face :invisible t))))
    `(ledger-occur-xact-face ((,class (:inherit highlight))))

      ;; EMMS
    `(emms-browser-artist-face ((,class (:inherit outline-2))))
    `(emms-browser-album-face ((,class (:inherit outline-3))))
    `(emms-browser-track-face ((,class (:inherit outline-4))))
    `(emms-browser-year/genre-face ((,class (:inherit outline-1))))
    `(emms-playlist-selected-face ((,class (:inverse-video t))))
    `(emms-playlist-track-face ((,class (:inherit outline-4))))

      ;; mu4e
    `(mu4e-header-highlight-face ((,class (:underline nil :inherit region))))
    `(mu4e-header-marks-face ((,class (:underline nil :foreground ,yellow))))
    `(mu4e-flagged-face ((,class (:foreground ,orange :inherit nil))))
    `(mu4e-replied-face ((,class (:foreground ,blue :inherit nil))))
    `(mu4e-unread-face ((,class (:foreground ,yellow :inherit nil))))
    `(mu4e-cited-1-face ((,class (:inherit outline-1 :slant normal))))
    `(mu4e-cited-2-face ((,class (:inherit outline-2 :slant normal))))
    `(mu4e-cited-3-face ((,class (:inherit outline-3 :slant normal))))
    `(mu4e-cited-4-face ((,class (:inherit outline-4 :slant normal))))
    `(mu4e-cited-5-face ((,class (:inherit outline-5 :slant normal))))
    `(mu4e-cited-6-face ((,class (:inherit outline-6 :slant normal))))
    `(mu4e-cited-7-face ((,class (:inherit outline-7 :slant normal))))
    `(mu4e-ok-face ((,class (:foreground ,green))))
    `(mu4e-view-contact-face ((,class (:inherit nil :foreground ,yellow))))
    `(mu4e-view-link-face ((,class (:inherit link :foreground ,blue))))
    `(mu4e-view-url-number-face ((,class (:inherit nil :foreground ,aqua))))
    `(mu4e-view-attach-number-face ((,class (:inherit nil :foreground ,orange))))
    `(mu4e-highlight-face ((,class (:inherit highlight))))
    `(mu4e-title-face ((,class (:inherit nil :foreground ,green))))

      ;; Gnus
    `(gnus-cite-1 ((,class (:inherit outline-1 :foreground nil))))
    `(gnus-cite-2 ((,class (:inherit outline-2 :foreground nil))))
    `(gnus-cite-3 ((,class (:inherit outline-3 :foreground nil))))
    `(gnus-cite-4 ((,class (:inherit outline-4 :foreground nil))))
    `(gnus-cite-5 ((,class (:inherit outline-5 :foreground nil))))
    `(gnus-cite-6 ((,class (:inherit outline-6 :foreground nil))))
    `(gnus-cite-7 ((,class (:inherit outline-7 :foreground nil))))
    `(gnus-cite-8 ((,class (:inherit outline-8 :foreground nil))))
    `(gnus-header-content ((,class (:inherit message-header-other))))
    `(gnus-header-subject ((,class (:inherit message-header-subject))))
    `(gnus-header-from ((,class (:inherit message-header-other-face :weight bold :foreground ,orange))))
    `(gnus-header-name ((,class (:inherit message-header-name))))
    `(gnus-button ((,class (:inherit link :foreground nil))))
    `(gnus-signature ((,class (:inherit font-lock-comment-face))))

    `(gnus-summary-normal-unread ((,class (:foreground ,blue :weight normal))))
    `(gnus-summary-normal-read ((,class (:foreground ,foreground :weight normal))))
    `(gnus-summary-normal-ancient ((,class (:foreground ,aqua :weight normal))))
    `(gnus-summary-normal-ticked ((,class (:foreground ,orange :weight normal))))
    `(gnus-summary-low-unread ((,class (:foreground ,comment :weight normal))))
    `(gnus-summary-low-read ((,class (:foreground ,comment :weight normal))))
    `(gnus-summary-low-ancient ((,class (:foreground ,comment :weight normal))))
    `(gnus-summary-high-unread ((,class (:foreground ,yellow :weight normal))))
    `(gnus-summary-high-read ((,class (:foreground ,green :weight normal))))
    `(gnus-summary-high-ancient ((,class (:foreground ,green :weight normal))))
    `(gnus-summary-high-ticked ((,class (:foreground ,orange :weight normal))))
    `(gnus-summary-cancelled ((,class (:foreground ,red :background nil :weight normal))))

    `(gnus-group-mail-low ((,class (:foreground ,comment))))
    `(gnus-group-mail-low-empty ((,class (:foreground ,comment))))
    `(gnus-group-mail-1 ((,class (:foreground nil :weight normal :inherit outline-1))))
    `(gnus-group-mail-2 ((,class (:foreground nil :weight normal :inherit outline-2))))
    `(gnus-group-mail-3 ((,class (:foreground nil :weight normal :inherit outline-3))))
    `(gnus-group-mail-4 ((,class (:foreground nil :weight normal :inherit outline-4))))
    `(gnus-group-mail-5 ((,class (:foreground nil :weight normal :inherit outline-5))))
    `(gnus-group-mail-6 ((,class (:foreground nil :weight normal :inherit outline-6))))
    `(gnus-group-mail-1-empty ((,class (:inherit gnus-group-mail-1 :foreground ,comment))))
    `(gnus-group-mail-2-empty ((,class (:inherit gnus-group-mail-2 :foreground ,comment))))
    `(gnus-group-mail-3-empty ((,class (:inherit gnus-group-mail-3 :foreground ,comment))))
    `(gnus-group-mail-4-empty ((,class (:inherit gnus-group-mail-4 :foreground ,comment))))
    `(gnus-group-mail-5-empty ((,class (:inherit gnus-group-mail-5 :foreground ,comment))))
    `(gnus-group-mail-6-empty ((,class (:inherit gnus-group-mail-6 :foreground ,comment))))
    `(gnus-group-news-1 ((,class (:foreground nil :weight normal :inherit outline-5))))
    `(gnus-group-news-2 ((,class (:foreground nil :weight normal :inherit outline-6))))
    `(gnus-group-news-3 ((,class (:foreground nil :weight normal :inherit outline-7))))
    `(gnus-group-news-4 ((,class (:foreground nil :weight normal :inherit outline-8))))
    `(gnus-group-news-5 ((,class (:foreground nil :weight normal :inherit outline-1))))
    `(gnus-group-news-6 ((,class (:foreground nil :weight normal :inherit outline-2))))
    `(gnus-group-news-1-empty ((,class (:inherit gnus-group-news-1 :foreground ,comment))))
    `(gnus-group-news-2-empty ((,class (:inherit gnus-group-news-2 :foreground ,comment))))
    `(gnus-group-news-3-empty ((,class (:inherit gnus-group-news-3 :foreground ,comment))))
    `(gnus-group-news-4-empty ((,class (:inherit gnus-group-news-4 :foreground ,comment))))
    `(gnus-group-news-5-empty ((,class (:inherit gnus-group-news-5 :foreground ,comment))))
    `(gnus-group-news-6-empty ((,class (:inherit gnus-group-news-6 :foreground ,comment))))

    `(erc-direct-msg-face ((,class (:foreground ,orange))))
    `(erc-error-face ((,class (:foreground ,red))))
    `(erc-header-face ((,class (:foreground ,foreground :background ,highlight))))
    `(erc-input-face ((,class (:foreground ,green))))
    `(erc-keyword-face ((,class (:foreground ,yellow))))
    `(erc-current-nick-face ((,class (:foreground ,green))))
    `(erc-my-nick-face ((,class (:foreground ,green))))
    `(erc-nick-default-face ((,class (:weight normal :foreground ,purple))))
    `(erc-nick-msg-face ((,class (:weight normal :foreground ,yellow))))
    `(erc-notice-face ((,class (:foreground ,comment))))
    `(erc-pal-face ((,class (:foreground ,orange))))
    `(erc-prompt-face ((,class (:foreground ,blue))))
    `(erc-timestamp-face ((,class (:foreground ,aqua))))
    `(erc-keyword-face ((,class (:foreground ,green))))

      ;; twittering-mode
    `(twittering-username-face ((,class (:inherit erc-pal-face))))
    `(twittering-uri-face ((,class (:foreground ,blue :inherit link))))
    `(twittering-timeline-header-face ((,class (:foreground ,green :weight bold))))
    `(twittering-timeline-footer-face ((,class (:inherit twittering-timeline-header-face))))

    `(custom-variable-tag ((,class (:foreground ,blue))))
    `(custom-group-tag ((,class (:foreground ,blue))))
    `(custom-state ((,class (:foreground ,green))))

      ;; ansi-term
    `(term ((,class (:foreground nil :background nil :inherit default))))
    `(term-color-black   ((,class (:foreground ,foreground :background ,foreground))))
    `(term-color-red     ((,class (:foreground ,red :background ,red))))
    `(term-color-green   ((,class (:foreground ,green :background ,green))))
    `(term-color-yellow  ((,class (:foreground ,yellow :background ,yellow))))
    `(term-color-blue    ((,class (:foreground ,blue :background ,blue))))
    `(term-color-magenta ((,class (:foreground ,purple :background ,purple))))
    `(term-color-cyan    ((,class (:foreground ,aqua :background ,aqua))))
    `(term-color-white   ((,class (:foreground ,background :background ,background))))

      ;; e2wm
    `(e2wm:face-history-list-normal ((,class (:foreground ,foreground :background ,background))))
    `(e2wm:face-history-list-select1 ((,class (:foreground ,aqua :background ,background))))
    `(e2wm:face-history-list-select2 ((,class (:foreground ,yellow :background ,background))))

      ;; rpm-spec-mode
    `(rpm-spec-dir-face ((,class (:foreground ,green))))
    `(rpm-spec-doc-face ((,class (:foreground ,green))))
    `(rpm-spec-ghost-face ((,class (:foreground ,red))))
    `(rpm-spec-macro-face ((,class (:foreground ,yellow))))
    `(rpm-spec-obsolete-tag-face ((,class (:foreground ,red))))
    `(rpm-spec-package-face ((,class (:foreground ,red))))
    `(rpm-spec-section-face ((,class (:foreground ,yellow))))
    `(rpm-spec-tag-face ((,class (:foreground ,blue))))
    `(rpm-spec-var-face ((,class (:foreground ,red))))

      ;; sx
    `(sx-question-mode-content-face ((,class (:background ,highlight))))
    `(sx-question-list-answers ((,class (:height 1.0 :inherit sx-question-list-parent :foreground ,green))))
    `(sx-question-mode-accepted ((,class (:height 1.5 :inherit sx-question-mode-title :foreground ,green))))
    `(sx-question-mode-kbd-tag ((,class (:height 0.9 :weight semi-bold :box (:line-width 3 :style released-button :color ,contrast-bg)))))))


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'happy-hacking)

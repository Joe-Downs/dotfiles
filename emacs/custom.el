(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-interval 10)
 '(auto-save-visited-interval 10)
 '(bibtex-comment-start "%")
 '(display-fill-column-indicator-character 124)
 '(elcord-refresh-rate 5)
 '(elcord-use-major-mode-as-main-icon t)
 '(fill-column 80)
 '(fringe-mode 0 nil (fringe))
 '(helm-full-frame t)
 '(inhibit-startup-screen t)
 '(lsp-ui-doc-position 'at-point)
 '(lsp-ui-doc-show-with-cursor t)
 '(org-agenda-files nil)
 '(org-cite-global-bibliography '("~/git/roam-notes/refs.bib"))
 '(org-duration-format '((special . h:mm)))
 '(org-format-latex-options
   '(:foreground default :background default :scale 1.5 :html-foreground "Black"
                 :html-background "Transparent" :html-scale 1.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\[")))
 '(org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
 '(org-support-shift-select t)
 '(package-selected-packages
   '(anaconda-mode auctex auto-complete-rst banner-comment bibtex-utils company
                   company-anaconda company-c-headers company-fuzzy diredfl
                   docker dockerfile-mode elcord elpher fill-column-indicator
                   fzf gemini-mode gh-md graphviz-dot-mode groovy-mode helm
                   helm-ag helm-lsp helm-projectile helm-swoop hl-sentence
                   json-mode julia-mode kotlin-mode latex-extra ligature
                   list-unicode-display lorem-ipsum lsp-java lsp-julia lsp-latex
                   lsp-pyright lsp-ui magit markdown-mode mood-line
                   multiple-cursors nasm-mode org-roam org-roam-ui poporg
                   powershell projectile python-docstring pyvenv
                   rainbow-delimiters rainbow-identifiers sideline-lsp
                   sql-indent toml-mode treemacs-projectile use-package web-mode
                   wucuo yaml yaml-mode yasnippet yasnippet-snippets))
 '(save-place-mode t)
 '(scroll-bar-mode nil)
 '(scroll-down-aggressively 0.25)
 '(scroll-margin 5)
 '(send-mail-function 'smtpmail-send-it)
 '(sentence-end-double-space nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(treemacs-eldoc-display 'detailed)
 '(treemacs-indent-guide-mode t)
 '(treemacs-indent-guide-style 'line)
 '(truncate-lines t)
 '(truncate-partial-width-windows nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#232627" :foreground "#fcfcfc" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 120 :width normal :foundry "PfEd" :family "Firacode"))))
 '(custom-group-tag ((t (:inherit variable-pitch :foreground "#d7d700" :weight bold :height 1.2))))
 '(custom-variable-tag ((t (:foreground "#af5f00" :weight bold))))
 '(fill-column-indicator ((t (:foreground "#cd0000"))))
 '(font-latex-sectioning-5-face ((t (:foreground "#00ffff" :weight bold))))
 '(font-lock-builtin-face ((t (:foreground "#cd5c5c"))))
 '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
 '(font-lock-comment-face ((t (:foreground "#5faf00"))))
 '(font-lock-doc-face ((t (:foreground "#875f00"))))
 '(font-lock-function-name-face ((t (:foreground "#cdcd00"))))
 '(font-lock-keyword-face ((t (:foreground "#af5fd7"))))
 '(font-lock-string-face ((t (:foreground "#d75f87"))))
 '(font-lock-type-face ((t (:foreground "#00cdcd"))))
 '(helm-ff-directory ((t (:inherit diredfl-dir-name))))
 '(helm-ff-dirs ((t (:inherit helm-ff-directory))))
 '(helm-ff-dotted-directory ((t (:inherit helm-ff-directory))))
 '(helm-ff-file ((t (:extend t :inherit diredfl-file-name))))
 '(helm-ff-file-extension ((t (:extend t :inherit diredfl-file-suffix))))
 '(helm-ff-symlink ((t (:inherit diredfl-symlink :extend t))))
 '(helm-ff-truename ((t (:inherit diredfl-file-name :extend t))))
 '(magit-hash ((t (:foreground "#cdcd00"))))
 '(magit-section-highlight ((t (:extend t :background "#005fff"))))
 '(minibuffer-prompt ((t (:foreground "#00ffff"))))
 '(mood-line-unimportant ((t (:foreground "indian red" :weight bold))))
 '(org-formula ((t (:foreground "#5fd75f"))))
 '(org-table ((t (:foreground "#5c5cff"))))
 '(outline-1 ((t (:foreground "#ffafff"))))
 '(outline-2 ((t (:foreground "#ff8787"))))
 '(outline-3 ((t (:foreground "#d7af87"))))
 '(outline-4 ((t (:foreground "#87af87"))))
 '(outline-5 ((t (:foreground "#87d7ff"))))
 '(outline-6 ((t (:foreground "#5f87ff"))))
 '(outline-7 ((t (:foreground "#875fff"))))
 '(outline-8 ((t (:foreground "#af5fd7"))))
 '(web-mode-html-tag-bracket-face ((t (:foreground "#e5e5e5")))))

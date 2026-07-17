;;; jd-dark-theme.el --- Joe Downs dark theme -*- lexical-binding: t -*-
(deftheme jd-dark "Joe Downs dark theme.")

(custom-theme-set-faces
 'jd-dark

 ;; Core
 '(default              ((t (:background "#232627" :foreground "#fcfcfc"
                             :family "Firacode" :foundry "CTDB"
                             :height 120 :weight regular))))
 '(bold                 ((t (:weight heavy))))
 '(italic               ((t (:slant italic :family "Fira Mono"))))

 ;; Syntax
 '(font-lock-builtin-face           ((t (:foreground "#cd5c5c"))))
 '(font-lock-comment-face           ((t (:foreground "#5faf00"))))
 '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
 '(font-lock-doc-face               ((t (:foreground "#875f00"))))
 '(font-lock-function-name-face     ((t (:foreground "#cdcd00"))))
 '(font-lock-keyword-face           ((t (:foreground "#af5fd7"))))
 '(font-lock-string-face            ((t (:foreground "#d75f87"))))
 '(font-lock-type-face              ((t (:foreground "#00cdcd"))))

 ;; UI
 '(fill-column-indicator ((t (:foreground "#cd0000"))))
 '(minibuffer-prompt     ((t (:foreground "#00ffff"))))

 ;; Outlines / Org headings
 '(outline-1 ((t (:foreground "#ffafff"))))
 '(outline-2 ((t (:foreground "#ff8787"))))
 '(outline-3 ((t (:foreground "#d7af87"))))
 '(outline-4 ((t (:foreground "#87af87"))))
 '(outline-5 ((t (:foreground "#87d7ff"))))
 '(outline-6 ((t (:foreground "#5f87ff"))))
 '(outline-7 ((t (:foreground "#875fff"))))
 '(outline-8 ((t (:foreground "#af5fd7"))))

 ;; Org
 '(org-formula ((t (:foreground "#5fd75f"))))
 '(org-link    ((t (:inherit link :underline nil :weight bold))))
 '(org-table   ((t (:foreground "#5c5cff"))))

 ;; LaTeX
 '(font-latex-sectioning-5-face ((t (:foreground "#00ffff" :weight bold))))

 ;; Helm
 '(helm-ff-directory        ((t (:inherit diredfl-dir-name))))
 '(helm-ff-dirs             ((t (:inherit helm-ff-directory))))
 '(helm-ff-dotted-directory ((t (:inherit helm-ff-directory))))
 '(helm-ff-file             ((t (:extend t :inherit diredfl-file-name))))
 '(helm-ff-file-extension   ((t (:extend t :inherit diredfl-file-suffix))))
 '(helm-ff-symlink          ((t (:inherit diredfl-symlink :extend t))))
 '(helm-ff-truename         ((t (:inherit diredfl-file-name :extend t))))
 '(helm-selection           ((t (:extend t :background "ForestGreen"
                                 :foreground "black" :distant-foreground "black"))))

 ;; Magit
 '(magit-hash              ((t (:foreground "#cdcd00"))))
 '(magit-section-highlight ((t (:extend t :background "#005fff"))))

 ;; Mood-line
 '(mood-line-unimportant ((t (:foreground "indian red" :weight bold))))

 ;; Web
 '(web-mode-html-tag-bracket-face ((t (:foreground "#e5e5e5"))))

 ;; Custom buffer
 '(custom-group-tag    ((t (:inherit variable-pitch :foreground "#d7d700"
                             :weight bold :height 1.2))))
 '(custom-variable-tag ((t (:foreground "#af5f00" :weight bold)))))

(provide-theme 'jd-dark)
;;; jd-dark-theme.el ends here

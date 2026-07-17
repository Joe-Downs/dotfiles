;;; jd-light-theme.el --- Joe Downs light theme -*- lexical-binding: t -*-
(deftheme jd-light "Joe Downs light theme.")

(custom-theme-set-faces
 'jd-light

 ;; Core
 '(default ((t (:background "#f5f5f0" :foreground "#1a1a2e"
                :family "Firacode" :foundry "CTDB"
                :height 120 :weight regular))))
 '(bold   ((t (:weight heavy))))
 '(italic ((t (:slant italic :family "Fira Mono"))))

 ;; Syntax — same hues as dark, darkened for a light background
 '(font-lock-builtin-face           ((t (:foreground "#aa2020"))))  ; dark red   ← #cd5c5c
 '(font-lock-comment-face           ((t (:foreground "#3a7a00"))))  ; dark green ← #5faf00
 '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
 '(font-lock-doc-face               ((t (:foreground "#6b4400"))))  ; dark amber ← #875f00
 '(font-lock-function-name-face     ((t (:foreground "#787800"))))  ; olive      ← #cdcd00
 '(font-lock-keyword-face           ((t (:foreground "#7722bb"))))  ; dark purple← #af5fd7
 '(font-lock-string-face            ((t (:foreground "#b01050"))))  ; dark rose  ← #d75f87
 '(font-lock-type-face              ((t (:foreground "#006b6b"))))  ; dark teal  ← #00cdcd

 ;; UI
 '(fill-column-indicator ((t (:foreground "#cc0000"))))
 '(minibuffer-prompt     ((t (:foreground "#006b8a"))))             ; dark cyan  ← #00ffff

 ;; Outlines / Org headings — same hue sequence, darkened
 '(outline-1 ((t (:foreground "#990099"))))  ; dark magenta  ← #ffafff
 '(outline-2 ((t (:foreground "#cc2222"))))  ; dark coral    ← #ff8787
 '(outline-3 ((t (:foreground "#7a5500"))))  ; dark tan      ← #d7af87
 '(outline-4 ((t (:foreground "#336633"))))  ; dark sage     ← #87af87
 '(outline-5 ((t (:foreground "#1a6699"))))  ; dark sky blue ← #87d7ff
 '(outline-6 ((t (:foreground "#2244cc"))))  ; dark periwinkle← #5f87ff
 '(outline-7 ((t (:foreground "#5522bb"))))  ; dark violet   ← #875fff
 '(outline-8 ((t (:foreground "#7722aa"))))  ; dark purple   ← #af5fd7

 ;; Org
 '(org-formula ((t (:foreground "#2a7a2a"))))  ; dark green ← #5fd75f
 '(org-link    ((t (:inherit link :underline nil :weight bold))))
 '(org-table   ((t (:foreground "#2222cc"))))  ; dark blue  ← #5c5cff

 ;; LaTeX
 '(font-latex-sectioning-5-face ((t (:foreground "#006b8a" :weight bold))))

 ;; Helm
 '(helm-ff-directory        ((t (:inherit diredfl-dir-name))))
 '(helm-ff-dirs             ((t (:inherit helm-ff-directory))))
 '(helm-ff-dotted-directory ((t (:inherit helm-ff-directory))))
 '(helm-ff-file             ((t (:extend t :inherit diredfl-file-name))))
 '(helm-ff-file-extension   ((t (:extend t :inherit diredfl-file-suffix))))
 '(helm-ff-symlink          ((t (:inherit diredfl-symlink :extend t))))
 '(helm-ff-truename         ((t (:inherit diredfl-file-name :extend t))))
 '(helm-selection           ((t (:extend t :background "#b8ddb8"
                                 :foreground "black" :distant-foreground "black"))))

 ;; Magit
 '(magit-hash              ((t (:foreground "#787800"))))          ; olive      ← #cdcd00
 '(magit-section-highlight ((t (:extend t :background "#c8d8ff"))));light blue  ← #005fff

 ;; Mood-line
 '(mood-line-unimportant ((t (:foreground "#8b2020" :weight bold))))

 ;; Web
 '(web-mode-html-tag-bracket-face ((t (:foreground "#505050"))))

 ;; Custom buffer
 '(custom-group-tag    ((t (:inherit variable-pitch :foreground "#878700"
                             :weight bold :height 1.2))))
 '(custom-variable-tag ((t (:foreground "#8a4400" :weight bold)))))

(provide-theme 'jd-light)
;;; jd-light-theme.el ends here

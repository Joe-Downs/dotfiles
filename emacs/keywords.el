;;; -*- lexical-binding: t; -*-
;;; Octave
(font-lock-add-keywords 'octave-mode
			;; Highlight variables in assignment, skip ==
			`(("\\([[:graph:]]*\\)\\([[:blank:]]\\{1,\\}=\\{1\\}[[:blank:]]\\{1,\\}\\)\\([[:graph:]]\\)"
			   1 font-lock-variable-name-face)))

;;; Programming
;; Keywords for every programming mode. Specific languages will follow in the
;; form of sub headings
(add-hook 'prog-mode-hook
	  (lambda ()
	    (font-lock-add-keywords nil `(("\\<\\(TODO\\|FIXME\\):"
					   0 font-lock-warning-face prepend)))))

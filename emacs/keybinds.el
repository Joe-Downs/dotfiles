;;; -*- lexical-binding: t; -*-
;;; keybinds.el
;; This file stores all my manually-set keyboard shortcuts; redefining shortcuts
;; that don't quite work for me.
(global-set-key (kbd "C-x g") 'goto-line)
(global-set-key (kbd "C-<return>") 'company-complete-selection)
(global-set-key (kbd "C-c s i") 'yas-insert-snippet)
(global-set-key (kbd "C-c s n") 'yas-new-snippet)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c \"") 'poporg-dwim)
(global-set-key (kbd "C-x C-i") 'upcase-initials-region)

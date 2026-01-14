;;;; org-mode Customization
(setq org-adapt-indentation t)

;; Automatically toggle LaTeX fragments in org-mode when cursor is over them
(use-package org-fragtog
:hook (org-mode . org-fragtog) ; this auto-enables it when you enter an org-buffer
)

;;; org-roam Customization
;; Sample Config from the git repo
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/git/roam-notes/"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; Define method to get the hierarchy for (sub)heading nodes. Display
  ;; something like: Title > Heading > Subheading
  (cl-defmethod org-roam-node-hierarchy ((node org-roam-node))
    (let ((level (org-roam-node-level node)))
      (concat
       (when (> level 0) (concat (org-roam-node-file-title node) " > "))
       (when (> level 1) (concat (string-join (org-roam-node-olp node) " > ") " > "))
       (org-roam-node-title node))))
  ;; TODO: this should be percentage based, or right-align the tags
  (setq org-roam-node-display-template (concat "${hierarchy:*} " (propertize "${tags:*}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

;; Use a new default template for org-roam-captures. This just gives us the node
;; name as the filename, instead of a loooong timestamp. I think my note-taking
;; method (at least for now until I look into journaling and whatnot, I won't
;; have nodes with the same name; instead just expanding on the current one.
(setq org-roam-capture-templates
      '(("d" "default" plain "%?"
	 :target (file+head "${title}.org"
                            "#+title: ${title}\n")
	 :unnarrowed t)))

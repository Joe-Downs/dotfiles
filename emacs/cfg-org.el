;;;; Org Related Customization (org-mode, org-roam, etc)
(setq org-dir (file-truename "~/git/roam-notes/"))

;;; org-mode Customization
;;(setq org-adapt-indentation t)
(use-package org
  :ensure t
  :bind (("C-c c" . org-capture)
         ("C-c g" . org-agenda)
         )
  :custom
  ((org-adapt-indentation t)
   (org-agenda-files `("tasks.org"
                       "inbox.org"
                       "cse/homelab/tasks.org"
                       "school/thesis/thesis-tasks.org"
                       "school/thesis/thesis-plan.org"
                       "school/assignments.org"))
   (org-capture-templates `(("i" "Inbox" entry  (file "inbox.org")
                            ,(concat "* TODO %?\n"
                                     "  /Entered on/ %U"))
                           ))
   (org-directory org-dir)
   (org-global-properties
    '(("Effort_ALL" . "0:00 0:05 0:10 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 7:00")))
   (org-log-done 'time)
   (org-outline-path-complete-in-steps nil)
   (org-refile-targets '((org-agenda-files :maxlevel . 2)))
   (org-refile-use-outline-path 'file)
   ))

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
         ("C-c n j" . org-roam-dailies-capture-today)
         ;; Only bind these when we're in org-mode, since we'll only use them
         ;; within a file, not globally like the above
         :map org-mode-map
         ("C-c a t" . org-roam-tag-add)
         ("C-c a a" . org-roam-alias-add)
         ("C-c a r" . org-roam-ref-add)
         ("C-c r t" . org-roam-tag-remove)
         ("C-c r a" . org-roam-alias-remove)
         ("C-c r r" . org-roam-ref-remove)
         )
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

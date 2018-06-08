(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(setq-default org-hide-leading-stars t)

(setq org-todo-keywords
    '((sequence "TODO(t)"
		"WAITING(w)"
		"|"
		"DONE(d)"
		"CANCELED(c)")))

(setq org-capture-templates
    '(("j" "Journal Entry"
	entry (file+datetree "~/org/journal.org")
	"* %?")
	("T" "Tickler Entry"
	entry (file+headline "~/org/tickler.org" "Tickler")
	"* %i%?\n  %U")
	("t" "Task [inbox]"
	entry (file+headline "~/org/inbox.org" "Inbox")
	"* TODO %?"
	:empty-lines 1
	:prepend t)))

(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-refile-targets '((org-agenda-files :maxlevel . 5)))

(setq org-agenda-files (list "~/org/inbox.org"
			    "~/org/tickler.org"
			    "~/org/gtd.org"))

(defun rpc/open-org-file ()
  "Open my primary org file."
  (interactive)
  (find-file "~/org/gtd.org"))

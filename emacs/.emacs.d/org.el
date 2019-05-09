(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(setq-default org-hide-leading-stars t)

(require 'periodic-commit-minor-mode)
(add-hook 'org-mode-hook 'periodic-commit-minor-mode)

(setq org-todo-keywords
    '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "HOLD(h)" "|" "DONE(d)" "CANCELED(c)")))

(setq org-todo-state-tags-triggers
      '(("CANCELED" ("CANCELED" . t))
	("WAITING" ("WAITING" . t))
	("HOLD" ("WAITING") ("HOLD" . t))
	(done ("WAITING") ("HOLD"))
	("TODO" ("WAITING") ("CANCELED") ("HOLD"))
	("NEXT" ("WAITING") ("CANCELED") ("HOLD"))
	("DONE" ("WAITING") ("CANCELED") ("HOLD"))))

(setq org-capture-templates
    '(("j" "Journal Entry"
	entry (file+datetree "~/org/journal.org")
	"* %?")
	("T" "Tickler Entry"
	entry (file+headline "~/org/tickler.org" "Tickler")
	"* %i%?\n%U\n%a")
	("n" "Templates for creating notes")
	("nn" "General Note"
	 entry (file+headline "~/org/gtd.org" "Notes")
	 "* %?\n%U"
	 :empty-lines 1
	 :prepend t)
	("nr" "Retro reflection"
	 entry (file+headline "~/org/gtd.org" "Retro thoughts")
	 "* %?\n%U"
	 :empty-lines 1
	 :prepend t)
	("nf" "Feedback example"
	 entry (file+headline "~/org/gtd.org" "Feedback examples")
	 "* %^{Title} %^g\n%U\n%?"
	 :empty-lines 1
	 :prepend t)
	("np" "Personal reflection"
	 entry (file+headline "~/org/gtd.org" "Personal reflections")
	 "* %^{Title}\n%U\n%?"
	 :empty-lines 1
	 :prepend t)
	("q" "Question"
	 entry (file+headline "~/org/gtd.org" "Questions")
	 "* TODO %? %^g"
	 :empty-lines 1
	 :prepend t)
	("p" "Project"
	 entry (file+headline "~/org/gtd.org" "Projects")
	 "* TODO %^{Project}\n:PROPERTIES:\n:CATEGORY: %^{Slug}\n:END:"
	 :empty-lines 1
	 :prepend t
	 :immediate-finish t)
	("t" "Task"
	entry (file+headline "~/org/gtd.org" "Inbox")
	"* TODO %?"
	:empty-lines 1
	:prepend t)))

(defvar rpc/jira-domain "jira.mongodb.org")
(defun org-jira-link-open (ticket)
  (org-open-link-from-string
   (format "https://%s/browse/%s" rpc/jira-domain ticket)))
(defun org-jira-link-complete ()
  (concat "jira:" (read-string "ticket: ")))
(org-link-set-parameters "jira"
						 :complete 'org-jira-link-complete
						 :follow 'org-jira-link-open)

(defun rpc/org-link-describe (link desc)
  (let* ((parts (split-string link ":"))
		 (protocol (car parts))
		 (path (cadr parts)))
	path))

(setq org-make-link-description-function #'rpc/org-link-describe)

(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-refile-targets '((nil :maxlevel . 9)
			   (org-agenda-files :maxlevel . 9)))

(defun rpc/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets."
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'rpc/verify-refile-target)

(add-hook 'org-capture-mode-hook 'auto-fill-mode)
(add-hook 'org-capture-mode-hook 'evil-insert-state)
(add-hook 'org-log-buffer-setup-hook 'evil-insert-state)

(require 'org-agenda)

(define-key org-agenda-mode-map "j" 'org-agenda-next-item)
(define-key org-agenda-mode-map "k" 'org-agenda-previous-item)

(setq org-agenda-files (list "~/org/tickler.org" "~/org/gtd.org"))

(setq org-tag-persistent-alist
	  '(("@seth")
	    ("@wisdom")
	    ("@patrick")
	    ("@chuck")
	    ("@craigh")
	    ("@devin")
	    ("@matt")))

(setq rpc/org-agenda-block-notes '(tags "NOTE" ((org-agenda-overriding-header "Notes"))))
(setq rpc/org-agenda-block-refile '(tags "REFILE" ((org-agenda-overriding-header "Items to Refile"))))
(setq rpc/org-agenda-block-waiting '(tags-todo "-CANCELED+WAITING|HOLD/!"
											   ((org-agenda-overriding-header "Waiting and Postponed Tasks")
												(org-agenda-skip-function 'rpc/skip-non-tasks)
												(org-tags-match-list-sublevels nil))))
(setq rpc/org-agenda-block-misc '(tags-todo "misc" ((org-agenda-overriding-header "Misc Small Tasks"))))
(setq rpc/org-agenda-block-jira '(tags-todo "@jira" ((org-agenda-overriding-header "Tasks Needing JIRA"))))
(setq rpc/org-agenda-block-workflow '(tags-todo "workflow" ((org-agenda-overriding-header "Workflow Improvements"))))
(setq rpc/org-agenda-block-discussions '(tags-todo "discussion" ((org-agenda-overriding-header "Async Discussions"))))
(setq rpc/org-agenda-block-archivable '(tags-todo "-REFILE/"
											 ((org-agenda-overriding-header "To Archive")
											  (org-agenda-skip-function 'rpc/skip-non-archivable)
											  (org-tags-match-list-sublevels nil))))

(defun rpc/org-agenda-block-person (abbr)
  `(tags-todo ,(format "@%s" abbr)
			  ((org-agenda-overriding-header ,(format "TODOs requiring %s" abbr)))))

(setq rpc/org-agenda-people
	  `("p" "Tasks by Person"
		,(mapcar 'rpc/org-agenda-block-person
				 '("seth"
				   "craigh"
				   "wisdom"
				   "chuck"
				   "patrick"
				   "devin"
				   "matt"))))

(setq rpc/org-agenda-workflow
	  `("w" "Workflow Improvement Agenda"
		(,rpc/org-agenda-block-workflow)))

(setq rpc/org-agenda-discussions
	  `("d" "Async Discussion Agenda"
		(,rpc/org-agenda-block-discussions)))

(setq rpc/org-agenda-refile
	  `("r" "Items to Refile"
		(,rpc/org-agenda-block-refile)))

(setq rpc/org-agenda-archive
	  `("A" "Items to Archive"
		(,rpc/org-agenda-block-archivable)))

(setq rpc/org-agenda-main
	  `(" " "Main Agenda"
		(;(agenda "" nil)
		 ,rpc/org-agenda-block-refile
		 ,rpc/org-agenda-block-jira
		 (tags-todo "-CANCELED/!"
					((org-agenda-overriding-header "Stuck Projects")
					 (org-agenda-skip-function 'rpc/skip-non-stuck-projects)))
		 ,rpc/org-agenda-block-waiting
		 (tags-todo "-CANCELED/!NEXT"
					((org-agenda-overriding-header "Project Next Tasks")
					 (org-agenda-skip-function 'rpc/skip-projects-and-single-tasks)))
		 (tags-todo "-HOLD-CANCELED/!"
					((org-agenda-overriding-header "Projects")
					 (org-agenda-skip-function 'rpc/skip-non-projects)))
		 ))
	  )

(setq org-agenda-compact-blocks t)
(setq org-agenda-custom-commands `(,rpc/org-agenda-main
								   ,rpc/org-agenda-people
								   ,rpc/org-agenda-workflow
								   ,rpc/org-agenda-discussions
								   ,rpc/org-agenda-refile
								   ,rpc/org-agenda-archive))

(defun rpc/skip-non-archivable ()
  "Skip trees that are not archivable tasks or projects."
  (save-restriction
	(widen)
	(if (member (org-get-todo-state) '("DONE" "CANCELED"))
	  (save-excursion (or (outline-next-heading) (point-max)))
		nil
	  )
	)
  )


(defun rpc/skip-non-projects ()
  "Skip trees that are not projects."
  (if (save-excursion (rpc/skip-non-stuck-projects))
      (save-restriction
	(widen)
	(let ((subtree-end (save-excursion (org-end-of-subtree t))))
	  (cond
	   ((rpc/is-project-p)
	    nil)
	   ((and (rpc/is-project-subtree-p) (not (rpc/is-task-p)))
	    nil)
	   (t
	    subtree-end))))
    (save-excursion (org-end-of-subtree t))))

(defun rpc/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (rpc/is-project-p)
	  (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
		 (has-next))
	    (save-excursion
	      (forward-line 1)
	      (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
		(unless (member "WAITING" (org-get-tags-at))
		  (setq has-next t))))
	    (if has-next
		next-headline
	      nil))
	next-headline))))

(defun rpc/is-project-p ()
  "Any task with a todo keyword subtask is a project."
  (save-restriction
    (widen)
    (let ((has-subtask)
	  (subtree-end (save-excursion (org-end-of-subtree t)))
	  (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
	(forward-line 1)
	(while (and (not has-subtask)
		    (< (point) subtree-end)
		    (re-search-forward "^\*+" subtree-end t))
	  (when (member (org-get-todo-state) org-todo-keywords-1)
	    (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun rpc/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (rpc/find-project-task)
      (if (equal (point) task)
          nil
        t))))

(defun rpc/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun rpc/is-task-p ()
  "Any task with a todo keyword and no subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun rpc/skip-projects-and-single-tasks ()
  "Skip trees that are projects and single non-project tasks"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ;((org-is-habit-p)
       ; next-headline)
       ;((and rpc/hide-scheduled-and-waiting-next-tasks
       ;      (member "WAITING" (org-get-tags-at)))
       ; next-headline)
       ((rpc/is-project-p)
        next-headline)
       ((and (rpc/is-task-p) (not (rpc/is-project-subtree-p)))
        next-headline)
       (t
        nil)))))

(defun rpc/skip-non-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((rpc/is-task-p)
        nil)
       (t
        next-headline)))))

(defun rpc/open-org-file ()
  "Open my primary org file."
  (interactive)
  (find-file "~/org/gtd.org"))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (sh . t)
   (sqlite . t)
   (ledger . t)
   (emacs-lisp . t)))

(defun rpc/org-narrow-to-headline ()
  (interactive)
  (rpc/open-org-file)
  (widen)
  (org-goto 'outline-path-completion)
  (org-narrow-to-subtree)
  (org-set-startup-visibility)
  (org-cycle))

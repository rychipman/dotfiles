(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(setq-default org-hide-leading-stars t)

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
	("n" "Note"
	 entry (file+headline "~/org/inbox.org" "Notes")
	 "* %? :NOTE:"
	 :empty-lines 1
	 :prepend t)
	("l" "Task with link"
	 entry (file+headline "~/org/inbox.org" "Tasks")
	 "* TODO %?\n  %A"
	 :empty-lines 1
	 :prepend t)
	("t" "Task"
	entry (file+headline "~/org/inbox.org" "Tasks")
	"* TODO %?"
	:empty-lines 1
	:prepend t)))

(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-refile-targets '((nil :maxlevel . 9)
			   (org-agenda-files :maxlevel . 9)))

(defun rpc/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets."
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'rpc/verify-refile-target)

(add-hook 'org-capture-mode-hook 'evil-insert-state)

(require 'org-agenda)

(define-key org-agenda-mode-map "j" 'org-agenda-next-item)
(define-key org-agenda-mode-map "k" 'org-agenda-previous-item)

(setq org-agenda-files (list "~/org/inbox.org"
			    "~/org/tickler.org"
			    "~/org/gtd.org"))

(setq org-agenda-compact-blocks t)
(setq org-agenda-custom-commands
      (quote (("N" "Notes" tags "NOTE"
	       ((org-agenda-overriding-header "Notes")))
	      (" " "Agenda"
	       (;(agenda "" nil)
		(tags "REFILE"
		      ((org-agenda-overriding-header "Tasks to Refile")))
		(tags-todo "-CANCELED/!"
			   ((org-agenda-overriding-header "Stuck Projects")
			    (org-agenda-skip-function 'rpc/skip-non-stuck-projects)))
		(tags-todo "-CANCELED+WAITING|HOLD/!"
			   ((org-agenda-overriding-header "Waiting and Postponed Tasks")
			    (org-agenda-skip-function 'rpc/skip-non-tasks)
			    (org-tags-match-list-sublevels nil)))
		(tags-todo "-CANCELED/!NEXT"
			   ((org-agenda-overriding-header "Project Next Tasks")
			    (org-agenda-skip-function 'rpc/skip-projects-and-single-tasks)))
		(tags-todo "-HOLD-CANCELED/!"
			   ((org-agenda-overriding-header "Projects")
			    (org-agenda-skip-function 'rpc/skip-non-projects)))
		))
	      ("w" "Work Agenda"
	       (;(agenda "" nil)
		(tags-todo "+work-CANCELED/!"
			   ((org-agenda-overriding-header "Stuck Projects")
			    (org-agenda-skip-function 'rpc/skip-non-stuck-projects)))
		(tags-todo "+work-CANCELED+WAITING|HOLD/!"
			   ((org-agenda-overriding-header "Waiting and Postponed Tasks")
			    (org-agenda-skip-function 'rpc/skip-non-tasks)
			    (org-tags-match-list-sublevels nil)))
		(tags-todo "+work-interns-CANCELED/!NEXT"
			   ((org-agenda-overriding-header "Project Next Tasks")
			    (org-agenda-skip-function 'rpc/skip-projects-and-single-tasks)))
		(tags-todo "+work-interns-HOLD-CANCELED/!"
			   ((org-agenda-overriding-header "Projects")
			    (org-agenda-skip-function 'rpc/skip-non-projects)))
		(tags-todo "+work+interns-CANCELED/!NEXT"
			   ((org-agenda-overriding-header "Project Next Tasks (intern-related)")
			    (org-agenda-skip-function 'rpc/skip-projects-and-single-tasks)))
		(tags-todo "+work+interns-HOLD-CANCELED/!"
			   ((org-agenda-overriding-header "Projects (intern-related)")
			    (org-agenda-skip-function 'rpc/skip-non-projects)))
		))
	      )))

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

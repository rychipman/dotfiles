(setq ledger-reconcile-insert-effective-date nil)

(define-key ledger-mode-map (kbd "C-k") 'ledger-navigate-prev-xact-or-directive)
(define-key ledger-mode-map (kbd "C-j") 'ledger-navigate-next-xact-or-directive)

(defun rpc/ledger-delete-extra-whitespace ()
  (interactive)

  (goto-char (point-min))
  (while (string= "\n" (thing-at-point 'line t))
	(delete-blank-lines)
	(goto-char (point-min)))

  (goto-char (point-max))
  (while (string= "\n" (thing-at-point 'line t))
	(delete-blank-lines)
	(goto-char (point-max)))

  (goto-char (point-min))
  (while (search-forward "\n\n" nil t)
	(delete-blank-lines))

  (save-buffer))

(defun rpc/ledger-format-and-save ()
  (interactive)
  (save-excursion
	(ledger-sort-buffer)
	(rpc/ledger-delete-extra-whitespace)
	(save-buffer)))

(load-file "~/.emacs.d/ledger/ledger-xact.el")
(load-file "~/.emacs.d/ledger/ledger-jump.el")
(load-file "~/.emacs.d/ledger/ledger-import.el")
(load-file "~/.emacs.d/ledger/ledger-add.el")

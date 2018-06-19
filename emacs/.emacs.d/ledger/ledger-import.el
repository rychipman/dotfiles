
(defun rpc/ledger-create-amount-matcher (xact)
  (let* ((amts (rpc/ledger-xact-amts xact))
		 (amt  (car amts)))
	(lexical-let ((amt amt))
	  (lambda (xact) (rpc/ledger-xact-has-amt amt xact)))))

(defun rpc/ledger-match-xact (xact)
  (rpc/ledger-set-jumplist-filter (rpc/ledger-create-amount-matcher xact))
  (ledger-jumplist-first)
  (hydra-ledger-match/body))

(defun rpc/ledger-find-xact-at-point ()
  (save-excursion
	(ledger-navigate-beginning-of-xact)
	(seq-find
	 '(lambda (xact) (= (line-number-at-pos) (rpc/ledger-xact-line xact)))
	 ledger-import-xacts)))

(defun rpc/ledger-try-match-xact-at-point ()
  (interactive)
  (let ((xact (rpc/ledger-find-xact-at-point)))
	(cond
	 ((rpc/ledger-has-match (rpc/ledger-create-amount-matcher xact))
	  (find-file-other-window "~/ledger/test.ledger")
	  (rpc/ledger-match-xact xact))
	 (t (rpc/ledger-prompt-add-xact xact)))))

(defun rpc/ledger-prompt-add-xact (xact)
  (when (y-or-n-p "No match found; add xact? ")
	(let* ((extents (ledger-navigate-find-xact-extents (point)))
		   (begin (car extents))
		   (end (cadr extents)))
	  (kill-region begin end)
	  (find-file-other-window "~/ledger/test.ledger")
	  (goto-char (point-max))
	  (newline)
	  (yank))))

;; TODO: save import and test buffer if not saved
(defun rpc/ledger-match-imports ()
  (interactive)
  (rpc/ledger-xact-load)
  (delete-other-windows)
  (find-file "~/ledger/import.ledger")
  (hydra-ledger-match/body))

(defhydra hydra-ledger-match ()
  "ledger-match"
  ("j" ledger-navigate-next-xact-or-directive)
  ("k" ledger-navigate-prev-xact-or-directive)
  ("RET" rpc/ledger-try-match-xact-at-point :exit t))

(defhydra hydra-ledger-match-jump ()
  "ledger-match-jump"
  ("j" ledger-jumplist-next)
  ("k" ledger-jumplist-prev)
  ("RET" rpc/ledger-match-choose :exit t))

(defun rpc/ledger-match-choose ()
  (interactive)
  (ledger-navigate-beginning-of-xact)
  (end-of-line)
  (newline-and-indent)
  (insert ":chosen:")
  (delete-window))

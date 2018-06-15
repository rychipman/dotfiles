(defhydra hydra-ledger-jump ()
  "ledger-jump"
  ("j" ledger-jumplist-next)
  ("n" ledger-jumplist-next)
  ("k" ledger-jumplist-prev)
  ("p" ledger-jumplist-prev)
  ("g" ledger-jumplist-first)
  ("G" ledger-jumplist-last))

(defvar ledger-xacts nil
  "The current xact filtering function.")

(defvar ledger-xact-jumplist nil
  "The current xact filtering function.")

(defvar ledger-jumplist-filter nil
  "The current xact filtering function.")

(defun rpc/ledger-set-jumplist-filter (filter)
  "TODO document this FILTER."
  (setq ledger-xact-jumplist nil
		ledger-jumplist-filter filter)
  (rpc/ledger-populate-jumplist))

(defun rpc/ledger-populate-jumplist ()
  "Populate 'ledger-xact-jumplist with the elements of 'ledger-xacts that match 'ledger-jumplist-filter."
  (unless ledger-xacts
    (error "No transactions found in ledger-xacts"))
  (let ((filtered (seq-filter ledger-jumplist-filter ledger-xacts)))
    (unless filtered
      (warn "no xacts match jumplist filter"))
    (setq ledger-xact-jumplist filtered)))

(defun rpc/ledger-jump-to-xact (xact)
  (unless xact
	(error "Cannot jump to nil xact"))
  (goto-char (point-min))
  (forward-line
   (1- (rpc/ledger-xact-line xact))))

(defun ledger-jump-uncleared ()
  (interactive)
  (ledger-jump-start 'rpc/ledger-xact-uncleared))

(defun ledger-jump-start (filter)
  "Enter the ledger-jump hydra with the provided FILTER."
  (rpc/ledger-set-jumplist-filter filter)
  (ledger-jumplist-first)
  (hydra-ledger-jump/body))

(defun ledger-jumplist-first ()
  "Jump to the first line number in 'ledger-xact-jumplist."
  (interactive)
  (unless ledger-xact-jumplist
	(error "No xacts in jumplist"))
  (rpc/ledger-jump-to-xact (car ledger-xact-jumplist)))

(defun ledger-jumplist-last ()
  "Jump to the last line number in 'ledger-xact-jumplist."
  (interactive)
  (unless ledger-xact-jumplist
	(error "No xacts in jumplist"))
  (rpc/ledger-jump-to-xact (car (last ledger-xact-jumplist))))

(defun after-current-line (linum)
  "Return true if LINUM is greater than the current line number."
  (> linum (line-number-at-pos)))

(defun ledger-jumplist-next ()
  "Jump to the first line number in 'ledger-xact-jumplist that is greater than the current line's number."
  (interactive)
  (unless ledger-xact-jumplist
	(error "No xacts in jumplist"))
  (let ((xact (seq-find
			   '(lambda (xact) (after-current-line (rpc/ledger-xact-line xact)))
			   ledger-xact-jumplist)))
	(unless xact
	  (error "No xacts in jumplist after current line"))
	(rpc/ledger-jump-to-xact xact)))

(defun before-current-line (linum)
  "Return true if LINUM is less than the current line number."
  (< linum (line-number-at-pos)))

(defun ledger-jumplist-prev ()
  "Jump to the first line number in 'ledger-xact-jumplist that is less than the current line's number."
  (interactive)
  (unless ledger-xact-jumplist
	(error "No xacts in jumplist"))
  (let ((xact (seq-find
			   '(lambda (xact) (before-current-line (rpc/ledger-xact-line xact)))
			   ledger-xact-jumplist)))
	(unless xact
	  (error "No xacts in jumplist before current line"))
	(rpc/ledger-jump-to-xact xact)))

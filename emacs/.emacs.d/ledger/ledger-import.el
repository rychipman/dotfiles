(require 'cl)

(defun rpc/ledger-create-amount-matcher (xact)
  (let* ((amts (rpc/ledger-xact-amts xact))
		 (amt  (car amts)))
	(lexical-let ((amt amt))
	  (lambda (xact) (rpc/ledger-xact-has-amt amt xact)))))

(defun rpc/ledger-match-xact (xact)
  (rpc/ledger-set-jumplist-filter (rpc/ledger-create-amount-matcher xact))
  (ledger-jumplist-first)
  (recenter)
  (hydra-ledger-match-jump/body))

(defun rpc/ledger-find-xact-at-point (&optional xacts)
  (unless xacts
	(setq xacts ledger-import-xacts))
  (save-excursion
	(ledger-navigate-beginning-of-xact)
	(seq-find
	 '(lambda (xact) (= (line-number-at-pos) (rpc/ledger-xact-line xact)))
	 xacts)))

(defun rpc/ledger-try-match-xact-at-point ()
  (interactive)
  (let ((xact (rpc/ledger-find-xact-at-point)))
	(cond
	 ((rpc/ledger-has-match (rpc/ledger-create-amount-matcher xact))
	  (find-file-other-window "~/ledger/test.ledger")
	  (setq ledger-match-candidate xact)
	  (rpc/ledger-match-xact xact))
	 (t (message "No match found for xact")
		(hydra-ledger-match/body)))))

(defun rpc/ledger-import-add-xact ()
  (interactive)
  (let (xact)
    (setq xact (rpc/ledger-find-xact-at-point ledger-import-xacts))
	(find-file-other-window "~/ledger/test.ledger")
	(goto-char (point-max))
	(newline) (newline)
	(insert (rpc/ledger-add-imported xact)))
  (rpc/ledger-format-and-save)
  (rpc/ledger-xact-load)
  (find-file-other-window "~/ledger/import.ledger")
  (rpc/ledger-try-match-xact-at-point))

;; TODO: save import and test buffer if not saved
(defun rpc/ledger-match-imports ()
  (interactive)
  (rpc/ledger-xact-load)
  (delete-other-windows)
  (find-file "~/ledger/import.ledger")
  (hydra-ledger-match/body))

(defhydra hydra-ledger-match
  (:foreign-keys warn)
  "ledger-match"
  ("j" ledger-navigate-next-xact-or-directive)
  ("n" ledger-navigate-next-xact-or-directive)
  ("k" ledger-navigate-prev-xact-or-directive)
  ("p" ledger-navigate-prev-xact-or-directive)
  ("g" (lambda ()
		 (interactive)
		 (goto-char (point-min))
		 (ledger-navigate-next-xact)
		 (ledger-navigate-prev-xact-or-directive)))
  ("G" (lambda ()
		 (interactive)
		 (goto-char (point-max))
		 (ledger-navigate-prev-xact-or-directive)
		 (ledger-navigate-next-xact)))
  ("q" nil :exit t)
  ("a" rpc/ledger-import-add-xact :exit t)
  ("RET" rpc/ledger-try-match-xact-at-point :exit t))

(defhydra hydra-ledger-match-jump
  (:foreign-keys warn)
  "ledger-match-jump"
  ("j" ledger-jumplist-next)
  ("k" ledger-jumplist-prev)
  ("q" rpc/ledger-match-jump-quit :exit t)
  ("RET" rpc/ledger-match-jump-choose :exit t))

(defun rpc/ledger-match-jump-quit ()
  (interactive)
  (select-window (next-window))
  (setq ledger-match-candidate nil)
  (hydra-ledger-match/body))

(defun rpc/ledger-match-jump-choose ()
  (interactive)
  (save-excursion (rpc/ledger-import-match-add-tags))
  (select-window (next-window))
  (ledger-delete-current-transaction (point))
  (rpc/ledger-format-and-save)
  (rpc/ledger-xact-load)
  (setq ledger-match-candidate nil)
  (hydra-ledger-match/body))

(defun rpc/ledger-import-match-add-tags ()
  (let (xact postings posting)
	(ledger-navigate-beginning-of-xact)
	(setq xact (rpc/ledger-find-xact-at-point ledger-xacts))
	(setq postings
		  (rpc/ledger-xact-filter-postings
		   '(lambda (post) (string= (rpc/ledger-post-acct post)
									(rpc/ledger-import-get-account)))
		   xact))
	(unless (= 1 (length postings))
	  (error "Expected one posting to match acct '%s', found %s"
			 (rpc/ledger-import-get-account)
			 (length postings)))
	(setq posting (car postings))
	(goto-char (point-min))
	(forward-line (1- (rpc/ledger-post-line posting)))
	(end-of-line)
	(insert "  ; :matched:")
	(newline)
	(insert "    ;" (rpc/ledger-import-get-tag)))
  (rpc/ledger-format-and-save))

(defun rpc/ledger-import-get-account ()
  (rpc/ledger-post-acct (rpc/ledger-get-import-posting)))

(defun rpc/ledger-import-get-tag ()
  (rpc/ledger-post-ofxid-tag (rpc/ledger-get-import-posting)))

(defun rpc/ledger-get-import-posting ()
  (let (filtered matches)
	(setq filtered (rpc/ledger-xact-filter-postings
					'rpc/ledger-post-has-ofxid-tag
					ledger-match-candidate))
	(setq matches (length filtered))
	(unless (= 1 matches)
	  (error "Expected 1 match, got %s" matches))
	(car filtered)))

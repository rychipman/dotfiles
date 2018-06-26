(defun rpc/ledger-prompt-payee (&optional payee)
  (completing-read
   (if payee
	   (format "Payee (%s): " payee)
	   "Payee: ")
   (ledger-payees-in-buffer)
   nil 'confirm))

(defun rpc/ledger-prompt-from-account ()
  (completing-read
   "From Account: "
   (ledger-accounts-list-in-buffer)
   nil t "liabilities "))

(defun rpc/ledger-prompt-to-account ()
  (completing-read
   "To Account: "
   (ledger-accounts-list-in-buffer)
   nil t "expenses "))

(defun rpc/ledger-edit-interactive ()
  (interactive)
  (save-excursion
	()
	)
  )

(defun rpc/ledger-add-interactive ()
  (interactive)
  (let (date payee desc from to xact)

	(setq date (ledger-read-date "Date: "))
	(setq payee (rpc/ledger-prompt-payee))
	(setq desc (ledger-read-string-with-default "Description" ""))
	(setq from (rpc/ledger-prompt-from-account))
	(setq to (rpc/ledger-prompt-to-account))

	(setq xact (format "%s %s" date payee))
	(unless (string= "" desc)
	  (setq xact (format "%s\n    ;; %s" xact desc)))
	(setq xact (format "%s\n    %s\n    %s" xact to from))

	(goto-char (point-max))
	(newline) (newline)
	(insert xact)
	(ledger-sort-buffer)
	(rpc/ledger-format-and-save)))

(defun rpc/ledger-add-imported (xact)
  (let (date payee desc amt to import-acct import-amt-inverse xact-str)

	(setq import-acct (rpc/ledger-get-import-acct xact))
	(setq import-amt-inverse (rpc/ledger-get-import-amt-inverse xact))

	(setq date (rpc/ledger-xact-date xact))
	(setq payee (rpc/ledger-prompt-payee (rpc/ledger-xact-payee xact)))
	(setq desc (ledger-read-string-with-default "Description" ""))
	(setq to (rpc/ledger-prompt-to-account))

	(setq xact-str (format "%s %s" date payee))
	(unless (string= "" desc)
	  (setq xact-str (format "%s\n    ;; %s" xact-str desc)))
	(setq xact-str (format "%s\n    ; imported" xact-str))
	(setq xact-str (format "%s\n    %s  %s" xact-str to import-amt-inverse))
	(setq xact-str (format "%s\n    %s" xact-str import-acct))

	xact-str))

(defun rpc/ledger-get-import-acct (xact)
  (let (postings post)
	(setq postings (rpc/ledger-xact-postings xact))
	(setq post (car postings))
	(rpc/ledger-post-acct post)))

(defun rpc/ledger-get-import-amt (xact)
  (let (postings post)
	(setq postings (rpc/ledger-xact-postings xact))
	(setq post (car postings))
	(rpc/ledger-post-amt post)))

(defun rpc/ledger-get-import-amt-inverse (xact)
  (let (postings post)
	(setq postings (rpc/ledger-xact-postings xact))
	(setq post (cadr postings))
	(rpc/ledger-post-amt post)))

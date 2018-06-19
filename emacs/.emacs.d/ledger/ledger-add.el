(defun rpc/ledger-prompt-payee ()
  (completing-read
   "Payee: "
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
	(save-buffer)))

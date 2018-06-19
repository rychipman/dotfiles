(defun rpc/ledger-xact-load ()
  (interactive)
  (setq ledger-xacts (read (shell-command-to-string "ledger emacs -f ~/ledger/test.ledger"))
		ledger-import-xacts (read (shell-command-to-string "ledger emacs -f ~/ledger/import.ledger"))))

(defun rpc/ledger-xact-file (xact)
  (car xact))

(defun rpc/ledger-xact-line (xact)
  (cadr xact))

(defun rpc/ledger-xact-payee (xact)
  (nth 4 xact))

(defun rpc/ledger-xact-postings (xact)
  (nthcdr 5 xact))

(defun rpc/ledger-xact-amts (xact)
  (mapcar 'rpc/ledger-post-amt (rpc/ledger-xact-postings xact)))

(defun rpc/ledger-xact-cleared (xact)
  (seq-every-p
   'rpc/ledger-post-cleared
   (rpc/ledger-xact-postings xact)))

(defun rpc/ledger-xact-uncleared (xact)
  (not (rpc/ledger-xact-cleared xact)))

(defun rpc/ledger-xact-filter-postings (filter xact)
  (seq-filter
   filter
   (rpc/ledger-xact-postings xact)))

(defun rpc/ledger-xact-map-postings (mapfunc xact)
  (seq-map
   mapfunc
   (rpc/ledger-xact-postings xact)))

(defun rpc/ledger-xact-has-amt (amt xact)
  (not (= 0
		 (seq-count
		  '(lambda (post) (string-equal amt (rpc/ledger-post-amt post)))
		 (rpc/ledger-xact-postings xact)))))

(defun rpc/ledger-post-line (post)
  (car post))

(defun rpc/ledger-post-acct (post)
  (-second-item post))

(defun rpc/ledger-post-amt (post)
  (-third-item post))

(defun rpc/ledger-post-cleared (post)
  (-fourth-item post))


(setq ledger-reconcile-insert-effective-date nil)

(define-key ledger-mode-map (kbd "C-k") 'ledger-navigate-prev-xact-or-directive)
(define-key ledger-mode-map (kbd "C-j") 'ledger-navigate-next-xact-or-directive)

(load-file "~/.emacs.d/ledger/ledger-xact.el")
(load-file "~/.emacs.d/ledger/ledger-jump.el")

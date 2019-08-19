;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq mac-command-modifier 'control)
(setq delete-old-versions -1 )		; delete excess backup versions silently
(setq version-control t )		; use version control
(setq vc-make-backup-files t )		; make backups file even when in version controlled dir
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")) ) ; which directory to put backups file
(setq vc-follow-symlinks t )				       ; don't ask for confirmation when opening symlinked file
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) ) ;transform backups file name
(setq inhibit-startup-screen t)         ; inhibit useless and old-school startup screen
(setq ring-bell-function 'ignore)	; silent bell when you make a mistake
(setq coding-system-for-read 'utf-8)	; use utf-8 by default
(setq coding-system-for-write 'utf-8)
(setq sentence-end-double-space nil)	; sentence SHOULD end with only a point.
(setq default-fill-column 80)		; toggle wrapping text at the 80th character
(setq initial-scratch-message "Welcome to Emacs") ; print a default message in the empty scratch buffer opened at startup
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(blink-cursor-mode 0)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(global-linum-mode 1)
(global-hl-line-mode 0)

(global-set-key (kbd "C-j") 'xref-find-definitions)

(setq-default tab-width 4)
(setq-default truncate-lines 0)
(setq-default help-window-select t)

(defvar plaint-download-mode-map (make-sparse-keymap)
  "Keymap for plaint-download-mode.")

(defun plaint-download-quit ()
  (interactive)
  (when (equal "*plaint-download*" (buffer-name))
	(kill-buffer)))

(define-key plaint-download-mode-map (kbd "q") 'plaint-download-quit)
(define-key plaint-download-mode-map (kbd "f") (lambda () (interactive) (find-file "~/ledger/import.ledger")))

(define-minor-mode plaint-download-mode
  "A minor mode for the *plaint-download* buffer."
  nil
  :lighter plaint
  plaint-download-mode-map)

(defun plaint-download ()
  (interactive)
  (pop-to-buffer "*plaint-download*")
  (async-shell-command "cd ~/ledger && plaint download" (current-buffer) (current-buffer))
  (evil-emacs-state)
  (plaint-download-mode))

(defun zoom-frame-monitor ()
 "Zoom the current frame to an appropriate size for my thinkvision monitor."
 (interactive)
 (set-face-attribute 'default (selected-frame) :height 150))

(defun zoom-frame-laptop ()
 "Zoom the current frame to an appropriate size for my laptop screen."
 (interactive)
 (set-face-attribute 'default (selected-frame) :height 110))

(defun zoom-frame-in ()
 "Zoom in the current frame."
 (interactive)
 (set-face-attribute 'default (selected-frame)
   :height (+ 10 (face-attribute 'default :height))))

(defun zoom-frame-out ()
 "Zoom out the current frame."
 (interactive)
 (set-face-attribute 'default (selected-frame)
   :height (- (face-attribute 'default :height) 10)))

(setq-default whitespace-style '(face spaces tabs tab-mark trailing))

(defun rpc/prevent-whitespace-mode-for-magit ()
  (not (derived-mode-p 'magit-mode)))

(with-eval-after-load 'whitespace
  (add-function :before-while whitespace-enable-predicate 'rpc/prevent-whitespace-mode-for-magit))

(defun rpc/set-path-from-env (env-var-name)
  (let* ((env-echo-cmd (format ". ~/.bashrc; echo -n $%s" env-var-name))
		 (env-var-value (shell-command-to-string env-echo-cmd)))
    (setenv env-var-name env-var-value)
    (when (string= env-var-name "PATH")
    	(setq exec-path
    		  (append (split-string-and-unquote env-var-value ":")
    				  exec-path)))))

(rpc/set-path-from-env "PATH")
(rpc/set-path-from-env "GOPATH")
(rpc/set-path-from-env "PKG_CONFIG_PATH")

(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "/usr/local/Cellar/mu/1.0/share/emacs/site-lisp/mu/mu4e/")

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'umber t)

(require 'use-package)

(use-package swiper
  :ensure t)

(use-package counsel
  :ensure t)

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-initial-inputs-alist nil)
  (define-key ivy-minibuffer-map (kbd "C-<return>") 'ivy-immediate-done))

(use-package general
  :ensure t
  :config
  (general-define-key

   :states '(normal visual emacs)
   :prefix "SPC"

   ;; file
   "f" '(:ignore t :which-key "file")
   "fe" '(:ignore t :which-key "emacs")
   "fee" (lambda () "edit" (interactive) (find-file "~/.emacs.d/init.el"))
   "feo" (lambda () "edit" (interactive) (find-file "~/org/lisp/org.el"))
   "fer" (lambda () "reload" (interactive) (load-file user-init-file))

   ;; buffer
   "b" '(:ignore t :which-key "file")
   "bb" 'counsel-ibuffer
   "bd" 'evil-delete-buffer

   ;; git
   "g" '(:ignore t :which-key "git")
   "gs" 'magit-status
   "gf" 'magit-file-popup
   "gm" 'magit-dispatch-popup
   "gb" 'magit-blame
   "gg" 'hydra-gitgutter/body
   "gt" 'git-timemachine
   "gl" 'git-link

   ;; errors
   "e" '(:ignore t :which-key "errors")
   "en" 'next-error
   "ep" 'previous-error

   ;; org
   "o" '(:ignore t :which-key "org")
   "oa" 'org-agenda
   "oc" 'org-capture
   "ol" 'org-store-link
   "or" 'org-refile
   "oA" 'org-archive
   "of" 'rpc/open-org-file
   "oh" 'rpc/org-narrow-to-headline
   "os" 'org-save-all-org-buffers
   "ot" 'rpc/org-clock-in

   ;; window
   "w" '(:ignore t :which-key "window")
   "wd" 'evil-window-delete
   "ww" 'eyebrowse-next-window-config
   "wW" 'eyebrowse-create-window-config
   "wR" 'eyebrowse-rename-window-config
   "wD" 'eyebrowse-close-window-config
   "wf" 'eyebrowse-switch-to-window-config

   ;; dired
   "d" '(:ignore t :which-key "dired")
   "dd" 'dired-jump-other-window

   ;; projectile
   "p" '(:ignore t :which-key "projectile")
   "pp" 'counsel-projectile-switch-project
   "pf" 'counsel-projectile-find-file
   "pd" 'projectile-dired-other-window
   "pD" 'counsel-projectile-find-dir
   "ps" 'counsel-projectile-rg
   "pb" 'counsel-projectile-switch-to-buffer
   "pu" 'projectile-discover-projects-in-search-path

   ;; ledger
   "l" '(:ignore t :which-key "ledger")
   "lf" (lambda () "file" (interactive) (find-file "~/ledger/test.ledger"))
   "ld" 'plaint-download
   "lr" 'ledger-report
   "lc" 'ledger-check-buffer
   "li" 'rpc/ledger-match-imports
   "la" 'rpc/ledger-add-interactive

   ;; ivy
   "i" '(:ignore t :which-key "ivy")
   "ir" 'ivy-resume

   ;; zoom
   "z" '(:ignore t :which-key "zoom")
   "zm" (lambda () "monitor" (interactive) (zoom-frame-monitor))
   "zl" (lambda () "laptop" (interactive) (zoom-frame-laptop))
   "zz" 'hydra-zoom/body

   ;; compile
   "c" '(:ignore t :which-key "compile")
   "cb" 'rpc/compile/build
   "cc" 'rpc/compile/check
   "ct" 'rpc/compile/unit-test
   "cC" 'compile
   "ck" 'rpc/compile-quit-windows

   ;; applications
   "a" '(:ignore t :which-key "applications")
   "am" 'rpc/mu4e/open

   ;; misc
   "x" 'counsel-M-x
   ))

(use-package avy
  :ensure t
  :commands (avy-goto-word-1))

(use-package which-key
  :ensure t
  :config
  (which-key-mode 1))

(use-package evil
  :ensure t
  :config
  (evil-mode 1)

  (evil-set-initial-state 'ledger-reconcile-mode 'emacs)
  (evil-set-initial-state 'ledger-check-mode 'emacs)
  (evil-set-initial-state 'special-mode 'emacs)

  (define-key evil-visual-state-map (kbd "v") 'exchange-point-and-mark)

  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
  (add-hook 'with-editor-mode-hook 'evil-insert-state)

  (define-key evil-visual-state-map (kbd ">") 'rpc/evil-shift-right-visual)
  (define-key evil-visual-state-map (kbd "<") 'rpc/evil-shift-left-visual)

  (define-key evil-normal-state-map (kbd "/") 'swiper)
  (define-key evil-normal-state-map (kbd "?") 'swiper)

  (defun rpc/evil-shift-right-visual ()
	(interactive)
	(evil-shift-right (region-beginning) (region-end))
	(evil-normal-state)
	(evil-visual-restore))

  (defun rpc/evil-shift-left-visual ()
	(interactive)
	(evil-shift-left (region-beginning) (region-end))
	(evil-normal-state)
	(evil-visual-restore))

  (use-package evil-god-state
	:ensure t
	:config
	(evil-define-key 'normal global-map "," 'evil-execute-in-god-state)
	(evil-define-key 'god global-map [escape] 'evil-god-state-bail))

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode 1)))

(use-package crux
  :ensure t)

(use-package expand-region
  :ensure t)

(use-package git-link
  :ensure t)

(use-package git-timemachine
  :ensure t)

(use-package magit
  :ensure t
  :config

  (use-package evil-magit
    :ensure t
	:config
	;(define-key git-rebase-mode-map (kbd "J") 'git-rebase-move-line-down)
	;(define-key git-rebase-mode-map (kbd "K") 'git-rebase-move-line-up)
	)

  ;(use-package forge
  ;	:ensure t)
  )

(use-package org
  :ensure t
  :config
  (load-file "~/org/lisp/org.el"))

(use-package eyebrowse
  :ensure t
  :init
  (setq eyebrowse-keymap-prefix (kbd "C-c C-S-w"))
  :config
  (setq-default eyebrowse-new-workspace t)
  (setq-default eyebrowse-wrap-around t)
  (eyebrowse-mode t))

(use-package graphviz-dot-mode
  :ensure t)

(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook ((go-mode . lsp)
		 (lsp-mode .lsp-ui)
		 (before-save . lsp-format-buffer))
  :custom
  (lsp-ui-peek-enable nil))

(use-package dap-mode
  :ensure t)

(use-package company-lsp
  :ensure t)

(use-package go-mode
  :ensure t)

(use-package ledger-mode
  :ensure t
  :config
  (load-file "~/.emacs.d/ledger/ledger.el")
  (define-key ledger-reconcile-mode-map (kbd "m") 'ledger-reconcile-toggle)
  (define-key ledger-mode-map (kbd "C-j") 'ledger-navigate-next-xact-or-directive)
  (define-key ledger-mode-map (kbd "C-k") 'ledger-navigate-prev-xact-or-directive))

(defun rpc/compile-quit-windows ()
  (interactive)
  (rpc/kill-window-by-buffer-name "*compilation*")
  (rpc/kill-window-by-buffer-name "*compile/build*")
  (rpc/kill-window-by-buffer-name "*compile/check*")
  (rpc/kill-window-by-buffer-name "*compile/unit-test*"))

(defun rpc/kill-window-by-buffer-name (bufname &optional killbuf)
  (let ((window (get-buffer-window bufname)))
	(when window (quit-window killbuf window))))

(defvar rpc/compile/build-command nil)
(defun rpc/compile/build ()
  (interactive)
  (let ((compilation-buffer-name-function (lambda (mode) "*compile/build*")))
    (if rpc/compile/build-command
	    (compile rpc/compile/build-command)
	  (call-interactively 'compile))))

(defvar rpc/compile/check-command nil)
(defun rpc/compile/check ()
  (interactive)
  (let ((compilation-buffer-name-function (lambda (mode) "*compile/check*")))
    (if rpc/compile/check-command
	    (compile rpc/compile/check-command)
	  (call-interactively 'compile))))

(defvar rpc/compile/unit-test-command nil)
(defun rpc/compile/unit-test ()
  (interactive)
  (let ((compilation-buffer-name-function (lambda (mode) "*compile/unit-test*")))
    (if rpc/compile/unit-test-command
	    (compile rpc/compile/unit-test-command)
	  (call-interactively 'compile))))

(defun remove-nth-element (list nth)
  "Return a copy of LIST without its NTH element."
  (if (zerop nth) (cdr list)
    (let ((last (nthcdr (1- nth) list)))
      (setcdr last (cddr last))
      list)))

(use-package mu4e
  :commands (mu4e rpc/mu4e/open rpc/mu4e/quit)
  :config

  ;; general settings
  (require 'org-mu4e)
  (setq
   org-mu4e-link-query-in-headers-mode t
   mu4e-maildir "~/mail"
   mu4e-mu-binary "/usr/bin/mu"
   mu4e-get-mail-command "mbsync -Va"
   mu4e-change-filenames-when-moving t
   mu4e-completing-read-function 'completing-read
   mu4e-compose-dont-reply-to-self t
   shr-color-visible-luminance-min 80
   send-mail-function 'smtpmail-send-it
   smtpmail-smtp-service 465
   smtpmail-stream-type 'ssl
   mu4e-hide-index-messages t
   mu4e-compose-format-flowed t
   message-kill-buffer-on-exit t
   mu4e-confirm-quit nil)

  (setq mu4e-user-mail-address-list '("ryan@mongodb.com"
									  "ryan.chipman@mongodb.com"
									  "ryan@10gen.com"
									  "ryan.chipman@10gen.com"
									  "ryan@ryanchipman.com"))

  (add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)

  (defun rpc/mu4e/open ()
	(interactive)
	(eyebrowse-create-window-config)
	(mu4e))

  (defun rpc/mu4e/quit ()
	(interactive)
	(mu4e-quit)
	(eyebrowse-close-window-config))

  (define-key mu4e-main-mode-map (kbd "q") 'rpc/mu4e/quit)

  ;; set up bookmarks
  (setq mu4e-bookmarks
    `(
      ,(make-mu4e-bookmark
	:name "Unread messages"
	:query "flag:unread AND NOT maildir:/mongodb/Trash AND NOT maildir:/personal/Trash"
	:key ?u)

      ,(make-mu4e-bookmark
	:name "Inbox messages"
	:query "maildir:/mongodb/INBOX OR maildir:/personal/INBOX"
	:key ?i)
      ))

  ;; set up contexts
  (setq mu4e-context-policy 'pick-first
	mu4e-compose-context-policy nil
	mu4e-contexts
	`(
	   ,(make-mu4e-context
	     :name "mongodb"
	     :enter-func (lambda () (mu4e-message "entering 'mongodb' context"))
	     :leave-func (lambda () (mu4e-message "leaving 'mongodb' context"))
	     :match-func (lambda (msg)
			   (when msg
			     (string-prefix-p "/mongodb" (mu4e-message-field msg :maildir))))
	     :vars '( ( user-mail-address  . "ryan@mongodb.com" )
		      ( user-full-name     . "Ryan Chipman" )
		      ( mu4e-drafts-folder . "/mongodb/Drafts" )
		      ( mu4e-sent-folder   . "/mongodb/Sent" )
		      ( mu4e-trash-folder  . "/mongodb/Trash" )
		      ( mu4e-refile-folder . "/mongodb/Archived" )
		      ( mu4e-sent-messages-behavior . delete )
		      ( smtpmail-smtp-server        . "smtp.gmail.com" )
		      ( mu4e-compose-signature      . nil )))

	   ,(make-mu4e-context
	     :name "personal"
	     :enter-func (lambda () (mu4e-message "entering 'personal' context"))
	     :leave-func (lambda () (mu4e-message "leaving 'personal' context"))
	     :match-func (lambda (msg)
			   (when msg
			     (string-prefix-p "/personal" (mu4e-message-field msg :maildir))))
	     :vars '( ( user-mail-address  . "ryan@ryanchipman.com" )
		      ( user-full-name     . "Ryan Chipman" )
		      ( mu4e-drafts-folder . "/personal/Drafts" )
		      ( mu4e-sent-folder   . "/personal/Sent Items" )
		      ( mu4e-trash-folder  . "/personal/Trash" )
		      ( mu4e-refile-folder . "/personal/Archive" )
		      ( mu4e-sent-messages-behavior . sent )
		      ( smtpmail-smtp-server        . "smtp.fastmail.com" )
		      ( mu4e-compose-signature      . nil )))))

  ;; don't set trashed flag when moving to trash
  (defvar rpc/mu4e-trash-flag-removed nil)
  (unless rpc/mu4e-trash-flag-removed
	(setq rpc/mu4e-trash-flag-removed t)
	(setq mu4e-marks (remove-nth-element mu4e-marks 5)))

  (add-to-list 'mu4e-marks
	       '(trash
		 :char ("d" . "▼")
		 :prompt "dtrash"
		 :dyn-target (lambda (target msg) (mu4e-get-trash-folder msg))
		 :action (lambda (docid msg target)
			   (mu4e~proc-move docid
			     (mu4e~mark-check-target target) "-N"))))
  )

(use-package projectile
  :ensure t
  :config

  (setq projectile-project-search-path '("~/git/"))

  (use-package counsel-projectile
    :ensure t
    :config
    (counsel-projectile-mode 1)))

(use-package git-gutter-fringe
  :ensure t
  :config
  (global-git-gutter-mode 1)
  (setq-default fringes-outside-margins t)
  (fringe-helper-define 'git-gutter-fr:added '(center repeated)
    "..XXX...")
  (fringe-helper-define 'git-gutter-fr:modified '(center repeated)
    "..XXX...")
  (fringe-helper-define 'git-gutter-fr:deleted 'bottom
    "X......."
    "XX......"
    "XXX....."
    "XXXX...."))

(use-package company
  :ensure t
  :config
  (global-company-mode 1)
  (setq-default company-echo-delay 0)
  (setq-default company-idle-delay 0.1)
  (setq-default company-auto-complete 'company-explicit-action-p)
  (setq-default company-minimum-prefix-length 2)
  (setq-default company-dabbrev-downcase nil)
  (define-key company-active-map (kbd "<tab>") 'company-select-next-if-tooltip-visible-or-complete-selection)
  (define-key company-active-map (kbd "S-<tab>") 'company-select-previous-or-abort)
  )

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (setq flycheck-indication-mode 'right-fringe)
  (fringe-helper-define 'flycheck-fringe-bitmap-double-arrow 'center
    "...XX..."
    "..XXX..."
    ".XXXX..."
    "XXXXX..."
    ".XXXX..."
    "..XXX..."
    "...XX..."))

(use-package hydra
  :ensure t
  :config

  (defhydra hydra-gitgutter ()
    "gitgutter"
    ("j" git-gutter:next-hunk)
    ("k" git-gutter:previous-hunk)
    ("s" git-gutter:stage-hunk)
    ("u" git-gutter:revert-hunk)
    ("R" git-gutter:set-start-revision))

  (defhydra hydra-zoom ()
    "zoom"
    ("=" zoom-frame-in "in")
    ("-" zoom-frame-out "out"))

  (defhydra hydra-flycheck
    (:pre (progn (setq hydra-lv t) (flycheck-list-errors))
     :post (progn (setq hydra-lv nil) (quit-windows-on "*Flycheck errors*"))
     :hint nil)
    "errors"
    ("f" flycheck-error-list-set-filter "filter")
    ("j" flycheck-next-error "next")
    ("k" flycheck-previous-error "prev")
    ("gg" flycheck-first-error "first")
    ("G" (progn (goto-char (point-max)) (flycheck-previous-error)) "last")
    ("q" nil)
    )

  (defun rpc/gud/prompt-call ()
	(interactive)
	(gud-call (read-string "GUD (call): ")))

  (defun rpc/gud/prompt-print ()
	(interactive)
	(gud-call (format "p %s" (read-string "GUD (print): "))))

  (defhydra hydra-gud
	(:foreign-keys run)
	"GUD"
	("n" gud-next "Next")
	("s" gud-step "Step")
	("c" gud-cont "Continue")
	("p" gud-print "Print")
	("P" rpc/gud/prompt-print "Print expression")
	("B" gud-break "Break")
	("d" gud-remove "Delete breakpoint")
	("R" gud-refresh "Refresh")
	("E" rpc/gud/prompt-call "Execute")
	("Q" nil)
	)

  (defhydra hydra-smerge
	(:foreign-keys run)
	"smerge"
	("n" smerge-next "Next")
	("p" smerge-previous "Previous")
	("RET" smerge-keep-current "Keep Current")
	("Q" nil)
 	)

  )

(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  (smartparens-global-mode)
  (show-smartparens-global-mode))

(use-package js2-mode
  :ensure t)

(use-package dart-mode
  :ensure t
  :custom
  (dart-format-on-save t)
  (dart-sdk-path "~/mobile/flutter/bin/cache/dart-sdk/"))

(use-package flutter
  :ensure t
  :after dart-mode
  :custom
  (flutter-sdk-path "~/mobile/flutter/"))

(use-package rust-mode
  :ensure t
  :config
  (setq rust-rustfmt-bin "~/.rustup/toolchains/nightly-x86_64-apple-darwin/bin/rustfmt")
  (setq rust-format-on-save t)

  (use-package cargo
	:ensure t)

  (use-package racer
	:ensure t
	:config
	(add-hook 'rust-mode-hook #'racer-mode)
	(add-hook 'racer-mode-hook #'eldoc-mode))
  )

(use-package yaml-mode
  :ensure t)

(use-package toml-mode
  :ensure t)

(use-package restclient
  :ensure t
  :config

  (use-package company-restclient
	:ensure t
	:config
	(add-to-list 'company-backends 'company-restclient))
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(ivy-mode t)
 '(ledger-reports
   (quote
	(("expenses-this-month" "ledger [[ledger-mode-flags]] -f /Users/ryan/ledger/test.ledger bal Expenses -p 'this month'")
	 (#("expenses-last-month" 0 1
		(idx 3))
	  "ledger [[ledger-mode-flags]] -f /Users/ryan/ledger/test.ledger bal Expenses -p 'last month'")
	 (#("bal" 0 1
		(idx 1))
	  "ledger [[ledger-mode-flags]] -f /Users/ryan/ledger/test.ledger bal Expenses -p 'last month'")
	 (#("unmatched" 0 1
		(idx 7))
	  "ledger [[ledger-mode-flags]] -f /Users/ryan/ledger/test.ledger reg -p 'until 5 days ago' Liabilities and not %matched and not %copied --uncleared")
	 (#("budget" 0 1
		(idx 2))
	  "ledger [[ledger-mode-flags]] -f /Users/ryan/ledger/test.ledger bal not equity and not income and not expenses")
	 (#("unknown" 0 1
		(idx 6))
	  "ledger [[ledger-mode-flags]] -f /Users/ryan/ledger/test.ledger reg Unknown")
	 (#("reg" 0 1
		(idx 5))
	  "%(binary) -f %(ledger-file) reg")
	 (#("payee" 0 1
		(idx 4))
	  "%(binary) -f %(ledger-file) reg @%(payee)")
	 (#("account" 0 1
		(idx 0))
	  "%(binary) -f %(ledger-file) reg %(account)"))))
 '(package-selected-packages
   (quote
	(dart-mode use-package-ensure-system-package forge key-chord crux ryo-modal perspective company-restclient restclient yaml-mode git-timemachine dumb-jump smart-jump toml-mode cargo cargo-mode persp-mode tablist elfeed mu4e-alert rust-mode gotest worf ledger-mode smartparens git-gutter-fringe hydra go-eldoc company epresent evil-magit diff-hl badger-theme counsel-projectile projectile cider clojure-mode syndicate evil-surround go-mode eyebrowse magit which-key general use-package)))
 '(safe-local-variable-values
   (quote
	((rpc/compile/build-command . "cd $(git rev-parse --show-toplevel) && go install cmd/mongosqld/mongosqld.go")
	 (rpc/compile/check-command . "go install")
	 (rpc/compile/check-command . "go install -gcflags='-e'")
	 (rpc/compile/unit-test-command . "go test")
	 (rpc/compile/build-command . "cargo build")
	 (rpc/compile/check-command . "cargo check")
	 (rpc/compile/unit-test-command . "cargo test")))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#1c1c1c" :foreground "#f6f3e8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 110 :width normal :foundry "nil" :family "Source Code Pro"))))
 '(fringe ((t (:background "#151515"))))
 '(hl-line ((t (:foreground nil :underline nil))))
 '(linum ((t (:inherit (shadow default) :background "#151515"))))
 '(shadow ((t (:foreground "#666666"))))
 '(whitespace-space ((t (:foreground "#353535"))))
 '(whitespace-tab ((t (:foreground "#353535")))))

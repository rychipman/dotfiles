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

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(blink-cursor-mode 0)
(scroll-bar-mode -1)

(global-linum-mode 1)
(global-hl-line-mode 0)

(setq-default tab-width 4)
(setq-default truncate-lines 0)
(setq-default help-window-select t)

(defun zoom-frame-monitor ()
 "Zoom the current frame to an appropriate size for my thinkvision monitor."
 (interactive)
 (set-face-attribute 'default (selected-frame) :height 150))

(defun zoom-frame-laptop ()
 "Zoom the current frame to an appropriate size for my laptop screen."
 (interactive)
 (set-face-attribute 'default (selected-frame) :height 120))

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
(global-whitespace-mode)

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/Users/ryan/.go/bin:/Users/ryan/.cargo/bin:/Library/TeX/texbin"))
(setenv "GOPATH" "/Users/ryan/.go")
(setenv "PKG_CONFIG_PATH" (concat (getenv "PKG_CONFIG_PATH") "/usr/local/Cellar/openssl/1.0.2n/lib/pkgconfig"))
(setq exec-path (append exec-path '("/usr/local/bin" "/usr/sbin" "/usr/bin" "/sbin" "/Users/ryan/.go/bin" "/Users/ryan/.cargo/bin" "/Library/TeX/texbin")))

(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "/usr/local/Cellar/mu/1.0/share/emacs/site-lisp/mu/mu4e/")

(load-theme 'wombat t)
(custom-theme-set-faces
 'wombat
 '(font-lock-builtin-face ((t (:foreground "#e5c654"))))
 '(font-lock-constant-face ((t (:foreground "#e58954"))))
 '(font-lock-function-name-face ((t (:foreground "#efe67c"))))
 '(font-lock-type-face ((t (:foreground "#d5f29f"))))
 '(font-lock-keyword-face ((t (:foreground "#8cceff"))))
 '(git-gutter-fr:modified ((t (:foreground "#c1a10f"))))
 '(git-gutter-fr:added ((t (:foreground "#46843c"))))
 '(git-gutter-fr:deleted ((t (:foreground "#8e0801"))))
 '(cursor ((t (:background "orange"))))
 '(company-preview ((t (:background "#303030"))))
 '(company-preview-common ((t (:foreground "#8cceff"))))
 '(company-tooltip ((t (:background "#303030"))))
 '(company-tooltip-selection ((t (:background "#444444"))))
 '(company-tooltip-common ((t (:foreground "#8cceff"))))
 '(company-scrollbar-fg ((t (:background "#777777"))))
 '(company-scrollbar-bg ((t (:background "#444444"))))
 '(whitespace-trailing ((t :background "#e5786d")))
 )
(set-face-attribute 'highlight nil :foreground 'unspecified :background "#303030" :underline nil)

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
   "fs" 'save-buffer
   "ff" 'counsel-find-file
   "fe" '(:ignore t :which-key "emacs")
   "fee" (lambda () "edit" (interactive) (find-file "~/.emacs.d/init.el"))
   "fer" (lambda () "reload" (interactive) (load-file user-init-file))

   ;; buffer
   "b" '(:ignore t :which-key "file")
   "bb" 'counsel-ibuffer
   "bd" 'evil-delete-buffer

   ;; quit
   "q" '(:ignore t :which-key "quit")
   "qq" 'save-buffers-kill-terminal

   ;; search
   "s"  '(:ignore t :which-key "search")
   "ss" 'swiper

   ;; git
   "g" '(:ignore t :which-key "git")
   "gs" 'magit-status
   "gf" 'magit-file-popup
   "gm" 'magit-dispatch-popup
   "gb" 'magit-blame
   "gg" 'hydra-gitgutter/body

   ;; org
   "o" '(:ignore t :which-key "org")
   "oa" 'org-agenda
   "oc" 'org-capture
   "ol" 'org-store-link
   "or" 'org-refile
   "oA" 'org-archive
   "oA" 'org-archive
   "of" 'rpc/open-org-file

   ;; window
   "w" '(:ignore t :which-key "window")
   "wl" 'evil-window-right
   "wh" 'evil-window-left
   "wk" 'evil-window-up
   "wj" 'evil-window-down
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

   ;; ledger
   "l" '(:ignore t :which-key "ledger")
   "lf" (lambda () "file" (interactive) (find-file "~/ledger/test.ledger"))
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
   "c" 'compile

   ;; mu4e
   "m" '(:ignore t :which-key "mu4e")
   "mm" 'mu4e

   ;; misc
   "SPC" 'counsel-M-x
   ":" 'eval-expression
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
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
  (add-hook 'with-editor-mode-hook 'evil-insert-state)

  (define-key evil-visual-state-map (kbd ">") 'rpc/evil-shift-right-visual)
  (define-key evil-visual-state-map (kbd "<") 'rpc/evil-shift-left-visual)

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

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode 1)))

(use-package magit
  :ensure t
  :config

  (use-package evil-magit
    :ensure t))

(use-package org
  :ensure t
  :config
  (load-file "~/.emacs.d/org.el"))

(use-package eyebrowse
  :ensure t
  :config
  (eyebrowse-mode t)
  (setq-default eyebrowse-new-workspace t)
  (setq-default eyebrowse-wrap-around t))

(use-package go-mode
  :ensure t
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  (setq-default gofmt-command "goimports")

  (use-package company-go
    :ensure t
    :config
    (add-hook 'go-mode-hook
		(lambda ()
		(set (make-local-variable 'company-backends) '(company-go))
		(company-mode 1)))
    )

  (use-package go-eldoc
    :ensure t
    :config
    (add-hook 'go-mode-hook 'go-eldoc-setup)
    )
  )

(use-package ledger-mode
  :ensure t
  :config
  (load-file "~/.emacs.d/ledger/ledger.el")
  (define-key ledger-reconcile-mode-map (kbd "m") 'ledger-reconcile-toggle))

(defun remove-nth-element (list nth)
  "Return a copy of LIST without its NTH element."
  (if (zerop nth) (cdr list)
    (let ((last (nthcdr (1- nth) list)))
      (setcdr last (cddr last))
      list)))

(use-package mu4e
  :commands (mu4e)
  :config

  ;; general settings
  (require 'org-mu4e)
  (setq
   mu4e-maildir "~/mail"
   mu4e-mu-binary "/usr/local/bin/mu"
   mu4e-get-mail-command "mbsync -Va"
   mu4e-change-filenames-when-moving t
   mu4e-completing-read-function 'completing-read
   mu4e-confirm-quit nil)

  (add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)

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
  )

(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  (smartparens-global-mode)
  (show-smartparens-global-mode))

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(ivy-mode t)
 '(package-selected-packages
   (quote
    (epresent evil-magit diff-hl badger-theme counsel-projectile projectile cider clojure-mode syndicate evil-surround go-mode eyebrowse magit which-key general use-package))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#1c1c1c" :foreground "#f6f3e8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "Source Code Pro"))))
 '(fringe ((t (:background "#151515"))))
 '(hl-line ((t (:foreground nil :underline nil))))
 '(linum ((t (:inherit (shadow default) :background "#151515"))))
 '(shadow ((t (:foreground "#666666"))))
 '(whitespace-space ((t (:foreground "#353535"))))
 '(whitespace-tab ((t (:foreground "#353535")))))

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
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(blink-cursor-mode 0)
(scroll-bar-mode -1)

(global-linum-mode 1)
(global-hl-line-mode 0)

(setq-default truncate-lines 0)
(setq-default help-window-select t)

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

(setq-default whitespace-style '(face spaces tabs tab-mark))
(global-whitespace-mode)

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/Users/ryan/.go/bin"))
(setenv "GOPATH" "/Users/ryan/.go")
(setq exec-path (append exec-path '("/usr/local/bin" "/usr/sbin" "/usr/bin" "/sbin" "/Users/ryan/.go/bin")))

(load-theme 'wombat t)
(custom-theme-set-faces
 'wombat
 '(font-lock-builtin-face ((t (:foreground "#e5c654"))))
 '(font-lock-constant-face ((t (:foreground "#e58954"))))
 '(font-lock-function-name-face ((t (:foreground "#efe67c"))))
 '(font-lock-type-face ((t (:foreground "#d5f29f"))))
 '(font-lock-keyword-face ((t (:foreground "#8cceff"))))
 '(diff-hl-change ((t (:foreground "#c1a10f" :background "#c1a10f"))))
 '(diff-hl-insert ((t (:foreground "#46843c" :background "#46843c"))))
 '(diff-hl-delete ((t (:foreground "#8e0801" :background "#8e0801"))))
 '(cursor ((t (:background "orange"))))
 '(company-preview ((t (:background "#303030"))))
 '(company-preview-common ((t (:foreground "#8cceff"))))
 '(company-tooltip ((t (:background "#303030"))))
 '(company-tooltip-selection ((t (:background "#444444"))))
 '(company-tooltip-common ((t (:foreground "#8cceff"))))
 '(company-scrollbar-fg ((t (:background "#777777"))))
 '(company-scrollbar-bg ((t (:background "#444444"))))
 )

(require 'use-package)

(use-package swiper
  :ensure t)

(use-package counsel
  :ensure t)

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-initial-inputs-alist nil))

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
   "gm" 'magit-dispatch-popup
   "gb" 'magit-blame

   ;; org
   "o" '(:ignore t :which-key "org")
   "oa" 'org-agenda
   "oc" 'org-capture
   "or" 'org-refile
   "oA" 'org-archive
   "oA" 'org-archive

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

   ;; help
   "h" '(:ignore t :which-key "help")
   "hf" 'counsel-describe-function
   "hv" 'counsel-describe-variable
   "hk" 'counsel-describe-key

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

   ;; resume
   "r" 'ivy-resume

   ;; zoom
   "z" '(:ignore t :which-key "zoom")
   "zt" '(lambda () "toggle zoom" (interactive)(toggle-zoom-frame))
   "zz" 'hydra-zoom/body

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
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
  (setq-default org-hide-leading-stars t)

  (setq org-todo-keywords
        '((sequence "TODO(t)"
		    "WAITING(w)"
		    "|"
		    "DONE(d)"
		    "CANCELED(c)")))

  (setq org-capture-templates
        '(("j" "Journal Entry"
           entry (file+datetree "~/org/journal.org")
           "* %?")
	  ("T" "Tickler Entry"
	   entry (file+headline "~/org/gtd/tickler.org" "Tickler")
	   "* %i%?\n  %U")
          ("t" "Task [inbox]"
           entry (file+headline "~/org/gtd/inbox.org" "Inbox")
           "* TODO %?"
           :empty-lines 1
           :prepend t)))

  (setq org-refile-use-outline-path t)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))

  (setq org-agenda-files (list "~/org/emacs.org"
			       "~/org/personal.org"
			       "~/org/work.org"
			       "~/org/gtd/inbox.org"
			       "~/org/gtd/tickler.org"
			       "~/org/gtd/gtd.org")))

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

(use-package mu4e
  :commands (mu4e)
  :config
  (setq-default
   mu4e-mu-binary "/usr/local/bin/mu"
   mu4e-get-mail.command "offlineimap"
   mu4e-drafts-folder "/MongoDB/[Gmail].Drafts"
   mu4e-sent-folder "/MongoDB/[Gmail].All Mail"
   mu4e-trash-folder "/MongoDB/[Gmail].Trash"
   mu4e-refile-folder "/MongoDB/[Gmail].All Mail"
   mu4e-sent-messages-behavior 'delete))

(use-package projectile
  :ensure t
  :config
  (use-package counsel-projectile
    :ensure t
    :config
    (counsel-projectile-mode 1)))

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode 1)
  (diff-hl-flydiff-mode)
  (fringe-mode 3)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  )

(use-package company
  :ensure t
  :config
  (global-company-mode 1)
  (setq-default company-echo-delay 0)
  (setq-default company-idle-delay 0.1)
  (setq-default company-auto-complete t)
  (setq-default company-minimum-prefix-length 2)
  (define-key company-active-map [tab] 'company-complete-common-or-cycle)
  )

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package hydra
  :ensure t
  :config

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

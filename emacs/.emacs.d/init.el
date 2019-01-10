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
   "fe" '(:ignore t :which-key "emacs")
   "fee" (lambda () "edit" (interactive) (find-file "~/.emacs.d/init.el"))
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
   "oA" 'org-archive
   "of" 'rpc/open-org-file

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

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode 1)))

(defun kakmacs-deselect ()
  (interactive)
  (deactivate-mark))

(defun kakmacs-kill-char-or-region ()
  (interactive)
  (if mark-active
      (call-interactively #'kill-region)
    (call-interactively #'delete-forward-char)))

(defun kakmacs-replace-char-at-point ()
  "Like vim's replace command."
  (interactive)
  (delete-char 1)
  (insert " ")
  (backward-char 1)
  (message "Replace with char.")
  (setq-local cursor-type '(hbar . 3))
  (call-interactively #'quoted-insert)
  (setq-local cursor-type (default-value 'cursor-type))
  (delete-char 1)
  (backward-char 1))

(defun kakmacs-delete-and-yank ()
  (interactive)
  (delete-region (region-beginning) (region-end))
  (yank))

(defun kakmacs-delete-and-exit ()
  (interactive)
  (delete-region (region-beginning) (region-end))
  (ryo-modal-mode -1))

(defun kakmacs-swiper-mark-match ()
  "Mark swiper match."
  (interactive)
  (set-mark swiper--current-match-start))

(defun kakmacs-mark-inner ()
  "Takes a char, like ( or \" and marks the innards of the first
  ancestor semantic unit starting with that char."
  (interactive)
  (let* ((expand-region-fast-keys-enabled nil)
         (char (char-to-string
                (read-char "Char: ")))
         (q-char (regexp-quote char))
         (starting-point (point)))
    (flet ((message (&rest args) nil))
      (er--expand-region-1)
      (er--expand-region-1)
      (while (and (not (= (point) (point-min)))
                  (not (looking-at q-char)))
        (er--expand-region-1))
      (if (not (looking-at q-char))
          (progn
            (goto-char starting-point)
            (setq mark-active nil)
            (error (concat "No match for " char)))
        (er/contract-region 1)))))

(defun kakmacs-mark-outer ()
  "Takes a char, like ( or \" and marks the first ancestor
semantic unit starting with that char."
  (interactive)
  (let* ((expand-region-fast-keys-enabled nil)
         (char (char-to-string
                (read-char "Char: ")))
         (q-char (regexp-quote char))
         (starting-point (point)))
    (flet ((message (&rest args) nil))
      (when (looking-at q-char)
        (er/expand-region 1))
      (while (and (not (= (point) (point-min)))
                  (not (looking-at q-char)))
        (er/expand-region 1))
      (unless (looking-at q-char)
        (goto-char starting-point)
        (setq mark-active nil)
        (error (concat "No match for " char))))))

(defun kakmacs-end-of-region ()
  "Goto end of region."
  (interactive)
  (goto-char (region-end)))

(use-package crux
  :ensure t)

(use-package expand-region
  :ensure t)

(use-package selected
  :load-path "mylisp/selected"
  :ensure t
  :config
  (setq selected-minor-mode-override t))

(use-package ryo-modal
  :ensure t
  :bind ("C-c SPC" . ryo-modal-mode)
  :commands ryo-modal-mode
  :init
  (add-hook 'ryo-modal-mode-hook
            (lambda ()
              (if ryo-modal-mode
                  (selected-minor-mode 1)
                (selected-minor-mode -1))))
  :config
  (defun rpc/ryo-modal-enter ()
	(interactive)
	(ryo-modal-mode 1))

  ;(global-set-key (kbd "<escape>") #'rpc/ryo-modal-enter)
  ;(setq ryo-modal-cursor-color nil)

  (defun rpc/surround (&optional char-str)
	(interactive "schar: ")
	(insert-pair nil char-str char-str))

  (defun rpc/init/edit ()
	(interactive)
	(find-file user-init-file))

  (defun rpc/init/reload ()
	(interactive)
	(load-file user-init-file))

  (defun rpc/modal/edit-after ()
	(interactive)
	(forward-char)
	(kakmacs-deselect))

  (require 'expand-region)
  (progn
	(ryo-modal-keys
	 (:norepeat t)
	 ("-" negative-argument)
	 ("0" "M-0")
	 ("1" "M-1")
	 ("2" "M-2")
	 ("3" "M-3")
	 ("4" "M-4")
	 ("5" "M-5")
	 ("6" "M-6")
	 ("7" "M-7")
	 ("8" "M-8")
	 ("9" "M-9"))

	(ryo-modal-keys
	 (:first '((lambda () (set-mark (point)))))
	 ("F" avy-goto-word-1)
	 ("H" backward-char)
	 ("J" next-line)
	 ("K" previous-line)
	 ("L" forward-char))

	(ryo-modal-keys
	 (:first '(kakmacs-deselect))
	 ("f" avy-goto-word-1 :then '(er/mark-word))
	 ("Ö" comment-dwim :exit t)
	 ("<backspace>" delete-backward-char)
	 ("z"
	  (("." er/mark-symbol :name "Symbol")
	   (":" er/mark-symbol-with-prefix :name "Symbol+Prefix")
	   ("B" mark-whole-buffer :name "All buffer")
	   ;("E" org-mark-element :name "Org Element")
	   ;("b" er/mark-python-block :name "Block")
	   ("d" er/mark-defun :name "Defun")
	   ("f" avy-goto-word-1 :then (er/mark-symbol) :name "◎ Symbol")
	   ("i" kakmacs-mark-inner :name "Inner")
	   ("m" er/mark-method-call :name "Method call")
	   ("n" er/mark-next-accessor :name "Next accessor")
	   ("o" kakmacs-mark-outer :name "Outer")
	   ;("p" er/mark-paragraph :name "Paragraph")
	   ;("s" er/mark-sentence :name "Sentence")
	   ;("t" org-mark-subtree :name "Org Subtree")
	   ("u" er/mark-url :name "URL")
	   ("w" er/mark-word :name "Word")
	   (";" er/mark-comment :name "Comment")
	   ("ö" er/mark-comment :name "Comment"))
	  :name "select"))

	(ryo-modal-keys
	 ("v" ryo-modal-repeat)
	 ("x" "M-x")
	 (":" eval-expression)
	 ("." er/expand-region)
	 (">" er/contract-region)
	 ("j" next-line)
	 ("k" previous-line)
	 ("h" backward-char)
	 ("l" forward-char)
	 ("w" forward-word)
	 ("b" backward-word)
	 ("," exchange-point-and-mark)
	 ("d" kakmacs-kill-char-or-region)
	 ("o" crux-smart-open-line :exit t)
	 ("O" crux-smart-open-line-above :exit t)
	 ("c" copy-region-as-kill)
	 ("C" copy-region-as-kill :then '(kakmacs-end-of-region
									  newline-and-indent
									  yank))
	 ("r" kakmacs-replace-char-at-point)
	 ("u" undo)
	 ("ö" comment-line)
	 ("S" query-replace)
	 ("s" swiper :then '(kakmacs-swiper-mark-match))
	 ("A" move-to-mode-line-start :exit t)
	 ("E" end-of-line :exit t)
	 ("n" mc/mark-next-like-this)
	 ("N" mc/mark-previous-like-this)
	 ("a" rpc/modal/edit-after :exit t)
	 ("i" kakmacs-deselect :exit t)
	 ("y" yank)
	 ("p" pop-to-mark-command)
	 ("P" counsel-mark-ring)
	 ("q" kakmacs-deselect)
	 ("R" delete-region :then '(yank) :exit t)
	 ("Y" delete-region :then '(yank))
	 ("]" forward-paragraph)
	 ("[" backward-paragraph)
	 ("SPC"
	  (;("w" rpc/window-hydra)
	   ;("pf" counsel-projectile-find-file)
	   ;("pp" counsel-projectile-switch-project)
	   ;("ps" counsel-projectile-rg)
	   ;("dd" dired-jump-other-window)
	   ;("mm" rpc/mu4e/open)
	   ;("gs" magit-status)
       ;("wl" windmove-right)
       ;("wh" windmove-left)
       ;("wk" windmove-up)
       ;("wj" windmove-down)
       ;("wd" delete-window)
       ;("ww" eyebrowse-next-window-config)
	   ;("wo" delete-other-windows)
       ;("wW" eyebrowse-create-window-config)
       ;("wR" eyebrowse-rename-window-config)
       ;("wD" eyebrowse-close-window-config)
       ;("wf" eyebrowse-switch-to-window-config)
	   ;("zm" zoom-frame-monitor)
	   ;("zl" zoom-frame-laptop)
	   ;("zz" hydra-zoom/body)
	   ;("cc" rpc/compile/check)
	   ;("cb" rpc/compile/build)
	   ;("ct" rpc/compile/unit-test)
	   ;("ck" rpc/compile-quit-windows)
	   ;("ld" plaint-download)
	   ;("lf" (lambda () (find-file "~/ledger/test.ledger")))
	   ;("lc" ledger-check-buffer)
	   ;("li" rpc/ledger-match-imports)
	   ;("la" rpc/ledger-add-interactive)
	   ;("en" next-error)
	   ;("ep" previous-error)
	   ;("fer" rpc/init/reload)
	   ;("fee" rpc/init/edit)
	   )
	  :name "leader")

	 ("g" :hydra
	  '(hydra-nav (:pre (set-mark (point)))
				  "A hydra for navigation"
				  ("h" backward-up-list)
				  ("l" down-list)
				  ("j" forward-sexp)
				  ("k" backward-sexp)
				  ("a" beginning-of-defun)
				  ("e" end-of-defun)
				  ("n" forward-paragraph)
				  ("p" backward-paragraph)
				  ("f" forward-sentence)
				  ("b" backward-sentence)
				  ("i" imenu :color blue)
				  ("q" nil "cancel" :color blue)
				  ("g" avy-goto-line :color blue))))

	(bind-keys
	 :map selected-keymap
	 ("S" . rpc/surround)
	 ("r" . kakmacs-delete-and-exit)
	 )
	)
  )

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
	(define-key git-rebase-mode-map (kbd "J") 'git-rebase-move-line-down)
	(define-key git-rebase-mode-map (kbd "K") 'git-rebase-move-line-up))

  (use-package forge
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

  (use-package go-dlv
	:ensure t)
  )

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
   mu4e-maildir "~/mail"
   mu4e-mu-binary "/usr/local/bin/mu"
   mu4e-get-mail-command "mbsync -Va"
   mu4e-change-filenames-when-moving t
   mu4e-completing-read-function 'completing-read
   shr-color-visible-luminance-min 80
   send-mail-function 'smtpmail-send-it
   smtpmail-smtp-service 465
   smtpmail-stream-type 'ssl
   mu4e-hide-index-messages t
   mu4e-compose-format-flowed t
   message-kill-buffer-on-exit t
   mu4e-confirm-quit nil)

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
  (use-package counsel-projectile
    :ensure t
    :config
    (counsel-projectile-mode 1)))

(use-package perspective
  :ensure t
  :config
  (persp-mode)
  (use-package persp-projectile
	:ensure t)
  )

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

(use-package boon
  :ensure t
  :config
  (require 'boon-qwerty))

(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  (smartparens-global-mode)
  (show-smartparens-global-mode))

(use-package js2-mode
  :ensure t)

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

(use-package smart-jump
  :ensure t
  :config
  (global-set-key (kbd "C-j") 'smart-jump-go)

  (require 'smart-jump-go-mode)
  (smart-jump-go-mode-register)
  (define-key go-mode-map (kbd "C-c C-j") nil)

  (require 'smart-jump-elisp-mode)
  (smart-jump-elisp-mode-register)

  (require 'smart-jump-rust-mode)
  (smart-jump-rust-mode-register)
  )

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
	(forge key-chord crux ryo-modal perspective company-restclient restclient yaml-mode git-timemachine dumb-jump smart-jump toml-mode cargo cargo-mode persp-mode tablist elfeed mu4e-alert rust-mode gotest worf ledger-mode smartparens git-gutter-fringe hydra go-eldoc company epresent evil-magit diff-hl badger-theme counsel-projectile projectile cider clojure-mode syndicate evil-surround go-mode eyebrowse magit which-key general use-package)))
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
 '(default ((t (:inherit nil :stipple nil :background "#1c1c1c" :foreground "#f6f3e8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "Source Code Pro"))))
 '(fringe ((t (:background "#151515"))))
 '(hl-line ((t (:foreground nil :underline nil))))
 '(linum ((t (:inherit (shadow default) :background "#151515"))))
 '(shadow ((t (:foreground "#666666"))))
 '(whitespace-space ((t (:foreground "#353535"))))
 '(whitespace-tab ((t (:foreground "#353535")))))

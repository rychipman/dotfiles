(in-package :stumpwm)

(defcommand firefox() ()
  (run-or-raise "firefox" '(:class "firefox")))

(defcommand lxterminal() ()
  (run-or-raise "lxterminal" '(:class "Lxterminal")))

(defcommand slack() ()
  (run-or-raise "slack" '(:class "Slack")))

(define-keysym #x1008ff11 "XF86AudioLowerVolume")
(define-keysym #x1008ff12 "XF86AudioMute")
(define-keysym #x1008ff13 "XF86AudioRaiseVolume")

(define-key *top-map* (kbd "XF86AudioRaiseVolume") "exec volume -i")
(define-key *top-map* (kbd "XF86AudioLowerVolume") "exec volume -d")
(define-key *top-map* (kbd "XF86AudioMute") "exec volume -t")

(define-key *top-map* (kbd "H-f") "fnext")
(define-key *top-map* (kbd "H-h") "slack")
(define-key *top-map* (kbd "H-j") "emacs")
(define-key *top-map* (kbd "H-k") "firefox")
(define-key *top-map* (kbd "H-l") "lxterminal")
(define-key *top-map* (kbd "H-o") "pull-hidden-next")
(define-key *top-map* (kbd "H-i") "pull-hidden-previous")
(define-key *top-map* (kbd "H-p") "exec rofi-pass")
(define-key *top-map* (kbd "H-;") "colon")
(define-key *top-map* (kbd "H-:") "eval")
(define-key *top-map* (kbd "H-DEL") "exec lock")
(set-prefix-key (kbd "H-m"))

(defcommand toggle-modeline() ()
  (toggle-mode-line (current-screen) (current-head)))
;(define-key *top-map* (kbd "H-m") "toggle-modeline")

(setf *screen-mode-line-format* "^f1[^B%n^b] %W")

(setf *mouse-focus-policy* :click)

(set-fg-color "#71c2af")
(set-bg-color "#1d2a30")
(set-border-color "#464b50")
(set-msg-border-width 5)
(setq *message-window-gravity* :center)
(setq *input-window-gravity* :center)
(setf *window-border-style* :thin)
(setf *normal-border-width* 2)

;(defun load-from-ql ()
;  (load "~/quicklisp/setup.lisp")
;
;  (ql:quickload :clx-truetype)
;  (load-module "ttf-fonts")
;  (xft:cache-fonts)
;
;  (set-font `(,(make-instance 'xft:font :family "Liberation Mono" :subfamily "Regular" :size 16)
;			   "9x15bold"))
;
;  (require :swank)
;  (swank-loader:init)
;  (swank:create-server :port 4004
;					   :style swank:*communication-style*
;					   :dont-close t))

;(load-from-ql)

;;; umber.el --- Custom face theme for Emacs

;; Copyright (C) 2019 Ryan Chipman

;; Author: Ryan Chipman <ryan@ryanchipman.com>

;;; Code:

(deftheme umber
  "Medium-contrast faces with a dark gray background.
Basic, Font Lock, Isearch, Gnus, Message, and Ansi-Color faces
are included.")

(let ((class '((class color) (min-colors 89))))
  (custom-theme-set-faces
   'umber

   ;; Basic faces
   `(default ((,class (:background "#242424" :foreground "#f6f3e8"))))
   `(cursor ((,class (:background "orange"))))

   ;; Highlighting faces
   `(fringe ((,class (:background "#303030"))))
   `(highlight ((,class (:background "#303030"))))
   `(region ((,class (:background "#444444" :foreground))))
   `(secondary-selection ((,class (:background "#333366"))))
   `(isearch ((,class (:background "#343434"))))
   `(lazy-highlight ((,class (:background "#384048"))))

   ;; Mode line faces
   `(mode-line ((,class (:background "#444444" :foreground "#f6f3e8"))))
   `(mode-line-inactive ((,class (:background "#444444" :foreground "#857b6f"))))

   ;; Escape and prompt faces
   `(minibuffer-prompt ((,class (:foreground "#e5786d"))))
   `(escape-glyph ((,class (:foreground "#ddaa6f" :weight bold))))
   `(homoglyph ((,class (:foreground "#ddaa6f" :weight bold))))

   ;; Font lock faces
   `(font-lock-builtin-face ((,class (:foreground "#d4b543"))))
   `(font-lock-comment-face ((,class (:foreground "#99968b"))))
   `(font-lock-constant-face ((,class (:foreground "#e58954"))))
   `(font-lock-function-name-face ((,class (:foreground "#ffe67c"))))
   `(font-lock-keyword-face ((,class (:foreground "#8cceff"))))
   `(font-lock-string-face ((,class (:foreground "#95e454"))))
   `(font-lock-type-face ((,class (:foreground "#cae682"))))
   `(font-lock-variable-name-face ((,class (:foreground "#dd8866"))))
   `(font-lock-warning-face ((,class (:foreground "#ccaa8f"))))
   `(font-lock-doc-face ((,class (:foreground "#888888"))))

   ;; Button and link faces
   `(link ((,class (:foreground "#8ac6f2" :underline t))))
   `(link-visited ((,class (:foreground "#e5786d" :underline t))))
   `(button ((,class (:background "#333333" :foreground "#f6f3e8"))))
   `(header-line ((,class (:background "#303030" :foreground "#e7f6da"))))

   ;; Org faces
   `(org-special-keyword ((,class (:foreground "#8ac6f2"))))
   `(org-date ((,class (:foreground "#8ac6f2"))))
   `(org-scheduled-previously ((,class (:foreground "#d4b543"))))
   `(org-warning ((,class (:foreground "chocolate1"))))
   `(org-upcoming-deadline ((,class (:foreground "#ccaa8f"))))
   `(org-agenda-done ((,class (:foreground "#99968b"))))

   ;; Gnus faces
   `(gnus-group-news-1 ((,class (:weight bold :foreground "#95e454"))))
   `(gnus-group-news-1-low ((,class (:foreground "#95e454"))))
   `(gnus-group-news-2 ((,class (:weight bold :foreground "#cae682"))))
   `(gnus-group-news-2-low ((,class (:foreground "#cae682"))))
   `(gnus-group-news-3 ((,class (:weight bold :foreground "#ccaa8f"))))
   `(gnus-group-news-3-low ((,class (:foreground "#ccaa8f"))))
   `(gnus-group-news-4 ((,class (:weight bold :foreground "#99968b"))))
   `(gnus-group-news-4-low ((,class (:foreground "#99968b"))))
   `(gnus-group-news-5 ((,class (:weight bold :foreground "#cae682"))))
   `(gnus-group-news-5-low ((,class (:foreground "#cae682"))))
   `(gnus-group-news-low ((,class (:foreground "#99968b"))))
   `(gnus-group-mail-1 ((,class (:weight bold :foreground "#95e454"))))
   `(gnus-group-mail-1-low ((,class (:foreground "#95e454"))))
   `(gnus-group-mail-2 ((,class (:weight bold :foreground "#cae682"))))
   `(gnus-group-mail-2-low ((,class (:foreground "#cae682"))))
   `(gnus-group-mail-3 ((,class (:weight bold :foreground "#ccaa8f"))))
   `(gnus-group-mail-3-low ((,class (:foreground "#ccaa8f"))))
   `(gnus-group-mail-low ((,class (:foreground "#99968b"))))
   `(gnus-header-content ((,class (:foreground "#8ac6f2"))))
   `(gnus-header-from ((,class (:weight bold :foreground "#95e454"))))
   `(gnus-header-subject ((,class (:foreground "#cae682"))))
   `(gnus-header-name ((,class (:foreground "#8ac6f2"))))
   `(gnus-header-newsgroups ((,class (:foreground "#cae682"))))

   ;; Message faces
   `(message-header-name ((,class (:foreground "#8ac6f2" :weight bold))))
   `(message-header-cc ((,class (:foreground "#95e454"))))
   `(message-header-other ((,class (:foreground "#95e454"))))
   `(message-header-subject ((,class (:foreground "#cae682"))))
   `(message-header-to ((,class (:foreground "#cae682"))))
   `(message-cited-text ((,class (:foreground "#99968b"))))
   `(message-separator ((,class (:foreground "#e5786d" :weight bold))))

   ;; Whitespace faces
   `(whitespace-trailing ((,class :background "#e5786d")))

   ;; Company faces
   `(company-preview ((,class (:background "#303030"))))
   `(company-preview-common ((,class (:foreground "#8cceff"))))
   `(company-tooltip ((,class (:background "#303030"))))
   `(company-tooltip-selection ((,class (:background "#444444"))))
   `(company-tooltip-common ((,class (:foreground "#8cceff"))))
   `(company-scrollbar-fg ((,class (:background "#777777"))))
   `(company-scrollbar-bg ((,class (:background "#444444"))))

   ;; git-gutter faces
   `(git-gutter-fr:modified ((,class (:foreground "#c1a10f"))))
   `(git-gutter-fr:added ((,class (:foreground "#46843c"))))
   `(git-gutter-fr:deleted ((,class (:foreground "#8e0801"))))

   ))

(custom-theme-set-variables
 'umber
 '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682"
			    "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"]))

(provide-theme 'umber)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; umber.el ends here

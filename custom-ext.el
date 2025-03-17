;; -*- lexical-binding: t; -*-

(require 'dash)

(eval-when-compile
  (require 'custom))

;; Custom variables and faces

(defgroup user-extensions nil
  "Group for user-defined extensions."
  :group 'emacs)

(defface font-lock-operator-face
  '((t (:inherit font-lock-function-name-face)))
  "Font lock mode face used to highlight operators."
  :group 'font-lock-faces)

(defface user-ext-log-view-info
  '((t (:foreground "Blue1" :weight bold)))
  "Face for DEBUG in log files."
  :group 'user-extensions)

(defface user-ext-log-view-debug
  '((t (:inherit success)))
  "Face for DEBUG in log files."
  :group 'user-extensions)

(defface user-ext-log-view-error
  '((t (:inherit error :weight bold)))
  "Face for ERROR in log files"
  :group 'user-extensions)

(defface user-ext-log-view-warning
  '((t (:inherit warning :weight bold)))
  "Face for WARNING in log files"
  :group 'user-extensions)

(defface user-ext-log-view-date
  '((t (:foreground "red")))
  "Face for date strings in log files."
  :group 'user-extensions)

(defface user-ext-log-view-time
  '((t (:inherit org-agenda-current-time)))
  "Face for time strings in log files."
  :group 'user-extensions)

;; Functions

(defun define-face-alias (current-face other-face)
  "Make CURRENT-FACE an alias to OTHER-FACE."
  (put current-face 'face-alias other-face))

;; Aliases

;; (cl-prettyexpand '(define-obsolete-face-alias log-view-date user-ext-log-view-debug
;; 		    "x"))
;; (progn
;;   (put log-view-date 'face-alias user-ext-log-view-debug)
;;   (put log-view-date 'obsolete-face (or (purecopy "x") t)))

(define-face-alias 'log-view-date 'user-ext-log-view-date)

(define-face-alias 'log-view-date    'user-ext-log-view-date)
(define-face-alias 'log-view-debug   'user-ext-log-view-debug)
(define-face-alias 'log-view-error   'user-ext-log-view-error)
(define-face-alias 'log-view-info    'user-ext-log-view-info)
(define-face-alias 'log-view-time    'user-ext-log-view-time)
(define-face-alias 'log-view-warning 'user-ext-log-view-warning)

(defgroup user-ext-global-modes nil
  "A group for global modes."
  :group 'user-extensions)

(provide 'custom-ext)

;;; custom-ext ends here

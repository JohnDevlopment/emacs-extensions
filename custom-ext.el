;;; custom-ext --- Customization variables.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

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

;; Aliases

(defalias 'log-view-date    'user-ext-log-view-date)
(defalias 'log-view-debug   'user-ext-log-view-debug)
(defalias 'log-view-error   'user-ext-log-view-error)
(defalias 'log-view-info    'user-ext-log-view-info)
(defalias 'log-view-time    'user-ext-log-view-time)
(defalias 'log-view-warning 'user-ext-log-view-warning)

(defgroup user-ext-global-modes nil
  "A group for global modes."
  :group 'user-extensions)

(provide 'custom-ext)

;;; custom-ext ends here

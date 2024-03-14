;; (defun make-alist (&rest args)
;;   "Construct an alist using ARGS and return it."
;;   (let ((index 0)
;; 	(length (length args))
;; 	res)
;;     (while (< index length)
;;       (let ((first-element (nth index args))
;; 	    (second-element (nth (1+ index) args)))
;; 	(cl-pushnew first-element res)
;; 	(cl-pushnew second-element res)
;; 	(setq index (+ index 2))))
;;     res))

;; (defvar user-ext-temp-custom-vars nil
;;   "A list of custom variables to reset after Emacs exits.")

;; (defun reset-customization-variables ()
;;   ;; (setq foo (eval (car (get 'foo 'standard-value))))
;;   (dolist (var user-ext-temp-custom-vars nil)
;;     (unless (custom-variable-p var)
;;       (user-error "'%s' is not a custom variable" var))
;;     (set var (eval (car (get var 'standard-value))))))

;; (defun add-temp-custom-variable (name)
;;   "Adds NAME to `user-ext-temp-custom-vars'."
;;   (interactive "vCustom variable: ")
;;   (cl-pushnew name user-ext-temp-custom-vars))

;; (add-hook 'kill-emacs-hook #'reset-customization-variables)

;;; Custom variables and faces

(add-to-list 'load-path "~/.emacs.d/codeium.el")

(eval-and-compile
  (require 'codeium))

(defface font-lock-operator-face
  '((t (:inherit font-lock-function-name-face)))
  "Font lock mode face used to highlight operators."
  :group 'font-lock-faces)

(defgroup user-extensions nil
  "Group for user-defined extensions."
  :group 'emacs)

(defface log-view-info
  '((t (:foreground "Blue1" :weight bold)))
  "Face for DEBUG in log files."
  :group 'user-extensions)

(defface log-view-debug
  '((t (:inherit success)))
  "Face for DEBUG in log files."
  :group 'user-extensions)

(defface log-view-error
  '((t (:inherit error :weight bold)))
  "Face for ERROR in log files"
  :group 'user-extensions)

(defface log-view-warning
  '((t (:inherit warning :weight bold)))
  "Face for WARNING in log files"
  :group 'user-extensions)

(defface log-view-date
  '((t (:foreground "red")))
  "Face for date strings in log files."
  :group 'user-extensions)

(defface log-view-time
  '((t (:inherit org-agenda-current-time)))
  "Face for time strings in log files."
  :group 'user-extensions)

;;; Functions

(defun add-mode-comment (mode)
  "Insert a comment line that changes the major mode to MODE.
When called interactively, it prompts the user for MODE."
  (interactive "sMode: ")
  (insert (format "# -*- mode: %s; -*-" mode)))

(defun bind-fill-region ()
  "Binds `fill-region' to M-F."
  (interactive)
  (local-set-key (kbd "M-F") #'fill-region)
  (message "Bind `fill-region' to M-F."))

(defun copy-line ()
  "Call `kill-ring-save' on the current line from point."
  (interactive)
  (let ((pos (point-marker)))
    (end-of-line)
    (kill-ring-save pos (point))
    (goto-char pos)))

(defun count-words-region2 (start end)
  "Count the number of characters in region.
START and END are expected to come directly from the region.
Call `count-words-region' and"
  (interactive "r")
  (if (not (use-region-p))
      (error "region required")
    (count-words--message "Region" start end)
    (setq deactivate-mark t)
    nil))

(defun date-format-version ()
  "Formats a version string in YYYYMMDD.HHMM format."
  (interactive)
  (insert (format-time-string "%Y%m%d.%H%M")))

(defun enable-codeium-completion ()
  "Add `codeium-completion-at-point' to `completion-at-point-functions'."
  (interactive)
  (cl-pushnew #'codeium-completion-at-point completion-at-point-functions))

(defun enable-wrap ()
  (interactive)
  (if (and (boundp 'visual-line-mode) visual-line-mode)
      (error "Already called this command")
    (visual-line-mode t)))

(defun kill-and-quit ()
  "Kills the current buffer and also deletes the current window.
The equivelent of doing C-x k and C-x 0."
  (interactive)
  (kill-buffer)
  (delete-window))

;; (defun list-contains (element list)
;;   "Return non-nil if ELEMENT is in LIST."
;;   (cl-find element list :test #'equal))

(defun narrow-to-region2 (start end)
  "Narrow to region and cancel region. START and END specify the region to narrow
to."
  (interactive "r")
  (narrow-to-region start end)
  (setq deactivate-mark t))

(defun p-mode ()
  "Activate electric-pair mode. Calls `electric-pair-local-mode'."
  (interactive)
  (when (null electric-pair-mode)
    (electric-pair-local-mode 1)
    (message "localized electric pair mode activated")))

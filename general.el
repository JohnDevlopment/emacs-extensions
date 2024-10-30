;;; general --- General extension functions.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Abbreviations

(define-abbrev emacs-lisp-mode-abbrev-table "propline"
  "" (lambda ()
       (interactive)
       (call-interactively #'add-file-local-variable-prop-line)))

(eval-and-compile
  (defvar-local user-ext-local-position-ring nil
    "Current file's mark ring.")
  (require 'debug-ext))

;; Functions

(defun save-current-position (&optional pos)
  "Save POS to the local position ring.
Unless POS is provided, point is used."
  (interactive)
  (let ((pos (or pos (point-marker))))
    (add-to-history 'user-ext-local-position-ring pos)
    (message
     (substitute-command-keys
      "Position saved to local position ring. Go back with `\\[pop-saved-position]'."))))

(defun pop-saved-position ()
  "Move point to the last saved position.
This pops the last-saved position from
`user-ext-local-position-ring'."
  (interactive)
  (let (pos)
    (assert (> (length user-ext-local-position-ring) 0)
	"The local position ring is empty!")
    (setq pos (pop user-ext-local-position-ring))
    (goto-char pos)
    (message "Restored to position %s." pos)))

(defun add-mode-comment (mode)
  "Insert a comment line to change the major mode to MODE.
When called interactively, it prompts the user for MODE."
  (interactive "sMode: ")
  (insert (format "# -*- mode: %s; -*-" mode)))

(defun bind-fill-region ()
  "Bind `fill-region' to M-F."
  (interactive)
  (local-set-key (kbd "M-F") #'fill-region)
  (message "Bind `fill-region' to M-F."))

(defun copy-line ()
  "Copy characters from point to the end of the line.
Unlike `kill-line', this does not delete the characters."
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
      (error "Region required")
    (count-words--message "Region" start end)
    (setq deactivate-mark t)
    nil))

(defun date-format-version ()
  "Formats a version string in YYYYMMDD.HHMM format."
  (interactive)
  (insert (format-time-string "%Y%m%d.%H%M")))

(defun enable-wrap ()
  "Enable line wrap if it is not already."
  (interactive)
  (if (and (boundp 'visual-line-mode) visual-line-mode)
      (error "Already called this command")
    (visual-line-mode t)))

(defun kill-and-quit ()
  "Kill the current buffer and also delete the current window.
The equivelent of doing C-x k and C-x 0."
  (interactive)
  (kill-buffer)
  (delete-window))

(defun narrow-to-region2 (start end)
  "Narrow to region and cancel region.
START and END specify the region to narrow to."
  (interactive "r")
  (narrow-to-region start end)
  (setq deactivate-mark t))

(provide 'general)

;;; general ends here

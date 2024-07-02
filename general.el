;; -*- lexical-binding: t; -*-

(eval-and-compile
  (require 'codeium))

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

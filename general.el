;;; general --- General extension functions.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Abbreviations

(require 'cl-lib)
(require 'cl-ext)
(require 'debug-ext)

(define-abbrev emacs-lisp-mode-abbrev-table "propline"
  "" (lambda ()
       (interactive)
       (call-interactively #'add-file-local-variable-prop-line)))

;; Variables

(defvar-local user-ext-local-position-ring nil
  "Current file's mark ring.")

;; Functions

(defun sort-words (separators start end)
  "Sort words in buffer between START and END via SEPARATORS.
SEPARATORS is a regular expression used to match the
characters used to separate the words.  START and END denote
the region of text to sort.

Interactively, START and END are the region."
  (interactive "*sSeparator: \nr")
  (let* ((content (buffer-substring-no-properties start end))
	 (sep (if (string-match separators content)
		  (match-string 0 content)
		" "))
	 (words (split-string content separators t))
	 (swords (cl-sort words #'string-lessp)))
    (deactivate-mark)
    (delete-region start end)
    (insert (string-join swords sep))))

(defun print-saved-positions ()
  (interactive)
  (cl-assert (> (length user-ext-local-position-ring) 0) t
	     "The local position ring is empty!")
  (let (msg)
    (setq msg
	  (string-join
	   (cl-loop
	    with i = 0
	    for pos in user-ext-local-position-ring
	    collect
	    (format "%d. %s" (cl-incf i) pos))
	   "\n"))
    (message msg)))

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
    (cl-assert (> (length user-ext-local-position-ring) 0) t
	       "The local position ring is empty!")
    (setq pos (pop user-ext-local-position-ring))
    (goto-char pos)
    (message "Restored to position %s." pos)))

(defun --add-mode-comment--mode-without-suffix (obj)
  (let (str)
    (when (or (stringp obj) (symbolp obj))
      (setq str (if (stringp obj) obj
		  (symbol-name obj)))
      (when (string-match "\\(.+\\)-mode$" str)
	(intern (match-string 1 str))))))

(defun --add-mode-comment--complete-mode ()
  (let* ((filt (lambda (e)
		 (string-match-p "-mode\\'"
				 (if (symbolp e) (symbol-name e) e))))
	 (str (completing-read
	       "Mode: " obarray filt t)))
    (--add-mode-comment--mode-without-suffix str)))

(defun add-mode-comment ()
  "Insert a comment line to change the major mode to MODE.
When called interactively, it prompts the user for MODE."
  (interactive)
  (let ((mode (--add-mode-comment--complete-mode)))
    (message "%s" mode)
    (add-file-local-variable-prop-line 'mode mode t)))

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

(defun save-and-kill ()
  "Save the current buffer and then kill it."
  (interactive)
  (save-buffer)
  (kill-buffer))

(defun kill-and-quit (&optional arg)
  "Kill the current buffer and also delete the current window.
The equivelent of doing C-x k and C-x 0."
  (interactive "P")
  (kill-buffer)
  (if arg
      (delete-window)
    (quit-window)))

(defun narrow-to-region2 (start end)
  "Narrow to region and cancel region.
START and END specify the region to narrow to."
  (interactive "r")
  (narrow-to-region start end)
  (setq deactivate-mark t))

(provide 'general)

;;; general ends here

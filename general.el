;; -*- lexical-binding: t; -*-

;; Errors

(eval-when-compile
  (require 'embed-doc)
  (declare-function elisp-ext-minify "elisp-ext" (start end)))

(embed-doc-document-symbol
    general
  "General functions, commands, and types."
  :commands
  bind-fill-region
  capf-enable-ispell-locally
  company-enable-ispell-locally
  copy-line
  count-words-region2
  date-format-version
  describe-region
  enable-wrap
  kill-and-quit
  pop-saved-position
  print-saved-positions
  save-and-kill
  save-current-position
  sort-words
  :functions
  signal-wrong-argument)

;; ### Variables

(defvar temp t "A temporary variable to be used anywhere.")

(defvar-local user-ext-local-position-ring nil
  "Current buffer's mark ring.")

;; ### Advice

(fext-defadvice narrow-to-region (after narrow-to-region)
  (deactivate-mark))

;; ### Functions

(defun --make-sure-user-actually-wants-to-quit ()
  (y-or-n-p "Did you actually mean to quit?"))

(defun describe-region (&optional beg end)
  "Describe the region."
  (interactive "r")
  (cond ((use-region-p)
	 (message "Region: %d %d" beg end))
	(t (message "There is no region")))
  (deactivate-mark))

(defun company-enable-ispell-locally ()
  "Add `company-ispell' to the local binding of `company-backends'."
  (interactive)
  (setq-local company-backends (cons #'company-ispell company-backends)))

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
  "Print the positions that are currently in the local ring.
This merely prints the contents of `user-ext-local-position-ring'."
  (interactive)
  (cl-assert (>= (length user-ext-local-position-ring) 0) t)
  (if (= (length user-ext-local-position-ring) 0)
      (message "The local position ring is empty")
    (let (msg)
      (setq msg
	    (string-join
	     (cl-loop
	      with i = 0
	      for pos in user-ext-local-position-ring
	      collect
	      (format "%d. %s" (cl-incf i) pos))
	     "\n"))
      (message msg))))

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

(defalias 'minify #'elisp-ext-minify)

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
  ;; TODO: Change to `cl-ext-unless'
  (if (not (use-region-p))
      (error "Region required")
    (count-words--message "Region" start end)
    (setq deactivate-mark t)
    nil))

(defun date-format-version ()
  "Formats a version string in YYYYMMDD.HHMM format."
  (interactive)
  (insert (format-time-string "%Y%m%d.%H%M")))
(make-obsolete 'date-format-version nil "2025-01-20")

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
The equivelent of doing \\[kill-buffer] and
\\[quit-window].  If ARG is non-nil, kill the selected
window instead of quitting it.

Interactively, ARG is the prefix argument."
  (interactive "P")
  (if arg
      (progn
	(kill-buffer)
	(delete-window))
    (quit-window t)))

(provide 'general)
;;; general ends here

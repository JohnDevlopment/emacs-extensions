;; -*- lexical-binding: t; -*-

;; Errors

(defun signal-wrong-argument (arg valid)
  "Signal an error to indicate an incorrect argument.
ARG is the argument that caused the error and VALID denotes
the accepted value(s).

If VALID is a list of two elements, it is interpreted as a
range and the message is formatted accordingly.  If the list
is 3 or more elements long, then it is formatted into a
comma-separated list (as a string).  Otherwise, VALID is
treated as a single value and formatted appropriately."
  (let (msg)
    (setq msg (pcase valid
		((pred listp) (format "Valid: %S," valid))
    		(`(,x ,y) (format "Valid range: %S - %S" x y))
    		(_ (format "Valid: %S" valid))))

    (setq msg (if (cl-typep valid 'list)
		  (pcase (length valid)
		    (1 (format "Valid: %S" (car valid)))
		    (2 (format "Valid range: %S-%S" (car valid) (car (cdr valid))))
		    (_
		     (setq valid (mapcar #'prin1-to-string valid))
		     (format "Valid: %s" (string-join valid ", "))))
		(format "Valid: %S" valid)))
    (signal 'wrong-argument (list arg msg))))

(define-error 'invalid-argument "Invalid argument")
(define-error 'wrong-argument "Wrong argument" 'invalid-argument)

;; Variables

(eval-when-compile
  (require 'documentation-ext)
  (document-extension "general"
    "General extension functions."
    :functions
    (add-mode-comment
     bind-fill-region
     copy-line
     count-words-region2
     date-format-version
     enable-wrap
     kill-and-quit
     narrow-to-region2

     pop-saved-position
     print-saved-positions
     save-current-position

     save-and-kill
     signal-wrong-argument
     sort-words
     wrap)

    :variables
    ((user-ext-local-position-ring local))))

(eval-when-compile
  (declare-function elisp-ext-minify "elisp-ext" (start end)))

(defvar-local user-ext-local-position-ring nil
  "Current file's mark ring.")

;; Functions

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

(defun wrap (n min max)
  "Clamp N to the range [MIN,MAX], wrapping it around as needed.
That is to say, if N is less than MIN, it is wrapped around
to MAX.  If N is greater than MAX, it is wrapped around to
MIN."
  (declare (pure t))
  (let (diff)
    (cond
     ((< n min)
      (setq diff (- min n 1))
      (cl-assert (>= diff 0))
      (- max diff))
     ((> n max)
      (setq diff (- n max 1))
      (cl-assert (>= diff 0))
      (+ min diff))
     (t n))))

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

(defun narrow-to-region2 (start end)
  "Narrow to region and cancel region.
START and END specify the region to narrow to."
  (interactive "r")
  (narrow-to-region start end)
  (setq deactivate-mark t))

(provide 'general)

;;; general ends here

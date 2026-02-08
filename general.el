;; -*- lexical-binding: t; -*-

(check-emacs-minimum-version "27.1")

(require 'debug-ext)

(eval-when-compile
  (require 'function-ext)
  (declare-function elisp-ext-minify "elisp-ext" (start end)))

(require 'embed-doc)

(embed-doc-document-symbol
    general
  "General all-purpose functions and commands.

This extension advises several commands:
- `backward-page'
- `forward-page'
- `narrow-to-region'

The above commands are turned into \"cycling\" commands;
that is, a transient map is activated to make them available
to use without their respective prefixes.

\\{user-ext-page-motion-transient-map}"
  :commands
  copy-line
  count-words-region2
  describe-region
  enable-wrap
  kill-and-quit
  minify
  pop-saved-position
  print-saved-positions
  save-and-kill
  save-current-position
  sleep
  upcase-insert
  yank-and-indent
  :functions
  funcall-safe
  in-hook-p)

(define-fringe-bitmap
  'saved-position-fringe
  [96 144 144 144 96 96 146 140 140 114]
  nil 9)


;; ### Customization

(defgroup general nil
  "General extension group."
  :group 'user-extensions)

(defface general-saved-position-face
  '((t (:inherit default)))
  "Face for saved position fringe."
  :group 'general)


;; ### Variables

(defvar-local user-ext-local-position-ring nil
  "Current buffer's mark ring.")

(defconst user-ext-page-motion-transient-map
  (let ((map (make-sparse-keymap "Page Motion: ")))
    (define-key map (kbd "[") #'backward-page)
    (define-key map (kbd "]") #'forward-page)
    map)
  "Transient keymap for page motion commands.

\\{user-ext-page-motion-transient-map}")


;; ### Advice

(fext-defadvice narrow-to-region (after narrow-to-region)
  (deactivate-mark))


;; --- Page Motion

(defun advice-ext--after-page-motion (&rest _r)
  (set-transient-map user-ext-page-motion-transient-map))

(advice-add 'backward-page :after #'advice-ext--after-page-motion)
(advice-add 'forward-page :after #'advice-ext--after-page-motion)


;; ### Functions

(defsubst funcall-safe (func &rest args)
  "Call FUNCTION with the remaining args.
If FUNCTION is nil, do nothing.
Return the value FUNCTION returns.

\(fn FUNCTION &rest ARGUMENTS)"
  (cl-check-type func (or symbol function))
  (and func (functionp func) (apply func args)))

(defun in-hook-p (hook function &optional local)
  "Return non-nil if FUNCTION is added to HOOK.
If LOCAL is non-nil, check the buffer-local value of HOOK,
otherwise check the global value.

In either case, if the respective value of HOOK is void,
this returns nil."
  (declare (side-effect-free t))
  (cl-check-type hook symbol)
  (cl-check-type function (or symbol function))
  (and (or (and local (buffer-local-boundp hook (current-buffer)))
	   (and (not local) (default-boundp hook)))
       (let ((hook-value (if local (symbol-value hook) (default-value hook))))
	 (when (memq function hook-value)
	   (cons function local)))))


;; --- General commands

(defun enable-wrap ()
  "Enable line wrap if it is not already."
  (interactive)
  (if (bound-and-true-p visual-line-mode)
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

(defun sleep (seconds)
  "Pause for SECONDS seconds."
  (interactive "nSeconds To Sleep For: ")
  (cl-check-type seconds (or integer float))
  (sleep-for seconds))

(defun minify (start end &optional force)
  "Minify the text between START and END in current buffer.
START and END are the two points in a region.  If the region
is not active, minify the whole buffer, asking the user
beforehand; unless FORCE is non-nil, in which, do it without
asking.

If called interactively, START and END are the region,
provided the region is active, otherwise they are ignored.
FORCE is the prefix argument."
  (interactive "r\nP")
  (cl-block quit
    (let ((msg "The region is not active, so the entire buffer will be minified. Continue?")
	  (reg (region-active-p))
	  answer
	  bstr)
      (setq answer (or reg force (y-or-n-p msg)))
      (cond
       ((and (not reg) (not answer))
	;; Quit; user said no and the region is not active
	(message "Quit")
	(cl-return-from quit))
       ((and (not reg) answer)
	;; User said yes but the region is not active
	(setq start (point-min) end (point-max))))
      (setq bstr (buffer-substring-no-properties start end))
      (delete-region start end)
      (setq bstr (replace-regexp-in-string "[ \t\n\r]+" " " bstr))
      (insert bstr))))

(defun activate-view-mode (&optional arg)
  "Activate view mode in current buffer and maybe setup exit actions.
If ARG is non-nil, setup buffer to be killed when View mode
is exited, similar to the behavior of `view-buffer' or
`view-file'.

Interactively, ARG is the raw prefix argument."
  (interactive "P")
  (if arg (view-mode-enter nil #'kill-buffer-if-not-modified)
    (view-mode-enter)))


;; --- Copy and insertion commands

(defun upcase-insert (obj)
  "Convert argument to upper case and insert that.
The lone argument is the string to insert.  If the string is
in uppercase (usually because of caps lock), convert it to
lowercase instead.

When called interactively, prompt the user for the argument."
  (interactive "sInput: ")
  (if (s-uppercase? obj)
      (insert (downcase obj))
    (insert (upcase obj))))

(defun copy-line ()
  "Copy characters from point to the end of the line.
Unlike `kill-line', this does not delete the characters."
  (interactive)
  (let ((pos (point-marker)))
    (end-of-line)
    (kill-ring-save pos (point))
    (goto-char pos)))

(defun yank-and-indent ()
  "Reinsert (\"paste\") the last stretch of killed text and indent it."
  (interactive)
  (let (beg end)
    (setq beg (point)
	  end (save-excursion
		(call-interactively #'yank)
		(point)))
    (indent-region beg end)))



;; --- Region-related commands

(defun describe-region (&optional beg end)
  "Describe the region."
  (interactive "r")
  (cond ((use-region-p)
	 (message "Region: %d %d" beg end))
	(t (message "There is no region")))
  (deactivate-mark))

(eval-and-compile
  (unless (fboundp 'count-words--format)
    (signal-extension-error "`count-words--format' not defined")))

(defun count-words-region2 (start end)
  "Count the number of characters in region.
START and END are expected to come directly from the region.
Call `count-words-region' and"
  (interactive "r")
  (unless (use-region-p)
    (error "Region required"))
  (prog1 nil
    (message (count-words--format "Region" start end))
    (setq deactivate-mark t)))

(defun bind-fill-region ()
  "Bind `fill-region' to M-F."
  (interactive)
  (local-set-key (kbd "M-F") #'fill-region)
  (message "Bind `fill-region' to M-F."))
(put #'bind-fill-region 'disabled t)


;; --- Position commands

(defun print-saved-positions ()
  "Print the positions that are currently in the local ring.
This merely prints the contents of `user-ext-local-position-ring'."
  (interactive)
  (cl-assert (>= (length user-ext-local-position-ring) 0) t)
  (unless (> (length user-ext-local-position-ring) 0)
    (error "The local position ring is empty"))
  (let ((msg (string-join
	      (cl-loop with i = 0
		       for pos in user-ext-local-position-ring
		       collect
		       (format "%d. %s" (cl-incf i) pos))
	      "\n")))
    (message msg)))

(defun general--make-saved-position-overlay (pos)
  (let ((ov (make-overlay pos (1+ pos))))
    (condition-case-unless-debug err
	(progn
	  (overlay-put ov 'category 'saved-position)
	  (overlay-put ov 'evaporate t)
	  (overlay-put ov 'before-string
		       (propertize
			"x" 'display
			'(left-fringe
			  saved-position-fringe
			  general-saved-position-face))))
      (error (message "Error deleting overlay: %S" err)
	     (delete-overlay ov)))))

(defun general--delete-saved-position-overlay (pos)
  (let ((ovs (overlays-at pos))
	found ov)
    (while (and (not found) ovs)
      (setq ov (pop ovs))
      (when (eq (overlay-get ov 'category) 'saved-position)
	(delete-overlay (setq found ov))))))

(defun save-current-position (&optional pos)
  "Save POS to the local position ring.
Unless POS is provided, point is used."
  (interactive)
  (let ((pos (or pos (point-marker))))
    (add-to-history 'user-ext-local-position-ring pos)
    (general--make-saved-position-overlay (line-beginning-position))
    (message
     (substitute-command-keys
      "Position saved to local position ring. Go back with `\\[pop-saved-position]'."))))

(defun pop-saved-position ()
  "Move point to the last saved position.
This pops the last-saved position from
`user-ext-local-position-ring'."
  (interactive)
  (let (pos)
    (cl-assert (>= (length user-ext-local-position-ring) 0) t)
    (or (> (length user-ext-local-position-ring) 0)
	(error "The local position ring is empty"))
    (setq pos (pop user-ext-local-position-ring))
    (goto-char pos)
    (general--delete-saved-position-overlay (line-beginning-position))
    (message "Restored to position %s." pos)))


(extension-provide 'general)
;;; general ends here

;; -*- lexical-binding: t; -*-

;; ### Functions

;; ---Create scratch or temp buffers

;;;###autoload
(defmacro define-scratch-buffer-function (name buffer-name arg-list
					       docstring int-spec
					       &rest body)
  "Define a function NAME for creating a scratch buffer.
The scratch buffer is named \"BUFFER-NAME\".  NAME has
ARG-LIST and DOCSTRING as its argument list and
documentation string, respectively.  Likewise, INT-SPEC is
used as the argument to `interactive' if non-nil.  The rest
of the arguments is the BODY of function NAME.

BODY is evaluated with the scratch buffer as the current one.
The last form of BODY is returned by the defined function."
  (declare (indent 3) (doc-string 4)
	   (debug (&define name stringp (&rest arg)
			   stringp interactive def-body)))
  `(progn
     (defun ,name ,arg-list
       ,docstring
       ,(cond ((eq int-spec 'noninteractive)
	       nil)
	      (int-spec `(interactive ,int-spec))
	      (t '(interactive)))
       (let (buffer window)
	 (setq buffer (tmpbuf ,buffer-name))
	 (unwind-protect
	     (with-current-buffer buffer
	       ,@body)
	   (setq window (display-buffer
			 buffer
			 '((display-buffer--maybe-same-window
			    display-buffer-reuse-window
			    display-buffer--maybe-pop-up-frame-or-window)
			   (frame . nil)
			   (dedicated . t))))
	   (select-window window))))))

(defun text-scratch--complete-mode ()
  (let* ((filt (lambda (e)
		 (string-match-p "-mode\\'"
				 (if (symbolp e) (symbol-name e) e))))
	 (str (completing-read
	       "Mode: " obarray filt t)))
    (intern str)))

;;;###autoload (autoload 'faces-buffer "_buffers-ext" "Open a buffer listing all the faces.\n\n\(fn)" t)
(define-scratch-buffer-function faces-buffer "faces" nil
  "Open a buffer listing all the faces."
  nil
  (let ((str "The quick brown fox jumped over the lazy dog.")
	fmt faces col1)
    (setq faces (seq-map 'symbol-name (face-list))
	  col1 (apply #'max (mapcar (lambda (x) (length x)) faces))
	  fmt (format "%%-%ds %%s" col1))
    (insert (format fmt "Face" "Display\n"))
    (dolist (face (seq-sort #'string< faces))
      (insert (format fmt face (propertize str 'face (intern-soft face))))
      (newline)))
  (read-only-mode 1)
  (local-set-key (kbd "q") #'quit-window)
  (local-set-key (kbd "k") #'kill-and-quit))

;;;###autoload (autoload 'general-scratch "_buffers-ext" "Open a general-purpose scratch buffer.\n\n\(fn MODE)" t)
(define-scratch-buffer-function general-scratch "general scratch" (mode)
  "Open a general-purpose scratch buffer.
The buffer will have its major mode set to `text-mode' by
default, unless ARG is non-nil, in which case the major mode
is set to MODE.

Interactively, ARG is the prefix argument. The user is
prompted for MODE."
  (list  (if current-prefix-arg
	     (text-scratch--complete-mode)
	   'text-mode))
  (cl-assert (symbolp mode) t (prin1-to-string mode))
  (funcall mode)
  (general-scratch-mode 1))

(defun general-scratch--quit ()
  "Close the scratch buffer."
  (interactive)
  (cl-ext-unless (string-empty-p (buffer-string))
    (kill-region (point-min) (point-max)))
  (kill-and-quit))

(defun general-scratch--disable ()
  (general-scratch-mode 0))

(define-minor-mode general-scratch-mode
  "Minor mode for `general-scratch'.

\\{general-scratch-mode-map}"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "C-c C-c") #'general-scratch--quit)
	    map)
  :lighter " Scratch"
  (if general-scratch-mode
      (cl-ext-progn
	(setq header-line-format "Type C-c C-c to exit.")
	(add-hook 'after-save-hook #'general-scratch--disable nil t))
    (setq header-line-format nil)
    (remove-hook 'after-save-hook #'general-scratch--disable t)))

;;;###autoload
(defun tmpbuf (buffer-name &optional switch)
  "Open a temporary buffer with the name BUFFER-NAME.
Create the buffer if it does not already exist.  If SWITCH is
non-nil, switch to the newly buffer.

If this is called interactively, switch to the buffer."
  (interactive "sBuffer name: \nP")
  (let ((newbuf (get-buffer-create (format "*%s*" buffer-name))))
    (if (or (called-interactively-p 'any) switch)
	(switch-to-buffer newbuf)
      newbuf)))

;; ---Kill buffers matching patterns

;;;###autoload
(defun kill-certain-temp-buffers ()
  "Convenience function to kill certain buffers you do not need.

This kills buffers belonging to `user-ext-temp-buffers-to-kill' and
`user-ext-temp-buffers-to-kill-regex'."
  (interactive)
  (let ((bl user-ext-temp-buffers-to-kill)
	(rl user-ext-temp-buffers-to-kill-regex)
	buf)
    ;; Kill buffers in `user-ext-temp-buffers-to-kill'
    (dolist (bn bl)
      (setq buf (get-buffer bn))
      (when buf
	(kill-buffer buf)
	(message "killed %s" bn)))
    (dolist (regexp rl)
      (kill-buffers regexp))))

;;;###autoload
(defun kill-customization-buffers ()
  "Close all customization buffers.
Internally, calls `kill-buffers' with \"^\*Customize.*\" as the pattern."
  (interactive)
  (kill-buffers "\\*Customize.*"))

;;;###autoload
(defun kill-buffers (pattern &optional predicate)
  "Close all buffers matching PATTERN.
If PREDICATE is specified, it is a function that accepts a
buffer object and returns a non-nil value if said buffer
should be killed.

Called interactively, PREDICATE cannot be specified."
  (interactive "sRegexp: ")
  (let ((bl (buffer-list))
	idx)
    (dolist (buf bl)
      (setq idx (string-match-p pattern
				(buffer-name buf)))
      (when (and (integerp idx)
		 (or (null predicate)
		     (funcall predicate buf)))
	(message "Killed %s" (buffer-name buf))
	(kill-buffer (buffer-name buf))))))

;; --- Other functions

;;;###autoload
(defun clone-indirect-buffer-this-window ()
  "Create an indirect buffer that is a twin copy of the current buffer."
  (interactive)
  (cl-loop
   named make-ind-buf
   with buf = nil
   with bufname = (buffer-name (current-buffer))
   for i from 2 to 9
   do
   (setq buf (format "%s<ind-%d>" bufname i))
   if (null (get-buffer buf))
   do
   (setq buf (make-indirect-buffer (current-buffer) buf t))
   (switch-to-buffer buf)
   (cl-return-from make-ind-buf)
   finally do
   (error "Limit of 8 indirect buffers reached")))

;;;###autoload
(defun revert-all-buffers ()
  "Reverts all buffers, including special buffers.
This reverts all buffers in like manner to `revert-buffer'.  The user is NOT
asked to confirm, so be careful when using this function.  IGNORE-AUTO and
PREVERSE-MODES are the same as for `revert-buffer', and they are specified
as prefix args."
  (interactive)
  (let ()
    (dolist (buf (buffer-list))
      (with-current-buffer buf
	(when (and (buffer-file-name)
		   (file-exists-p (buffer-file-name))
		   (not (buffer-modified-p)))
	  (revert-buffer t t))))
    (message "Reverted all file buffers")))

;;;###autoload
(defun activate-view-mode (&optional arg)
  "Activate view mode in current buffer and maybe setup exit actions.
If ARG is non-nil, setup buffer to be killed when View mode
is exited, similar to the behavior of `view-buffer' or
`view-file'.

Interactively, ARG is the raw prefix argument."
  (interactive "P")
  (if arg
      (view-mode-enter nil #'kill-buffer-if-not-modified)
    (view-mode-enter)))

;;;###autoload
(defun clone-and-view-buffer (base-buffer &optional clone)
  "Create an indirect buffer and show it in another window.

Creates an indirect buffer of BASE-BUFFER and shows it in
another window.  BASE-BUFFER should be a live buffer or the
name of an existing buffer.  If the optional arg CLONE is
non-nil, BASE-BUFFER's state is preserved in the indirect
buffer, meaning things like minor and major modes.  Otherwise,
the indirect buffer's state is reset to default values.

Interactively, CLONE is a prefix argument."
  (interactive "bMake indirect buffer of: \nP")
  (let ((vb (format "*view into %s*" base-buffer)))
    (make-indirect-buffer base-buffer vb clone)
    (display-buffer vb)
    (with-current-buffer vb
      (view-mode-enter nil #'kill-buffer-if-not-modified))))

(provide '_buffers-ext)
;;; _buffers-ext.el ends here

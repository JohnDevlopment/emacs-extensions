;;; -*- lexical-binding: t; -*-

(check-emacs-minimum-version "27.1")

(require 'cl-lib)
(require 'custom)

(eval-and-compile
  (embed-doc-document-symbol buffers-ext
    "Buffer functions."

    :customs
    user-ext-temp-buffers-to-kill
    user-ext-temp-buffers-to-kill-regex

    :commands
    kill-buffers
    kill-certain-temp-buffers
    kill-customization-buffers

    tmpbuf
    

    :functions
    buffer-string-no-properties
    with-tmpbuf))

;; ### Customization

(defgroup buffers-ext nil
  "Group for custom buffer-related options."
  :group 'user-extensions)

;; TODO: Add a setter that compiles this list into a regular expression so the functions can use that
(defcustom user-ext-temp-buffers-to-kill
  '("*Backtrace*" "*comment-tags*"
    "*Buffer List*"
    "*Completions*"
    "*Compile-Log*"
    "*Occur*"
    "*Packages*"
    "*Shell Command Output*")
  "A list of buffers to kill when `kill-certain-temp-buffers' is called."
  :type '(repeat string)
  :safe 'listp
  :group 'buffers-ext)

(defcustom user-ext-temp-buffers-to-kill-regex
  '("\\*.*[hH]elp.*" "\\*vc-.*")
  "A list of regexes of buffers to kill when `kill-certain-temp-buffers' is called."
  :type '(repeat regexp)
  :safe 'listp
  :group 'buffers-ext)


;; ### Variables

(defvar user-ext-text-scratch--mode-cache
  '(0 nil)
  "Cache for `text-scratch--complete-mode'.
Has the form (COUNT STRINGS), where COUNT is the number of
elements in `obarray', and STRINGS is the list of strings
converted from `obarray'.")

(defvar user-ext-text-scratch--mode-history nil
  "History for `text-scratch--complete-mode'.")


;; ### Functions

;; TODO: Move to "windows-ext.el"
(defun set-current-window-dedicated (window flag)
  "Set the dedicated status of the WINDOW to FLAG.
The exact behavior depends on FLAG:
- If FLAG is 1 or the symbol `toggle', flip the window's
dedicated status between on and off.
- If it is 4 or t, then set the window's dedicated status.
- If it is 16 or nil, then clear the window's dedicated
status.

When called interfactively, FLAG is the numeric value of the
prefix argument:
- No prefix arg to toggle the window's status.
- \\[universal-argument] to set it.
- \\[universal-argument] \\[universal-argument] to clear it."
  (interactive (list (selected-window)
		     (prefix-numeric-value current-prefix-arg)))
  (let ((msg (pcase flag
	       ((or 1 'toggle)
		(if (window-dedicated-p window)
		    "Released window from being dedicated"
		  "Set window to be dedicated"))
	       ((or 4 't)
		"Set window to be dedicated")
	       ((or 16 'nil)
		"Released window from being dedicated"))))
    (set-window-dedicated-p
     window
     (pcase flag
       ((or 1 'toggle) (not (window-dedicated-p window)))
       ((or 4 't) t)
       ((or 16 'nil) nil)))
    (message msg)))

(defmacro with-tmpbuf (buffer-name &rest body)
  "Create a temporary buffer and evaluate BODY like `progn'.
The buffer's name is generated using `generate-new-buffer-name'
with BUFFER-NAME as the argument.

The buffer is killed at the end once all BODY forms are
evaluated."
  (declare (indent 1) (debug t))
  (cl-check-type buffer-name string)
  `(let ((buffer (tmpbuf ,buffer-name nil t)))
     (unwind-protect
	 (progn
	   (save-current-buffer
	     (set-buffer buffer)
	     ,@body))
       (kill-buffer buffer))))

;; TODO: Possibly MOVE to "files-ext" or "subr-ext"
;; (defmacro with-temp-file-deleted-after (file body-form &rest unwind-body)
;;   "Create FILE, do BODYFORM, then do UNWINDFORMS and delete FILE.
;; FILE can be either a string or a list of the form
;; (BASE SUFFIX [TEXT]).
;; BODYFORM is either a single form or multiple forms wrapped
;; in something like `progn'.
;; After BODYFORM, evaluate UNWINDFORMS.

;; BODYFORM is wrapped in `unwind-protect', and UNWINDFORMS
;; corresponds to its same-name argument.

;; During the evaluation of BODYFORM, the buffer associated
;; with FILE is the current buffer.

;; After UNWINDFORMS is evaluated, FILE is deleted and the
;; buffer associated with it is killed.

;; See also: `make-temp-file'.

;; \(fn FILE BODYFORM UNWINDFORMS...)"
;;   (declare (indent 2)
;; 	   (debug ([&or stringp
;; 			(stringp stringp &optional stringp)]
;; 		   [&or ("progn" &rest form)
;; 			form]
;; 		   &rest form)))
;;   (let ((body (pcase body-form
;; 		(`(,fn . ,body1)
;; 		 (if (special-form-p (car (macroexpand-1 body-form)))
;; 		     (cons fn body1)
;; 		   `(progn (,fn ,@body1))))
;; 		(bf bf)))
;; 	(args (cl-typecase file
;; 		(string (list file))
;; 		(list (cl-destructuring-bind
;; 			  (base suffix &optional text)
;; 			  file
;; 			(list nil suffix text)
;; 			;; (let* ((args file)
;; 			;;        (base (pop args))
;; 			;;        (suffix (cadr args)))
;; 			;;   (list base nil suffix))
;; 			))
;; 		(otherwise
;; 		 (signal 'wrong-type-argument
;; 			 (list '(or string list) file 'file))))))
;;     `(let* ((temp-file (make-temp-file ,@args))
;; 	    (temp-buffer (find-file-noselect temp-file t)))
;;        (unwind-protect
;; 	   (with-current-buffer temp-buffer
;; 	     ,body)
;; 	 ,@unwind-body
;; 	 (and (buffer-name temp-buffer)
;; 	      (kill-buffer temp-buffer))
;; 	 (ignore-error file-error
;; 	   (delete-file temp-file))))))
;; (put 'with-temp-file-deleted-after 'disabled t)

(defun buffer-string-no-properties ()
  "Return the current buffer's contents without text properties.

If narrowing is in effect, this function returns only the
visible part of the buffer."
  (declare (side-effect-free t))
  (let ((bstr (buffer-string)))
    (set-text-properties 0 (length bstr) nil bstr)
    bstr))
(function-put 'buffer-string-no-properties 'error-free t)

(defun tmpbuf (buffer-name &optional switch unique)
  "Open a temporary buffer with the name BUFFER-NAME.
Create the buffer if it does not already exist.
If SWITCH is non-nil, switch to the newly buffer.
If UNIQUE is non-nil, create the buffer using `generate-new-buffer'.

If this is called interactively, switch to the buffer."
  (interactive "sBuffer name: \nP")
  (let ((newbuf (if unique
		    (generate-new-buffer (format "*%s*" buffer-name))
		  (get-buffer-create (format "*%s*" buffer-name)))))
    (if (or (called-interactively-p 'any) switch)
	(switch-to-buffer newbuf)
      newbuf)))

(defun revert-all-buffers ()
  "Reverts all buffers, including special buffers.
This reverts all buffers in like manner to `revert-buffer'.  The user is NOT
asked to confirm, so be careful when using this function.  IGNORE-AUTO and
PREVERSE-MODES are the same as for `revert-buffer', and they are specified
as prefix args."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name)
		 (file-exists-p (buffer-file-name))
		 (not (buffer-modified-p)))
	(revert-buffer t t))))
  (message "Reverted all file buffers"))


;; --- Kill buffers matching pattern

(defun kill-buffers (pattern &optional predicate)
  "Close all buffers matching PATTERN.
If PREDICATE is specified, it is a function that accepts a
buffer object and returns a non-nil value if said buffer
should be killed.

Called interactively, PREDICATE cannot be specified."
  (interactive "sRegexp: ")
  (let ((bl (buffer-list))
	idx bn)
    (dolist (buf bl)
      (setq idx (string-match-p pattern
				(buffer-name buf)))
      (when (and (integerp idx)
		 (or (null predicate)
		     (funcall predicate buf)))
	(with-demoted-errors "Error: %S"
	  (setq bn (buffer-name buf))
	  (kill-buffer (buffer-name buf))
	  (message "Killed %s" bn))))))

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

(defun kill-customization-buffers ()
  "Close all customization buffers.
Internally, calls `kill-buffers' with \"^\*Customize.*\" as the pattern."
  (interactive)
  (kill-buffers "\\*Customize.*"))


;; --- Scratch buffers

(defmacro define-scratch-buffer-function
    (name buffer-name arg-list docstring int-spec &rest body)
  "Define a function NAME for creating a scratch buffer.
The scratch buffer is named \"BUFFER-NAME\".  NAME has
ARG-LIST and DOCSTRING as its argument list and
documentation string, respectively.  Likewise, INT-SPEC is
used as the argument to `interactive' if non-nil.  The rest
of the arguments is the BODY of function NAME.

BODY is evaluated with the scratch buffer as the current one.
The last form of BODY is returned by the defined function."
  ;; TODO: update docstring: `noninteractive'
  (declare (indent 3) (doc-string 4)
	   (debug (&define name stringp (&rest arg)
			   stringp interactive def-body)))
  `(progn
     (defun ,name ,arg-list
       ,docstring
       ,(cond ((eq int-spec 'noninteractive) ; see TODO above
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


;; --- General scratch buffer

(defun general-scratch--compute-mode-cache ()
  (cl-symbol-macrolet ((var user-ext-text-scratch--mode-cache))
    (let* ((current-count (length obarray))
	   (spew
	    (make-progress-reporter "Computing mode cache..."
				    0
				    current-count)))
      (when (/= current-count (car var))
	(setf (car var) current-count)
	(cl-loop with str = ""
		 with i = 0
		 with strings
		 for sym being the symbols
		 do
		 (setq str (format "%s" sym))
		 (when (string-match-p "-mode\\'" str)
		   (push str strings))
		 (progress-reporter-update spew (cl-incf i))
		 finally do
		 (setf (cdr var)
		       (nreverse strings))
		 (progress-reporter-done spew))))))
(define-obsolete-function-alias 'text-scratch--compute-mode-cache
  'general-scratch--compute-mode-cache "2026-01-07")

(defun general-scratch--complete-mode ()
  (general-scratch--compute-mode-cache)
  (->> (completing-read "Mode: " (cdr user-ext-text-scratch--mode-cache)
			nil t nil 'user-ext-text-scratch--mode-history)
       (intern-soft)))
(define-obsolete-function-alias 'text-scratch--complete-mode ()
  'general-scratch--complete-mode "2026-01-07")

(defun general-scratch--quit ()
  "Close the scratch buffer."
  (interactive)
  (unless (string-empty-p (buffer-string))
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

(define-scratch-buffer-function general-scratch "general scratch" (mode)
  "Open a general-purpose scratch buffer.
The buffer will have its major mode set to `text-mode' by
default, unless ARG is non-nil, in which case the major mode
is set to MODE.

Interactively, ARG is the prefix argument. The user is
prompted for MODE."
  (list  (if current-prefix-arg
	     (general-scratch--complete-mode)
	   'text-mode))
  (cl-assert (symbolp mode) t (prin1-to-string mode))
  (funcall mode)
  (general-scratch-mode 1))


(extension-provide 'buffers-ext)
;;; buffers-ext ends here

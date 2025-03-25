;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-lib)
  (require 'cl-ext))

(defgroup local-lambda nil
  "Group for the local lambda feature."
  :group 'user-extensions)

(defconst local-lambda-package-version "1.0.0")

(defvar-local local-lambda-lambdas nil
  "Current buffer's local functions.")

;;;###autoload
(defun local-lambda-get (name)
  "Return the lambda with the name NAME, or nil if it doesn't exist.
NAME must be a symbol."
  (cl-check-type name string)
  (gethash name local-lambda-lambdas))

;;;###autoload
(defun local-lambda-add-local-lambda (key function &optional overwrite)
  "Add a local function FUNCTION under key KEY.
FUNCTION is saved to `local-lambda-lambdas' under
KEY, which is a symbol.  FUNCTION is a `lambda' expression."
  (cl-check-type key symbol)
  (cl-check-type function (and (not symbol) function))
  (cl-ext-unless local-lambda-lambdas
    (setq local-lambda-lambdas (make-hash-table :test #'equal)))
  ;; Either KEY is not defined or overwrite is allowed
  (cl-ext-when (or overwrite
		   (not (gethash key local-lambda-lambdas)))
    (puthash (symbol-name key) function local-lambda-lambdas))
  t)
(define-obsolete-function-alias
  'local-lambda-ext-add-local-lambda
  #'local-lambda-add-local-lambda
  local-lambda-package-version)

(defun local-lambda--completion ()
  (let ((hash local-lambda-lambdas)
	key)
    (cl-ext-unless hash
      (user-error "No local lambdas in buffer"))
    (setq key (completing-read "Lambda: " hash nil t))
    (cl-assert key t)
    (list key)))
(define-obsolete-function-alias
  'local-lambda-ext--completion
  #'local-lambda--completion
  local-lambda-package-version)

;;;###autoload
(defun local-lambda-run-local-lambda (key)
  "Run the local function under key KEY.
KEY must be have previously been added via one of the other
functions..

Interactively, KEY is prompted from the user with completion."
  (interactive (local-lambda--completion))
  (cl-check-type key string)
  (let ((fun (local-lambda-get key)))
    (cl-assert fun t)
    (if (commandp fun)
	(call-interactively fun)
      (funcall fun))))
(define-obsolete-function-alias
  'local-lambda-ext-run-local-lambda
  #'local-lambda-run-local-lambda
  local-lambda-package-version)
(put 'local-lambda-run-local-lambda 'interactive-only t)

;;;###autoload
(defmacro local-lambda-define-local-defun (name arglist &rest body)
  "Define NAME as a buffer-local function with ARGLIST.
ARGLIST is an argument list for a `defun', and BODY is a list
of forms to add to it.

As a result of this macro, NAME can be run with
`local-lambda-run-local-lambda'.

\(fn NAME ARGS [DOCSTRING] [INTERACTIVE] BODY)"
  (declare (doc-string 3)
	   (indent 2)
	   (debug (&define name lambda-list lambda-doc
			   ["&optional" ("interactive" interactive)]
			   def-body)))
  `(progn
     (local-lambda-add-local-lambda ',name (lambda ,arglist ,@body) t)
     t))
(define-obsolete-function-alias
  'local-lambda-ext-define-local-defun
  #'local-lambda-define-local-defun
  local-lambda-package-version)

;;;###autoload
(defmacro local-lambda-define-skeleton (name docstring &rest skeleton)
  "Define NAME as a buffer-local skeleton command.
SKELETON works the same way as the SKELETON argument in
`define-skeleton', which see."
  (declare (debug (&define name [&optional stringp] skeleton-edebug-spec))
	   (indent 1)
	   (doc-string 2))
  (let* ((name (make-symbol (format "%s-skeleton" name)))
	 (spiel "This is a buffer-local skeleton command (see `skeleton-insert').
Normally the skeleton text is inserted at point, with
nothing \"inside\".  If there is a highlighted region, the
skeleton text is wrapped around the region text.

A prefix argument ARG says to wrap the skeleton around the next ARG words.
A prefix argument of -1 says to wrap around region, even if not highlighted.
A prefix argument of zero says to wrap around zero words---that is, nothing.
This is a way of overriding the use of a highlighted region.")
	 (docstring (format "%s\n\n%s" docstring spiel)))
    `(progn
       (put ',name 'no-self-insert t)
       (local-lambda-define-local-defun ,name (&optional str arg)
	 ,docstring
	 (interactive "*P\nP")
	 (skeleton-proxy-new ',skeleton str arg)))))
(define-obsolete-function-alias
  'local-lambda-ext-define-skeleton
  #'local-lambda-define-skeleton
  local-lambda-package-version)

;;;###autoload
(defmacro local-lambda-define-self-insert-command (name string &optional no-complete)
  "Define a buffer-local command NAME to insert STRING into buffer.
By default, `company-complete' is called as a command after
inserting STRING, unless NO-COMPLETE is non-nil, in which
case it is not."
  (declare (debug (&define name stringp &optional form))
	   (indent 1))
  (cl-check-type name symbol)
  (cl-check-type string string)
  `(progn
     (local-lambda-define-local-defun ,name (&optional arg)
       ,(format "Insert \"%s\" into the buffer and trigger company completion.

This is a buffer-local self-insert command, that is, a
command which automates typing fixed strings into the buffer." string)
       (interactive "P")
       (let ((string ,(substring string 0 -1))
	     (final-c ,(aref string (1- (length string)))))
	 (insert string)
	 (setq last-command-event final-c)
	 (self-insert-command 1 final-c)
	 ,(cl-ext-unless no-complete
	    '(call-interactively #'company-complete))))
     ',name))
(define-obsolete-function-alias
  'local-lambda-ext-define-self-insert-command
  #'local-lambda-define-self-insert-command
  local-lambda-package-version)

;; Minor mode

(defvar local-lambda-mode-map
  (let* ((map (make-sparse-keymap))
	 (prefix "C-c M-l")
	 (prefix-map (define-prefix-command 'local-lambda-prefix-map)))
    (define-key map (kbd prefix) #'local-lambda-prefix-map)
    (define-key prefix-map (kbd "r") #'local-lambda-run-local-lambda)
    map)
  "Keymap for `local-lambda-mode'.")

;;;###autoload
(define-minor-mode local-lambda-mode
  "A minor mode for running buffer-local functions.

\\{local-lambda-mode-map}"
  :group 'local-lambda
  :lighter " Lolam"
  :keymap local-lambda-mode-map)

(provide 'local-lambda)
;;; local-lambda.el ends here

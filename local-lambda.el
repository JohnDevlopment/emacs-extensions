;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'use-package)
  (require 'cl-lib)
  (require 'cl-ext))

(defgroup local-lambda nil
  "Group for the local lambda feature."
  :group 'user-extensions)

(defvar-local user-ext-local-lambda-lambdas nil
  "Current buffer's local functions.")

;;;###autoload
(defun local-lambda-ext-add-local-lambda (key function &optional overwrite)
  "Add a local function FUNCTION under key KEY.
FUNCTION is saved to `user-ext-local-lambda-lambdas' under
KEY, which is a symbol.  FUNCTION is a `lambda' expression."
  (cl-check-type key symbol)
  (cl-check-type function (and (not symbol) function))
  (cl-ext-unless user-ext-local-lambda-lambdas
    (setq user-ext-local-lambda-lambdas (make-hash-table :test 'eq)))
  ;; Either KEY is not defined or overwrite is allowed
  (cl-ext-when (or (not (gethash key user-ext-local-lambda-lambdas))
		   overwrite)
    (puthash key function user-ext-local-lambda-lambdas)))

(defun local-lambda-ext--completion ()
  (let ((hash user-ext-local-lambda-lambdas)
	key)
    (cl-ext-unless hash
      (user-error "No local lambdas in buffer"))
    (setq key (intern-soft (completing-read "Lambda: " hash nil t)))
    (cl-assert key)
    (list key)))

;;;###autoload
(defun local-lambda-ext-run-local-lambda (key)
  "Run the local function under key KEY.
KEY must be have previously been added via
`local-lambda-ext-add-local-lambda' or
`local-lambda-ext-define-local-defun'.

Interactively, KEY is prompted from the user with completion."
  (interactive (local-lambda-ext--completion))
  (cl-check-type key symbol)
  (let ((fun (gethash key user-ext-local-lambda-lambdas)))
    (cl-assert fun)
    (--print-expr var key)
    (if (commandp fun)
	(call-interactively fun)
      (funcall fun))))

;;;###autoload
(defmacro local-lambda-ext-define-local-defun (name arglist &rest body)
  "Define NAME as a buffer-local function.
Add NAME to the hash table at `user-ext-local-lambda-lambdas'
under the key NAME.  ARGLIST is a list of arguments for a
`defun'.

This expands to a call to `local-lambda-ext-add-local-lambda'.

As a result of this macro, NAME can be run as a function with
`local-lambda-ext-run-local-lambda'.

\(fn NAME ARGS [DOCSTRING] [INTERACTIVE] BODY)"
  (declare (doc-string 3)
	   (indent 2)
	   (debug (&define name lambda-list lambda-doc
			   ["&optional" ("interactive" interactive)]
			   def-body)))
  `(local-lambda-ext-add-local-lambda ',name (lambda ,arglist ,@body) t))

;;;###autoload
(defmacro local-lambda-ext-define-skeleton (name &rest skeleton)
  (declare (debug (&define name skeleton-edebug-spec)))
  (let ((name (make-symbol (format "%s-skeleton" name))))
    `(progn
       (put ',name 'no-self-insert t)
       (local-lambda-ext-define-local-defun ,name (&optional str arg)
	 "This is a buffer-local skeleton command (see `skeleton-insert').
That is to say, this command is only available in this
buffer.

This is a skeleton command (see `skeleton-insert').
Normally the skeleton text is inserted at point, with
nothing \"inside\".  If there is a highlighted region, the
skeleton text is wrapped around the region text.

A prefix argument ARG says to wrap the skeleton around the
next ARG words.
A prefix argument of -1 says to wrap around region, even if
not highlighted.
A prefix argument of zero says to wrap around zero
words---that is, nothing.  This is a way of overriding the
use of a highlighted region."
	 (interactive "*P\nP")
	 (skeleton-proxy-new ',skeleton str arg)))))

;; Minor mode

(defvar local-lambda-mode-map
  (let* ((map (make-sparse-keymap))
	 (prefix "C-c M-l")
	 (prefix-map (define-prefix-command 'local-lambda-prefix-map)))
    (define-key map (kbd prefix) #'local-lambda-prefix-map)
    (define-key prefix-map (kbd "r") #'local-lambda-ext-run-local-lambda)
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

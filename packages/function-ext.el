;; -*- lexical-binding: t; -*-

(defconst user-ext-fext-valid-advice-classes
  '(before
    before-while
    before-until
    around
    after
    after-while
    after-until
    activation
    deactivation
    override
    filter-args
    filter-return)
  "List of valid CLASS symbols for `fext-defadvice'.")

(eval-when-compile
  (require 'cl-lib)
  (require 'cl-ext))

;;;###autoload
(defmacro fext-defalias (symbol definition &optional docstring)
  "Define SYMBOL's definition to DEFINITION."
  (declare (debug (symbolp function-form &optional stringp))
	   (doc-string 3))
  `(defalias ',symbol ,(pcase definition
			 (`(function ,sym)
			  (list 'function sym))
			 ((and (pred listp) (guard (eql (car definition) 'lambda)))
			  definition)
			 ((pred symbolp)
			  (list 'function definition))
			 (x (error "Invalid form %S: must be symbol or lambda" x)))
     ,docstring))

;;;###autoload
(defmacro fext-replace-function (symbol file arglist &optional docstring &rest body)
  "Replace the function definition of SYMBOL.
The new definition is
(lambda SYMBOL ARGLIST [DOCSTRING] BODY...).  The arguments
are the same as `defun', which see.

This function has two side effects: on the first invocation
of this macro for SYMBOL, its original definition is saved
to SYMBOL--old.  Second, SYMBOL is declared as a function
via `declare-function'.  This is just to placate the byte-
compiler.

\(fn SYMBOL FILE ARGLIST [DOCSTRING] [DECL] BODY...)"
  (declare (indent 3) (doc-string 4)
	   (debug (&define name stringp (&rest arg)
			   [&optional stringp] def-body)))
  (cl-check-type file string-or-null)
  (cl-check-type symbol symbol)
  (cl-check-type arglist (or list null))
  (let* ((backup-symbol (intern (concat (prin1-to-string symbol) "--old")))
	 (code (symbol-function symbol)))
    (declare-function ,backup-symbol ,file ,arglist)
    `(progn
       (unless (fboundp ',backup-symbol)
	 (defalias ',backup-symbol ,(if (listp code)
					`',code
				      code)))
       (defun ,symbol ,arglist ,docstring ,@body))))

;;;###autoload
(defmacro fext-defadvice (function args &rest body)
  "Define a piece of advice for FUNCTION (a symbol).
The syntax of `fext-defadvice' is as follows:

  (fext-defadvice FUNCTION (CLASS NAME [ARGLIST])
    [KEYWORD-ARGS]
    [DOCSTRING] [INTERACTIVE-FORM]
    BODY...)

FUNCTION ::= Name of the function being advised.
CLASS ::= `before' | `before-while' `before-until' |
    `around' | `after' | `after-while' |  `after-until' |
    `activation' | `deactivation' | `override' |
    `filter-args' | `filter-return'
ARGLIST ::= Argument list for the advice.  Without it,
    FUNCTION's argument list will consist of
    \`(&optional _args)'
DOCSTRING ::= Optional documentation string for defined
    function.
INTERACTIVE-FORM ::= Optional interactive form.
BODY ::= Any s-expression.

For CLASS, see the WHERE argument of `add-function'.

Keyword arguments can be provided at the beginning of BODY.
The following keywords are supported:

:remove FORM    If FORM evaluates to non-nil, the advice is
                removed from FUNCTION.  In this case, the
                rest of BODY is not evaluated.  CLASS and
                NAME are used to specify what advice is
                removed.

\(fn FUNCTION (CLASS NAME [ARGLIST]) BODY...)"
  (declare (indent 2) (debug (&define name
				      ([&or "before" "before-while"
					    "before-until" "around" "after"
					    "after-while" "after-until"
					    "activation" "deactivation"
					    "override"]
				       name
				       [&optional listp])
				      [&optional ":remove" form]
				      [&optional stringp]
				      [&optional ("interactive" interactive)]
				      def-body)))
  (let ((arglist '(&rest _args))
	(remove (cl-ext-progn
		  (cl-ext-when (eq (car-safe body) :remove)
		    (pop body)
		    (pop body))))
	aname fname
	class)
    (cl-destructuring-bind (c n &optional al) args
      (cl-check-type c symbol)
      (cl-check-type n symbol)
      (setq class c fname n)
      (when al
	(cl-check-type al list)
	(setq arglist al)))
    (unless (memq class user-ext-fext-valid-advice-classes) ; check valid CLASS
      (signal-wrong-argument class user-ext-fext-valid-advice-classes))
    (setq fname (intern (format "advice-ext--%S-%S" class fname)) ; advice function name
	  aname (intern (format "advice-%S-%S" class function))	  ; advice name
	  class (intern-soft (format ":%S" class)))		  ; CLASS (e.g., ":after")
    (if remove
	(cl-ext-progn
	  `(advice-remove (function ,function) (quote ,aname)))
      (cl-assert (not (null class)))
      `(progn
	 (defun ,fname ,arglist ,@body)
	 (unless (advice-member-p (quote ,aname) (quote ,function))
	   (advice-add (quote ,function) ,class (quote ,fname)
		       (alist-ext-define 'name (quote ,aname))))))))

(provide 'function-ext)

;;; function-ext.el ends here

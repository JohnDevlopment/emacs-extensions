;; -*- lexical-binding: t; -*-

(defconst user-ext-fext-valid-advice-classes
  '(before around after activation deactivation))

(eval-when-compile
  (require 'cl-lib))

(defmacro fext-replace-function (symbol file arglist &optional docstring &rest body)
  "Replace the function definition of SYMBOL.
The new definition is
(lambda SYMBOL ARGLIST [DOCSTRING] BODY...).  The arguments
are the same as `defun', which see.

This function has two side effects: on the first invocation
of this macro for SYMBOL, its original definition is saved
to SYMBOL--old.  Second, SYMBOL is declared as a function
via `declare-function'.

\(fn SYMBOL FILE ARGLIST [DOCSTRING] [DECL] BODY...)"
  (declare (indent 3) (doc-string 4))
  (cl-check-type file string-or-null)
  (cl-check-type symbol symbol)
  (cl-check-type arglist (or list null))
  (let* ((backup-symbol (intern (concat (prin1-to-string symbol) "--old")))
	 (code (symbol-function backup-symbol)))
    (declare-function ,backup-symbol ,file ,arglist)
    `(progn
       (unless (fboundp ',backup-symbol)
	 (defalias ',backup-symbol ,(if (listp code)
					`',code
				      code)))
       (defun ,symbol ,arglist ,docstring ,@body))))

(defmacro fext-defadvice (function args &rest body)
  "Define a piece of advice for FUNCTION (a symbol).
The syntax of `fext-defadvice' is as follows:

  (fext-defadvice FUNCTION (CLASS NAME [ARGLIST])
    [DOCSTRING] [INTERACTIVE-FORM]
    BODY...)

FUNCTION ::= Name of the function being advised.
CLASS ::= `before' | `around' | `after' | `activation' |
    `deactivation'
ARGLIST ::= Argument list for the advice.  Without it,
    FUNCTION's argument list will consist of
    \\`(&optional _args)'
DOCSTRING ::= Optional documentation string for defined
    function.
INTERACTIVE-FORM ::= Optional interactive form.
BODY ::= Any s-expression.

\(fn FUNCTION (CLASS NAME [ARGLIST]) BODY...)"
  (declare (indent 2) (debug (&define name
				      ([&or "before" "around"
					    "after" "activation" "deactivation"]
				       name
				       [&optional listp])
				      [&optional stringp]
				      [&optional ("interactive" interactive)]
				      def-body)))
  (let ((arglist '(&rest _args))
	aname fname
	class)
    (cl-destructuring-bind (c n &optional al) args
      (cl-check-type c symbol)
      (cl-check-type n symbol)
      (setq class c fname n)
      (when al
	(cl-check-type al list)
	(setq arglist al)))
    (unless (memq class user-ext-fext-valid-advice-classes)
      (signal-wrong-argument class user-ext-fext-valid-advice-classes))
    (setq fname (intern (format "advice-ext--%S-%S" class fname))
	  aname (intern (format "advice-%S-%S" class function))
	  class (intern-soft (format ":%S" class)))
    (cl-assert (not (null class)))
    `(progn
       (defun ,fname ,arglist ,@body)
       (unless (advice-member-p (quote ,aname) (quote ,function))
	 (advice-add (quote ,function) ,class (quote ,fname)
		     (alist-ext-define 'name (quote ,aname)))))))

(provide 'function-ext)

;;; function-ext.el ends here

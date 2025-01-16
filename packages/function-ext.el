;;; function-ext.el --- Function extension.          -*- lexical-binding: t; -*-

;;; Code:

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
    `(progn
       (declare-function ,backup-symbol ,file ,arglist)
       (unless (fboundp ',backup-symbol)
	 (defalias ,backup-symbol ,(if (listp code)
				       `',code
				     code)))
       (defun ,symbol ,arglist ,docstring ,@body))))

(provide 'function-ext)

;;; function-ext.el ends here

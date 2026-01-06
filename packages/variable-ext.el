;; -*- lexical-binding: t; -*-

(defun defconst2--watch (symbol _newvalue operation _where)
  (cl-ext-when (and (get symbol 'constant-flag)
		    (eq operation 'set))
      (error "Cannot change the value of constant `%S'" symbol)))

(defmacro defconst2 (symbol initvalue &optional docstring)
  "Define SYMBOL as a constant variable with INITVALUE.
In addition to calling `defconst' (which see), this also
sets a variable watcher and actively prevents SYMBOL from
being set by means of `set'.  DOCSTRING, if provided, is
used as the variable's documentation.

The variable's documentation will include a postamble
indicating that the variable cannot be changed via `set'.
However, calling this macro again will redefine the value."
  (declare (indent 2) (doc-string 3)
	   (debug (&define name def-form [&optional stringp])))
  (let* ((docstring-postamble "This variable is a constant; it cannot be changed via `set'
and `setq'.")
	 (docstring-body (or docstring "Undocumented."))
	 (docstring (format "%s\n\n%s" docstring-body docstring-postamble)))
    `(prog1 ',symbol
       (makunbound ',symbol)
       (setplist ',symbol nil)
       (defconst ,symbol ,initvalue ,docstring)
       (put ',symbol 'constant-flag t)
       (add-variable-watcher ',symbol #'defconst2--watch))))

(provide 'variable-ext)
;;; vext.el ends here

;;; function-ext.el --- Extensions for modifying function symbols.  -*- lexical-binding: t; -*-

;;; Code:

(defmacro function-ext-replace-function (symbol backup arglist &optional docstring &rest body)
  "Replace the function definition of SYMBOL with DOCSTRING and BODY.
The definition of SYMBOL is saved to BACKUP, which is a
symbol.  DOCSTRING, ARGLIST and BODY are passed directly to
`lamdba' so that it can handle those arguments accordingly.

Internally, this uses `defalias' to set function definitions
of both symbols."
  (declare (doc-string 4) (indent 3))
  (let ((old-fdef (symbol-function symbol)))
    `(progn
       ,(unless (fboundp backup)
	  `(defalias ',backup ,(if (listp old-fdef)
				   `',old-fdef
				 old-fdef)))
       (defalias ',symbol (lambda ,arglist ,docstring ,@body)))))

(when nil
  (cl-prettyexpand '(function-ext-replace-function py-hide-base old-py-hide-base (form &optional beg end)
		      "Hide visibility of existing form at point.
This function was replaced with a custom function
definition.  The old function definition is in symbol
`old-py-hide-base'."
		      (hs-minor-mode 1)
		      (save-excursion
			(let* ((form (prin1-to-string form))
			       (beg (or beg (or (funcall (intern-soft (concat "py--beginning-of-" form "-p")))
						(funcall (intern-soft (concat "py-backward-" form))))))
			       (end (or end (funcall (intern-soft (concat "py-forward-" form)))))
			       (modified (buffer-modified-p))
			       (inhibit-read-only t))
			  (if (and beg end)
			      (progn
				(hs-make-overlay beg end 'code)
				(set-buffer-modified-p modified))
			    (error (concat "No " (format "%s" form) " at point"))))))))

(provide 'function-ext)

;;; function-ext.el ends here

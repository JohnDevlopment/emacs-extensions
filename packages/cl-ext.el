;;; cl-ext.el --- Common lisp extensions extension.  -*- lexical-binding: t; -*-

(eval-when-compile
  (ignore t))

;; Functions

;;;###autoload
(defmacro cl-ext-when (cond first-form &rest body)
  "If COND yields non-nil, do FIRST-FORM and BODY, else return nil.
When COND yields non-nil, eval FIRST-FORM and BODY forms
sequentially and return value of last one.

There is one main difference between `cl-ext-when' and
`when': if BODY is empty, this expands to a `and' form, else
this behaves exactly the same as `when'.

\(fn COND FIRST-FORM BODY...)"
  (declare (indent 1) (debug t))
  (if body
      `(when ,cond
	 ,first-form
	 ,@body)
    `(and ,cond ,first-form)))

(defmacro cl-ext-append (x place)
  "Add X to the list stored in PLACE.
PLACE is a symbol whose definition is a list or some other
kind of sequence."
  (declare (pure t) (debug (form symbolp)))
  (cl-check-type place symbol)
  `(setq ,place (append ,place ,x)))
(make-obsolete 'cl-append 'cl-ext-append "2025.02.10")

(defalias 'cl-pushend 'cl-append)
(make-obsolete 'cl-pushend 'cl-append "2024.12.21")

(defmacro cl-ext-append-list (x place)
  "Add X to the list stored in PLACE, but wrap X in a list.
X is supposed to be a list, likely representing a Lisp
expression; it's added to PLACE in such a way that it isn't
destructured (see `append').

Let's use an example.  Supposed you're building a list of
Lisp expressions and adding another list to it:
   (let ((body '(\"Docstring.\")))
     (cl-append-list (interactive) body))
   => (\"Docstring\" (interactive))"
  (declare (pure t) (debug (form symbolp)))
  (cl-check-type x listp)
  (cl-check-type place symbol)
  `(setq ,place (append ,place (list ,x))))
(make-obsolete 'cl-append-list #'cl-ext-append-list "2025.02.10")

(defmacro cl-ext-nconcat (place &rest sequences)
  "Append the arguments (SEQUENCES) as strings to PLACE.
the string found at PLACE and SEQUENCES are combined via
`concat' and then set as the new value of PLACE."
  `(setq ,place (concat ,place ,@sequences)))

(defmacro cl-ext-save-point (&rest body)
  "Execute BODY and restore point to its original position.
Any errors are caught and printed as simple messages.

\(fn BODY...)"
  (declare (indent 0))
  `(let ((user-ext-cl--point (point-marker))
	 (user-ext-cl--result
	  (with-demoted-errors "Error caught from `cl-save-point': %S"
	    ,@body)))
     (goto-char user-ext-cl--point)
     user-ext-cl--result))
(defalias 'cl-save-point #'cl-ext-save-point)
(make-obsolete 'cl-save-point 'cl-ext-save-point "2025.02.02")

(provide 'cl-ext)

;;; cl-ext.el ends here

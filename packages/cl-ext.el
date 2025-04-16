;; -*- lexical-binding: t; -*-

(defconst user-ext-cl-special-forms
  '(let let* prog1 prog2 progn save-current-buffer save-excursion
	save-mark-and-excursion save-restriction)
  "A list of special forms that are recognized by cl-ext-* functions.
This is a list of symbols which are special forms.  Any list
whose car is one of these symbols will evaluate non-nil when
passed `cl-ext--pcase-special-form'.")

(defun cl-ext--pcase-special-form (form)
  "Return non-nil if FORM is a special form.
Return non-nil if FORM is a list whose car is one of the
special forms listed in `user-ext-cl-special-forms'."
  (pcase form
    ((and (pred listp)
	  (guard (>= (length form) 2))
	  (or (guard (memq (car form) user-ext-cl-special-forms))
	      (and (guard (macrop (car form)))
		   (guard (memq (car (macroexpand-1 form))
				user-ext-cl-special-forms)))))
     form)
    (_ nil)))

;;;###autoload
(defmacro cl-ext-when (cond first-form &rest body)
  "If COND yields non-nil, do FIRST-FORM and BODY, else return nil.
When COND yields non-nil, eval FIRST-FORM and BODY forms
sequentially and return value of last one.

The main difference between `cl-ext-when' and `when' is that
when BODY is empty, this expands to a `and' form; otherwise,
it behaves exactly the same as the latter.

If BODY is nil and COND is an `and' form ala
\`(or SUB-CONDS...)', SUB-CONDS is collapsed into COND.  As
a result, the form

   (cl-ext-when (and n (> n 0))
       n)

expands to

   (and n (> n 0) n)

If BODY is nil, and FIRST-FORM returns non-nil on the
predicate `cl-ext--pcase-special-form', this expands
directly to an `if' form.

\(fn COND FIRST-FORM BODY...)"
  (declare (indent 2) (debug t))
  (if body
      `(when ,cond
	 ,first-form
	 ,@body)
    (pcase cond
      (`(and . ,x)
       `(and ,@x ,first-form))
      (_
       (if (cl-ext--pcase-special-form first-form)
	   `(if ,cond ,first-form)
	 `(and ,cond ,first-form))))))

;;;###autoload
(defmacro cl-ext-unless (cond first-form &rest body)
  "If COND yields nil, do FIRST-FORM and BODY, else return nil.
When COND yields nil, eval FIRST-FORM and BODY forms
sequentially and return value of last one.

The main difference between `cl-ext-unless' and `unless' is
that when BODY is empty, this expands to an `or' form;
otherwise, it behaves exactly the same as the latter.

If BODY is nil and COND is an `or' form ala
\`(or SUB-CONDS...)', SUB-CONDS is collapsed into COND.  As
a result, the form

   (cl-ext-unless (or n (<= n 0))
       n)

expands to

   (or n (<= n 0) n)

If BODY is nil, and FIRST-FORM returns non-nil on the
predicate `cl-ext--pcase-special-form', this expands
directly to an ...

\(fn COND FIRST-FORM BODY...)"
  (declare (indent 2) (debug t))
  (if body
      `(unless ,cond
	 ,first-form
	 ,@body)
    (pcase cond
      (`(or . ,x)
       `(or ,@x ,first-form))
      (_
       (pcase first-form
	 ((pred cl-ext--pcase-special-form)
	  `(if ,cond ,first-form))
	 (_ `(or ,cond ,first-form)))))))

;;;###autoload
(defmacro cl-ext-append (x place)
  "Add X to the list stored in PLACE."
  (declare (debug (form symbolp)))
  `(setf ,place (append ,place ,x)))
(define-obsolete-function-alias 'cl-append #'cl-ext-append "2025.02.10")
(define-obsolete-function-alias 'cl-pushend 'cl-append "2024.12.21")

;;;###autoload
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
(define-obsolete-function-alias 'cl-append-list #'cl-ext-append-list "2025.02.10")

;;;###autoload
(defmacro cl-ext-nconcat (place &rest sequences)
  "Append the arguments (SEQUENCES) as strings to PLACE.
the string found at PLACE and SEQUENCES are combined via
`concat' and then set as the new value of PLACE."
  `(setq ,place (concat ,place ,@sequences)))

;;;###autoload
(defmacro cl-ext-save-point (&rest body)
  "Execute BODY and restore point to its original position.
Any errors are caught and printed as simple messages.

\(fn BODY...)"
  (declare (indent 0) (debug (&rest form)))
  `(let ((user-ext-cl--point (point-marker))
	 (user-ext-cl--result
	  (with-demoted-errors "Error caught from `cl-save-point': %S"
	    ,@body)))
     (goto-char user-ext-cl--point)
     user-ext-cl--result))
(define-obsolete-function-alias 'cl-save-point #'cl-ext-save-point "2025.02.02")

;;;###autoload
(defmacro cl-ext-progn (&rest body)
  "Eval BODY forms sequentially and return value of last one.

This expansion changes to different things depending on how
many elements BODY has: if 0, this expands to a single call
to \`(ignore)'; if 1, to just that element; if 2 or greater,
this behaves exactly like `progn'.

\(fn BODY...)"
  (declare (indent 0) (debug progn))
  (pcase (length body)
    (0 '(ignore))
    (1 (car body))
    (_ `(progn ,@body))))

(provide 'cl-ext)

;;; cl-ext.el ends here

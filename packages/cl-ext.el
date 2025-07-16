;; -*- lexical-binding: t; -*-

;; ### Functions

(require 'ert)

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
       (if (cl-ext--pcase-special-form first-form)
	   `(if ,cond ,first-form)
	 `(and ,@x ,first-form)))
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
       (if (cl-ext--pcase-special-form first-form)
	   `(unless ,cond ,first-form)
	 `(or ,@x ,first-form)))
      (_
       (if (cl-ext--pcase-special-form first-form)
	   `(unless ,cond ,first-form)
	 `(or ,cond ,first-form))))))

;;;###autoload
(defmacro cl-ext-append (x place)
  "Add X to the list stored in PLACE."
  (declare (debug (form symbol)))
  (cl-check-type place symbol)
  `(setq ,place (append ,place '(,x))))
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
  (declare (debug (form symbolp)))
  (cl-check-type place symbol)
  `(setq ,place (append ,place (list ,x))))
(define-obsolete-function-alias 'cl-append-list #'cl-ext-append-list "2025.02.10")

;;;###autoload
(defmacro cl-ext-until (test &rest body)
  "If TEST yields nil, eval BODY...and repeat.
This is the opposite of `while'--BODY is evaluated every
iteration of the loop until TEST returns non-nil.

\(fn TEST BODY...)"
  (declare (indent 1) (debug (form &rest form)))
  `(while (not ,test)
     ,@body))
(cl-ext-unless (fboundp 'until)
    (defalias 'until #'cl-ext-until))

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
  (declare (indent 0) (debug t))
  (pcase (length body)
    (0 '(ignore))
    (1 (car body))
    (_ `(progn ,@body))))

;;;###autoload
(defmacro cl-ext-check-type (form type &optional string)
  "Verify FORM is of type TYPE; signal an error if not.
To wit, signal `type-error' if VALUE does not match TYPE
according to `cl-typep', which see.  Include STRING in the
error message if provided."
  `(cl-ext-unless (cl-typep ,form ',type)
       (signal-type-error ,form ,(if string
				     string
				   `(type-of ,form))
			  ',type)))

(defun cl-ext--cond-validate-clause (clause)
  (pcase clause
    (`(,condition . ,body)
     (--print-expr var condition)
     (--print-expr var body))
    (_ (error "Invalid clause %S; must be of form (CONDITION BODY...)"
	      clause))))

(defmacro cl-ext-cond (clause &rest clauses)
  "Try each clause until one succeeds.

This works pretty much like `cond' (which see) except that
it expands differently depending on how many clauses there
are."
  (declare (indent 1) (debug (cond-clause &rest cond-clause)))
  (cl-ext-unless clause
      (signal-wrong-argument clause "t or any valid form"))
  (if-let (clauses)
      (cl-ext-progn
	(cl-loop for clause in clauses
		 do (cl-ext--cond-validate-clause clause))
	(push clause clauses)
	`(cond ,@clauses))
    (cl-ext--cond-validate-clause clause)
    (cl-destructuring-bind (condition &rest body) clause
      `(if ,condition
	   (progn ,@body)))))
(def-edebug-spec cond-clause ([&or "t" form] &rest form))

;; ### Tests

(ert-deftest cl-ext-test-append ()
  "Tests the result of `cl-ext-append'."
  (let ((lst '(1 2)))
    (should (equal lst '(1 2)))
    (cl-ext-append 3 lst)
    (should (equal lst '(1 2 3)))))

(ert-deftest cl-ext-test-when ()
  "Tests the expansion of `cl-ext-when'."
  (should (equal (macroexpand '(cl-ext-when foo bar))
		 '(and foo bar)))
  (should (equal (macroexpand '(cl-ext-when foo bar baz))
		 '(if foo (progn bar baz))))
  (should (equal (macroexpand '(cl-ext-when foo (save-excursion bar)))
		 '(if foo (save-excursion bar)))))

(ert-deftest cl-ext-test-progn ()
  "Tests the expansion of `cl-ext-progn'."
  (should (equal (macroexpand '(cl-ext-progn))
		 '(ignore)))
  (should (equal (macroexpand '(cl-ext-progn foo))
		 'foo))
  (should (equal (macroexpand '(cl-ext-progn foo var))
		 '(progn foo var))))

(provide 'cl-ext)
;;; cl-ext.el ends here

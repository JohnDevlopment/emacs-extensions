;; -*- lexical-binding: t; -*-

;; ### Functions

(require 'ert)
(require 'dash)
(require 'cl-lib)

(defconst user-ext-cl-special-forms
  '(let let* prog1 prog2 progn save-current-buffer save-excursion
	save-mark-and-excursion save-restriction)
  "A list of special forms that are recognized by cl-ext-* functions.
This is a list of symbols which are special forms.  Any list
whose car is one of these symbols will evaluate non-nil when
passed `cl-ext--pcase-special-form'.")


;; ### Functions

(function-put #'cl-tagbody 'lisp-indent-function 'defun)
(function-put #'cl-prog 'lisp-indent-function 1)

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

(defun cl-ext--take-until-keyword (list)
  (--take-while (not (keywordp it)) list))

(cl-defun cl-ext--plist-remove (list key &optional (count 1))
  "Remove KEY and COUNT elements after it from LIST.
If COUNT is omitted or nil, it defaults to 1.

Example:
   (cl-ext--plist-remove \\='(1 :warn t 2 3 4) :warn 1)
   =>
   (1 2 3 4)


\(fn LIST KEY [COUNT])"
  (if-let ((idx1 (-elem-index key list))
	   (idx2 (+ idx1 count)))
      (cl-loop for item in list
	       and i below (length list)
	       unless (<= idx1 i idx2)
	       collect item)
    list))

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
(define-obsolete-function-alias 'cl-ext-when #'when "2025-09-17")

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

   (or (and n nil) (<= n 0) n)

If BODY is nil, and FIRST-FORM returns non-nil on the
predicate `cl-ext--pcase-special-form', this expands
directly to an `unless' form.

\(fn COND FIRST-FORM BODY...)"
  ;; TODO: Update documentation
  (declare (indent 1) (debug t))
  (if body
      `(unless ,cond
	 ,first-form
	 ,@body)
    (pcase cond
      (`(or . ,x)
       (if (cl-ext--pcase-special-form first-form)
	   `(unless ,cond ,first-form)
	 `(or (and (or ,@x) nil) ,first-form)))
      (_
       (if (cl-ext--pcase-special-form first-form)
	   `(unless ,cond ,first-form)
	 `(or ,cond ,first-form))))))
(define-obsolete-function-alias 'cl-ext-unless #'unless "2025-09-10")

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
During the evaluation of BODY, the saved point is lexically
bound to `user-ext-cl--point'.
Any errors are caught and printed as simple messages.

\(fn BODY...)"
  (declare (indent 0) (debug (&rest form)))
  (cl-ext-unless lexical-binding
      (error "Lexical binding must be enabled"))
  `(let* ((user-ext-cl--point (point-marker))
	  (user-ext-cl--result
	   (with-demoted-errors "Error caught from `cl-save-point': %S"
	     ,@body)))
     (goto-char user-ext-cl--point)
     user-ext-cl--result))
(define-obsolete-function-alias 'cl-save-point #'cl-ext-save-point "2025.02.02")

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
    (`(,_condition . ,body)
     (cl-ext-unless body
	 (error "Invalid clause %S; body is empty" clause)))
    (_ (error "Invalid clause %S; must be of form (CONDITION BODY...)"
	      clause))))

(cl-defun cl-ext--cond-two-clauses (clause1 clause2 &rest r)
  "

\(fn (COND1 BODY1...) (COND2 BODY2...) [ARG...])"
  (cl-ext-when r
      (cl-return-from cl-ext--cond-two-clauses))
  (cl-destructuring-bind (cond1 &rest body1) clause1
    (cl-destructuring-bind (cond2 &rest body2) clause2
      (if (eq cond2 t)
	  (cl-ext-progn
	    `(if ,cond1
		 (progn ,@body1)
	       ,@body2))
	`(cond ,clause1
	       ,clause2)))))

;;;###autoload
(defmacro cl-ext-cond (clause &rest clauses)
  "Try each clause until one succeeds.

This works pretty much like `cond' (which see) except that
it expands differently depending on how many clauses there
are."
  (declare (debug ((form &rest form) &rest (form &rest form)))
	   (indent defun))
  (unless (and (listp clause) clause)
    (signal-wrong-argument clause "Non-nil list"))
  (if clauses
      (cl-ext-progn
	(if-let ((exp (apply #'cl-ext--cond-two-clauses clause clauses)))
	    (cl-ext-progn exp)
	  (cl-loop for clause in clauses
		   do (cl-ext--cond-validate-clause clause))
	  (push clause clauses)
	  `(cond ,@clauses)))
    (cl-ext--cond-validate-clause clause)
    (cl-destructuring-bind (condition &rest body) clause
      `(if ,condition
	   (progn ,@body)))))

(defmacro cl-ext-repeat-form (count form)
  "Repeat FORM COUNT number of times."
  (declare (debug (integerp sexp)))
  (cl-check-type count integer)
  (let ((body (cl-loop for i below count
		       collect form)))
    `(progn ,@body)))


;; --- Keyword argument parsing

(defun cl-ext--format-args-alist (list-or-alist)
  (cl-check-type list-or-alist (or alist list))
  (if (alist-p list-or-alist)
      list-or-alist
    (let ((alist (-partition-before-pred (## keywordp %1) list-or-alist)))
      (unless (keywordp (car (nth 0 alist)))
	(push :positional-args (nth 0 alist)))
      alist)))

(defmacro cl-ext-format-args-alist (listvar)
  "Format the value of LISTVAR (a list) into an alist.
LISTVAR is a symbol, the name of a variable containing a
list.

LISTVAR's value is changed to a list where each item has the
form (KEYWORD ARG...).  If the first item does not have a
KEYWORD, then :positional-args is added to it as the key,
such that
   (a)
becomes
   (:positional-args a)

This should be used at the beginning of any function that
uses `cl-ext-get-keyword-with-arg', `cl-ext-get-keyword-no-arg',
or `cl-ext-get-keyword-with-n-args'."
  (declare (debug t))
  (cl-check-type listvar symbol)
  `(setq ,listvar (cl-ext--format-args-alist ,listvar)))

(defun cl-ext--keyword-values (list key &optional count)
  "Return a sublist of COUNT elements following keyword KEY in LIST.
If COUNT is non-nil, return that many elements following
KEY; if it is nil, return the whole sublist."
  (declare (pure t) (side-effect-free t))
  (cl-check-type count (or (integer 0 *) null))
  (let ((parts (cl-ext--format-args-alist list)))
    (when-let ((sl (assq key parts)))
      (pcase-let* ((`(,kw . ,args) sl)
		   (diff (if count (- count (length args)) 0)))
	(when (> diff 0)
	  (setq args (append args (-repeat diff :missing))))
	(pcase count
	  (0 t)
	  ('nil args)
	  (_ (-take count args)))))))

;;;###autoload
(defmacro cl-ext-get-keyword-no-arg (alistvar key)
  "Return t if KEY is found in ALISTVAR, nil otherwise.
ALISTVAR is the name of a variable containing an alist, a
symbol.
KEY is a keyword used to search the alist."
  (cl-check-type alistvar symbol)
  (cl-check-type key keyword)
  `(cl-ext--keyword-values ,alistvar ,key 0))

;;;###autoload
(defmacro cl-ext-get-keyword-with-arg (alistvar key)
  "Extract the value of KEY from ALISTVAR.
ALISTVAR is the name of a variable containing an alist, a
symbol.
The result, if non-nil, is the element that directly follows
KEY in ALISTVAR."
  (declare (debug t))
  (cl-check-type alistvar symbol)
  (cl-check-type key keyword)
  `(car (cl-ext--keyword-values ,alistvar ,key 1)))

;;;###autoload
(defmacro cl-ext-get-keyword-with-n-args (alistvar key count)
  "Extract COUNT values if keyword KEY from ALISTVAR."
  (declare (debug t))
  (cl-check-type count (or (integer 0 *) symbol))
  (and (symbolp count)
       (not (eq count '*))
       (error "Invalid symbol %S, can only be \\=`*'" count))
  (cl-check-type alistvar symbol)
  (cl-check-type key keyword)
  `(cl-ext--keyword-values ,alistvar ,key
			   ,@(and (integerp count) (list count))))


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

;; Local Variables:
;; eval: (abbrev-ext-install-local-abbrev-functions)
;; eval: (abbrev-ext-define-local-abbrev "cx" "cl-ext")
;; eval: (abbrev-ext-define-local-abbrev "ux" "user-ext-cl")
;; End:

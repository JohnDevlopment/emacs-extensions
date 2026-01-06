;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-ext))

(define-error 'enum-error "Enum error")
(define-error 'enum-missing-error "Enum missing member")

(defsubst enum-signal-member-error (enum name)
  "Signal an error for a missing member NAME of ENUM."
  (signal 'enum-missing-error (list enum name)))

(defun enum-get (enum name)
  "Get the value from member NAME of ENUM."
  (let ((val (alist-get name enum 'enum-missing-error)))
    (if (eq val 'enum-missing-error)
	(enum-signal-member-error enum name)
      val)))

(defun enum-get-safe (enum name)
  "Get the value of ENUM member NAME, or nil if not a member."
  (declare (side-effect-free t))
  (cl-ext-cond
      ((symbolp name) (alist-get name enum))
    (t nil)))
(--ignore
 (cl-prettyprint (symbol-function 'enum-get-safe))
 t)

(defun defenum--cleanup (name)
  (ignore-errors (makunbound name))
  (dolist (sym (get name 'enum-methods))
    (ignore-errors (fmakunbound sym))
    (setplist sym nil))
  (setplist name nil))

(defun defenum--members (name values)
  (let ((ncols 0)
	members name-val-pairs)
    (cl-loop with enum = name
	     with fname
	     for (name val mdoc) in values
	     do
	     (setq fname (intern (format "%S-%S" enum name))
		   ncols (max ncols (length (symbol-name name)))
		   name-val-pairs (cons (cons name val) name-val-pairs)
		   members (cons (list :name name :function fname :doc mdoc :value val)
				 members)))
    (list :members (nreverse members) :columns (1+ ncols)
	  :pairs name-val-pairs)))
(--ignore
 (cl-prettyprint (symbol-function 'defenum--members))
 (cl-prettyprint (defenum--members 'debug-level
		   '((low 1 "Low level")
		     (medium 2 "Medium level")
		     (high 3 "High level"))))
 t)

(defun defenum--doc (doc name data)
  (let ((doc (or (and doc (format "%s\n\nMembers:\n" doc))
		 (format "Enumeration %s.\n\nMembers:\n" enum-name)))))
  (mapconcat (lambda (x)
	       (format "%s. Use `%S-%S' to access it"))
	     ))

;;;###autoload
(defmacro defenum (name values &optional doc)
  "Define an enum NAME with VALUES as members.
The optional argument DOCUMENTATION is used as the enum's
documentation.

VALUES is a list where each element takes the form
(NAME VALUE [MDOC]), where NAME is a symbol denoting the
name of the member, VALUE is its value, and the optional
MDOC is its documentation.

\(fn NAME VALUES [DOCUMENTATION])"
  (declare (indent defun)
	   (debug (&define name ((symbolp sexp &optional stringp)
				 &rest (symbolp sexp &optional stringp))
			   &optional stringp)))
  (-let* (((&plist :methods methods :columns name-length :pairs name-val-pairs)
	   (defenum--members name values))
	  (fmt (format "%%-%ds %%s\n" name-length))
	  (enum-name name))
    (defenum--cleanup name)
    `(prog1 ',enum-name
       (defconst ,enum-name ',name-val-pairs
	 ,(defenum--doc name name-val-pairs))
       ,@(cl-loop with ename = name
		  with fname
		  for (name _r) in values
		  collect (cl-ext-progn
			    (setq fname (intern (format "%S-%S" ename name))
				  cmname (intern (format "%S--inliner" fname))
				  methods (cons fname methods))
			    `(progn
			       (defsubst ,fname ()
				 ,(format "Get member `%S' from enum `%S'." name ename)
				 (enum-get ,ename ',name)))))
       (put ',name 'enum t)
       (put ',name 'enum-methods ,(macroexp-quote methods)))))
(--ignore
 (cl-prettyprint (symbol-function 'defenum))
 t)

(provide 'enum-ext)
;;; enum-ext.el ends here

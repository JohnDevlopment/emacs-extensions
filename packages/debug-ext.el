;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'alist-ext)
(require 'llama)

(eval-when-compile
  (require 'cl-ext))


;; ### Variables

;;;###autoload
(defmacro --ignore (&rest body)
  "Do nothing and return nil.
This accepts any number of arguments but never evaluates,
both in runtime and during compilation. This effectively
acts as a comment.

This function emits a warning when it is byte compiled
unless the :no-warn keyword is present.

\(fn [:no-warn] BODY...)"
  (declare (debug ([&optional ":no-warn"] &rest sexp))
	   (indent defun))
  (let ((no-warn (cl-ext-get-keyword-no-arg body :no-warn)))
    (or no-warn (--show-compiler-warning --ignore))
    (list 'ignore t)))

;; (defenum debug-level
;;   ((low 1 "Low level")
;;    (medium 2 "Medium level")
;;    (high 3 "High level"))
;;   "Debug level for functions like `debug-ext-message'.")

(defconst user-ext-debug-level-name-mapping
  (alist-ext-define 1 "LOW" 2 "MEDIUM" 3 "HIGH")
  "Mapping of level numbers to level names.")

(defvar-local user-ext-debug-level 3
  "The default debug level for functions like `debug-ext-message'.
Valid values are in `user-ext-debug-level-name-mapping',
which see.")
(defun debug-ext--valid-debug-level (symbol newval op _where)
  (cl-assert (eq symbol 'user-ext-debug-level) t)
  (pcase op
    ((or 'set 'let)
     (if-let ((val (assoc newval user-ext-debug-level-name-mapping)))
	 (cl-ext-progn
	   (set symbol newval))
       (set symbol (default-value symbol))
       (error "Invalid debug level %S, can one of %s" newval
	      (eval-when-compile
		(mapconcat (##int-to-string %1)
			   (alist-ext-keys user-ext-debug-level-name-mapping)
			   ", ")))))))
(add-variable-watcher 'user-ext-debug-level #'debug-ext--valid-debug-level)

(defconst user-ext-debug-struct-function-re
  (rx bos (or "${base}-p" "copy-${base}" "make-${base}"
	      (seq "${base}-" (+? nonl)))
      (opt "--cmacro") eos))

(defconst user-ext-debug-truncate-text-column 50
  "The column from which to elide strings.")


;; ### Functions

;;;###autoload
(defmacro --show-compiler-warning (function)
  (cl-check-type function symbol)
  `(byte-compile-warn ,(format "Function %S is used here" function)))

(define-obsolete-function-alias 'assert #'cl-assert "2024-12-24")

;;;###autoload
(defmacro --print-expr (type form &optional printcharfun)
  "Print the result of FORM.
TYPE is used to indicate how FORM should be handled.
Currently, it can be either symbol `var' or symbol `sexp'.
PRINTCHARFUN is the output stream in which to print the
result (see `princ').

This macro emits a warning when it is byte compiled."
  (declare (debug ([&or "var" "sexp"] sexp &optional form)))
  (when (macroexp-compiling-p)
    (--show-compiler-warning --print-expr))
  (pcase type
    ('var
     (cl-check-type form symbol)
     `(let ((evalled-form ,form))
	(prog1 evalled-form
	  (princ (format "DEBUG: %s = %S" (quote ,form) evalled-form) ,printcharfun)
	  (princ "\n" ,printcharfun))))
    ('sexp
     (let ((sargs (cdr-safe form)))
       `(let ((evalled-form ,form))
	  (--princ-form ,(macroexp-quote form) (list ,@sargs) evalled-form ,printcharfun))))
    (_ (error "Unknown type %S" type))))

;;;###autoload
(defun --symbol-plist (symbol &rest props)
  (or (cl-evenp (length props))
      (error "PROPS requires an even number of elements"))
  (if props
      (when (eq (car-safe props) :set)
	(pop props)
	(setplist symbol props)))
  (cl-prettyprint (symbol-plist symbol)))
(cl-define-compiler-macro --symbol-plist (&whole form symbol &rest props)
  (pcase props
    (`(:set nil)
     (list 'setplist symbol nil))
    (_ form)))

;;;###autoload
(defun --destroy-function (symbol)
  "Destroy the function held by SYMBOL."
  (cl-check-type symbol symbol)
  (ignore-errors (fmakunbound symbol))
  (setplist symbol nil))

;;;###autoload
(defun --destroy-variable (symbol)
  "Destroy the variable SYMBOL.
If SYMBOL is automatically buffer-local, then remove that
setting from SYMBOL."
  (cl-check-type symbol symbol)
  (ignore-error void-variable (makunbound symbol))
  (setplist symbol nil)
  (when (local-variable-if-set-p symbol)
    (prog1 t
      (let ((sn (symbol-name symbol)))
	(unintern symbol nil)
	(intern sn)))))

;;;###autoload
(defun --destroy-struct (symbol &optional destroy)
  "Destroy the type SYMBOL and its functions.
SYMBOL must be a struct defined by `cl-defstruct'.
Unless DESTROY is non-nil, any matched symbol is not
actually destroyed, and neither is SYMBOL."
  (cl-check-type symbol symbol)
  (cl-loop with base = symbol
	   with regex = (s-format user-ext-debug-struct-function-re
				  'aget `(("base" . ,(format "%S" base))))
	   for symbol being the symbols
	   when (string-match-p regex (symbol-name symbol))
	   do
	   (--print-expr var symbol)
	   (when destroy
	     (--destroy-function symbol))
	   finally do
	   (when destroy
	     (setplist base nil))))

(define-obsolete-function-alias 'print-expr #'--print-expr "2025-03-10")

(defun --princ-form (form sargs result &optional printcharfun)
  ;; TODO: add documentation string
  (let ((string (truncate-string-to-width (with-output-to-string (princ result))
					  user-ext-debug-truncate-text-column
					  nil nil t)))
    (prog1 result
      (princ (format "DEBUG: %S [%S] = %S"
		     form `(,(car form) ,@sargs) result)
	     printcharfun)
      (princ "\n" printcharfun))))
(define-obsolete-function-alias '--princ #'--princ-form "2025-09-10")

;;;###autoload
(defun --proclaim (spec)
  "Record a global declaration.

- (debug-level LEVEL): Sets `user-ext-debug-level' to LEVEL."
  (pcase spec
    (`(debug-level ,level)
     (cl-check-type level integer)
     (setq user-ext-debug-level level))))

;;;###autoload
(defmacro --declare (&rest specs)
  "Like `--proclaim' but SPECS are unquoted and unevaluated.
SPECS are evaluated at both compile time, load time, and
eval time."
  (let ((body (mapcar (lambda (x) `(--proclaim ',x)) specs)))
    (if (macroexp-compiling-p)
	`(cl-eval-when (compile load eval) ,@body)
      `(progn ,@body))))

;;;###autoload
(defmacro --message (fmt &rest args)
  "Display a message when the buffer's level matches LEVEL.
This uses `message' to display a message.

The first argument is a format control string, and the rest
are either keyword arguments or data to be formatted
according to the string.  See `format-message' for details
on how this works.

The optional keyword :level specifies the level for which
the message is visible.  LEVEL must be an integer: if
`user-ext-debug-level' is greater than or equal to LEVEL,
then the message is displayed.  LEVEL defaults to 3.

\(fn FORMAT-STRING ARGS...)"
  (let ((level (cl-ext-get-keyword-with-arg args :level 3))
	level-name)
    (when (macroexp-compiling-p)
      (--show-compiler-warning --message))
    (when (>= user-ext-debug-level level)
      (setq level-name (alist-get level user-ext-debug-level-name-mapping))
      (cl-assert level-name)
      `(message ,fmt ,@args))))

(defun debug-ext-get-function-body (symbol)
  "Get the function definition of SYMBOL."
  (declare (obsolete nil "2025-03-22"))
  (indirect-function symbol))

(provide 'debug-ext)

;;; debug-ext.el ends here

;; Local Variables:
;; eval: (local-lambda-ext-define-self-insert-command doc/byte-compile-warning "This function emits a warning when it is byte compiled.")
;; eval: (abbrev-ext-install-local-abbrev-functions)
;; eval: (abbrev-ext-define-local-abbrev "dx" "debug-ext")
;; eval: (abbrev-ext-define-local-abbrev "ux" "user-ext-debug")
;; End:

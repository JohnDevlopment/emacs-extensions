;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'alist-ext)
(require 'llama)

(eval-when-compile
  (require 'cl-ext))

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

(defmacro --show-compiler-warning (function)
  (cl-check-type function symbol)
  `(byte-compile-warn ,(format "Function %S is used here" function)))

(define-obsolete-function-alias 'assert #'cl-assert "2024-12-24")

(defun --symbol-plist (symbol &rest props)
  (cl-ext-unless (cl-evenp (length props))
      (error "PROPS requires an even number of elements"))
  (if props
      (cl-ext-when (eq (car-safe props) :set)
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
  "Destroy the variable SYMBOL."
  (cl-check-type symbol symbol)
  (makunbound symbol)
  (setplist symbol nil))

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
	   (cl-ext-when destroy
	       (--destroy-function symbol))
	   finally do
	   (cl-ext-when destroy
	       (setplist base nil))))

(defmacro --print-expr (type form &optional printcharfun)
  "Print the result of FORM.
TYPE is used to indicate how FORM should be handled.
Currently, it can be either symbol `var' or symbol `sexp'.
PRINTCHARFUN is the output stream in which to print the
result (see `princ').

This macro emits a warning when it is byte compiled."
  (declare (debug ([&or "var" "sexp"] sexp &optional form)))
  (cl-ext-when (macroexp--compiling-p)
      (--show-compiler-warning --print-expr))
  (pcase type
    ('var
     (cl-check-type form symbol)
     `(cl-ext-progn
	(princ (format "DEBUG: %s = %S" (quote ,form) ,form) ,printcharfun)
	(princ "\n" ,printcharfun)))
    ('sexp
     `(cl-ext-progn
	(princ (format ,(concat "DEBUG: " (cl-prin1-to-string form) " = %S") ,form)
	       ,printcharfun)
	(princ "\n" ,printcharfun)))
    (_ (error "Unknown type %S" type))))
(define-obsolete-function-alias 'print-expr #'--print-expr "2025-03-10")

;;;###autoload
(defmacro --message (fmt &rest args)
  "Display a message when the buffer's level matches LEVEL.
This uses `message' to display a message.

The first argument is a format control string, and those
after LEVEL are data to be formatted according to the
string.  See `format-message' for details on how this works.

The optional second argument LEVEL is an integer denoting
the level at which the message should be displayed.
If `user-ext-debug-level' is greater than or equal to LEVEL,
then the message is displayed.

\(fn FORMAT-STRING [LEVEL] ARGS...)"
  (let* ((level (if (cl-member (car-safe args) '(1 2 3))
		    (pop args) user-ext-debug-level))
	 level-name)
    (cl-ext-when (macroexp--compiling-p)
	(--show-compiler-warning --message))
    (cl-ext-when (>= user-ext-debug-level level)
	(setq level-name (alist-ext-rget level user-ext-debug-level-name-mapping))
      (cl-assert level-name)
      `(message ,fmt ,@args))))

(defun debug-ext-get-function-body (symbol)
  "Get the function definition of SYMBOL."
  (declare (obsolete nil "2025-03-22"))
  (indirect-function symbol))

(defmacro --ignore (&rest _args)
  "Do nothing and return nil.
This accepts any number of arguments but never evaluates,
both in runtime and during compilation. This effectively
acts as a comment.

This function emits a warning when it is byte compiled."
  (declare (debug (&rest sexp)))
  (--show-compiler-warning --ignore)
  (list 'ignore t))

(provide 'debug-ext)

;;; debug-ext.el ends here

;; Local Variables:
;; eval: (local-lambda-ext-define-self-insert-command doc/byte-compile-warning "This function emits a warning when it is byte compiled.")
;; End:

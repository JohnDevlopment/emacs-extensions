;; -*- lexical-binding: t; -*-

(require 'cl-lib)

(eval-when-compile
  (require 'cl-ext))

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

(defmacro --message (format-string &rest args)
  "Display a message at the bottom of the screen.
This is just a macro wrapper around `message'.  The only
difference is that compiling this macro will emit a warning,
like the other macros in this library."
  (cl-ext-when (macroexp--compiling-p)
      (--show-compiler-warning --message))
  (declare (debug (stringp &rest form)))
  `(message ,format-string ,@args))

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

;;; debug-ext.el --- Debug extension                     -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-lib))

(defmacro --show-compiler-warning (function)
  (cl-check-type function symbol)
  `(byte-compile-warn ,(format "Function %S is used here" function)))

(defalias 'assert #'cl-assert)
(make-obsolete 'assert #'cl-assert "2024-12-24")

(defun --symbol-plist (symbol &rest props)
  (cl-ext-unless (= (mod (length props) 2) 0)
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

(defmacro --print-expr (type form)
  "Print the result of FORM.
TYPE is used to indicate how FORM should be handled.
Currently, it can be either symbol `var' or symbol `sexp'.

FORM should not be quoted."
  (pcase type
    ('var
     (cl-check-type form symbol)
     `(message "DEBUG: %s = %S" (quote ,form) ,form))
    ('sexp
     `(message ,(concat "DEBUG: " (cl-prin1-to-string form) " = %S") ,form))
    (_ (error "Unknown type %S" type))))
(define-obsolete-function-alias 'print-expr #'--print-expr "2025-03-10")

(defun debug-ext-get-function-body (symbol)
  "Get the function definition of SYMBOL."
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

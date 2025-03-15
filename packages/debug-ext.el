;;; debug-ext.el --- Debug extension                     -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-lib))

(defalias 'assert #'cl-assert)
(make-obsolete 'assert #'cl-assert "2024-12-24")

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

(provide 'debug-ext)

;;; debug-ext.el ends here

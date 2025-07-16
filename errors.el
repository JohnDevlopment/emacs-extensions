;; -*- lexical-binding: t; -*-

(require 'cl-lib)

(define-error 'invalid-argument "Invalid argument")
(define-error 'wrong-argument "Wrong argument" 'invalid-argument)
(define-error 'type-error "Type error")

(eval-and-compile
  (embed-doc-document-symbol
      errors
    "Error symbols and functions.

Errors:
.
├── `invalid-argument'
│   └── `wrong-argument'
└── `type-error'"
    :functions
    signal-wrong-argument
    signal-invalid-argument))

(defun signal-wrong-argument (arg valid)
  "Signal a `wrong-argument' error with ARG and VALID.
This error means ARG is not a valid argument and should be
one of VALID.  VALID is either a single value or a list.

If VALID is a list with only one element, that is no
different from just a single value.
If VALID is a list of two elements, it is interpreted as a
range and the message is formatted accordingly.
If VALID is 3 or more elements long, it formatted into a
comma-separated list (as a string).  As a special case, if
there are exactly 3 elements, and the third is the symbol
`list', it is formatted into a comma-separated list as a
string with only the first two.

See also the documentation for this extension by typing
\\[get-extension-documentation] errors."
  (let (msg)
    (setq msg (if (cl-typep valid 'list)
		  (pcase valid
		    (`(,x)
		     (format "Valid: %S" x))
		    (`(,x ,y)
		     (format "Valid range: %S-%S" x y))
		    (`(,x ,y list)
		     (format "Valid: %S, %S" x y))
		    (_
		     (setq valid (mapcar #'prin1-to-string valid))
		     (format "Valid: %s" (string-join valid ", "))))
		(format "Valid: %S" valid)))
    (signal 'wrong-argument (list arg msg))))

(defun signal-invalid-argument (ivarg msg &rest args)
  "Signal an error to indicate an invalid argument.
The error signals that IVARG is invalid according to the
rules of the containing function.  Display a message by
passing MSG and ARGS to `format'.

See also the documentation for this extension by typing
\\[get-extension-documentation] errors."
  (signal 'invalid-argument (list ivarg (format msg args))))

(defsubst signal-type-error (value type expected-type)
  "Signal a `type-error' error with VALUE, TYPE, and EXPECTED-TYPE.
The error means that VALUE is not of an expected type: it
details what type VALUE is and what type it was supposed to
be.  EXPECTED-TYPE takes any form supported by `cl-typep',
which see."
  (signal 'type-error `(,value ,type . ,expected-type)))

(provide 'errors)
;;; errors.el ends here

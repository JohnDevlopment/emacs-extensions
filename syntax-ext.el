;; -*- lexical-binding: t; -*-

(check-emacs-minimum-version "27.1")

(require 'cl-lib)
(require 'syntax)

;;;###autoload
(defun ppss-open-paren-depth (cl-x &optional n)
  "Get the N'th innermost depth of CL-X's open parentheses.
CL-X is a `ppss' type, and N is the optional depth.  If N is
omitted or nil, then get the innermost depth.

See the info node `(elisp)Parser State', and look at element
9 for more information."
  (declare (side-effect-free t) (pure t))
  (let ((depths (ppss-open-parens cl-x)))
    (car (last depths n))))

;;;###autoload
(defsubst make-ppss-easy (ppss)
  "Constructor of type `ppss' from PPSS."
  (make-ppss :depth                    (nth 0 ppss)
	     :innermost-start          (nth 1 ppss)
	     :last-complete-sexp-start (nth 2 ppss)
	     :string-terminator        (nth 3 ppss)
	     :comment-depth            (nth 4 ppss)
	     :quoted-p                 (nth 5 ppss)
	     :min-depth                (nth 6 ppss)
	     :comment-style            (nth 7 ppss)
	     :comment-or-string-start  (nth 8 ppss)
	     :open-parens              (nth 9 ppss)
	     :two-character-syntax     (nth 10 ppss)))

(function-put 'syntax-ppss-toplevel-pos
	      'byte-optimizer 'byte-compile-inline-expand)

(extension-provide 'syntax-ext)
;;; syntax-ext.el ends here

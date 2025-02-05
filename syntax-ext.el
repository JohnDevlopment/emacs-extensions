(require 'cl-lib)
(require 'syntax)

(defun make-ppss-easy (&optional ppss)
  "Constructor of type `ppss' from PPSS.
If PPSS is omitted, then it is set to the value of
`syntax-ppss'."
  (declare (pure t))
  (let ((ppss (or ppss (syntax-ppss))))
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
	       :two-character-syntax     (nth 10 ppss))))

;; -*- lexical-binding: t; -*-

(check-emacs-minimum-version "27.1")

(require 'gv)
(require 'macroexp)

(with-emacs-version >= "28.1"
  (define-short-documentation-group user-extensions/subr
    "Operations On Lists"
    (push-end
     :no-manual t
     :no-eval* (push-end c l))
    (pop-last
     :no-manual t
     :no-eval* (pop-last l))
    "Misc"
    (compile-when
	:no-manual t
	:no-eval* (compile-when (fboundp 'eglot) 1))))

(defmacro compile-when (cond &rest body)
  "Expand BODY if COND evaluates to non-nil.
Evaluate COND at compile and load time; if it evaluates to
non-nil, expand BODY at compile time."
  (declare (debug (sexp body)) (indent 1))
  (if (eval cond)
      `(progn ,@body)))

(defmacro push-end (newelt place)
  "Add NEWELT to the list stored in the generalized variable PLACE.
This is morally equivalent to (setf PLACE (append PLACE (list NEWELT))),
except that PLACE is evaluated only once."
  (declare (debug (form gv-place)))
  (if (symbolp place)
      (cl-ext-progn
	`(setq ,place (append ,place (list ,newelt))))
    (macroexp-let2 macroexp-copyable-p x newelt
      (gv-letplace (getter setter) place
	(funcall setter `(append ,getter (list ,x)))))))

(defmacro pop-last (place)
  "Return the last element of PLACE's value, and remove it from the list.
PLACE must be a generalized variable whose value is a list.
If the value is nil, `pop-last' returns nil but does not
actually change the list."
  (declare (debug (gv-place)))
  (if (symbolp place)
      `(prog1 (car (last ,place))
	 (nbutlast ,place))
    (gv-letplace (getter setter) place
      (macroexp-let2 macroexp-copyable-p x getter
	`(prog1 (car (last ,x))
	   ,(funcall setter `(butlast ,x)))))))

(extension-provide 'subr-ext)
;;; subr-ext.el ends here

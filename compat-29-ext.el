;; -*- lexical-binding: t; -*-

(eval-when-compile
  (load "compat-macs.el"))

(compat-version "29.1")

(compat-defmacro setopt (&rest pairs)
  "Set VARIABLE/VALUE pairs, and return the final VALUE.
This is like `setq', but is meant for user options instead of
plain variables.  This means that `setopt' will execute any
`custom-set' form associated with VARIABLE.

Note that `setopt' will emit a warning if the type of a VALUE
does not match the type of the corresponding VARIABLE as
declared by `defcustom'.  (VARIABLE will be assigned the value
even if it doesn't match the type.)

\(fn [VARIABLE VALUE]...)"
  (declare (debug setq))
  (cl-ext-unless (cl-evenp (length pairs))
      (error "PAIRS must have an even number of variable/value members"))
  (let ((expr nil))
    (while pairs
      (unless (symbolp (car pairs))
        (error "Attempting to set a non-symbol: %s" (car pairs)))
      (push `(setopt--set ',(car pairs) ,(cadr pairs))
            expr)
      (setq pairs (cddr pairs)))
    (macroexp-progn (nreverse expr))))

(compat-defun setopt--set (variable value)
  "Set user option VARIABLE to VALUE, running its `custom set'."
  (custom-load-symbol variable)
  ;; Check that the type is correct.
  (when-let* ((type (get variable 'custom-type)))
    (cl-ext-unless (widget-apply (widget-convert type) :match value)
	(warn "Value `%S' for variable `%s' does not match its type \"%s\""
              value variable type)))
  (put variable 'custom-check-value (list value))
  (funcall (or (get variable 'custom-set) #'set-default) variable value))

(provide 'compat-29-ext)
;;; compat-ext.el ends here

;; -*- lexical-binding: t; -*-

(check-emacs-minimum-version "27.1")

(require 'tempo)

(declare-function tempo-define-template "tempo-ext")

(defvar user-ext-tempo-define-function
  #'tempo-define-template
  "The template-defining function being used for this mode.")

;;;###autoload
(defun tempo-ext-on-region ()
  "Guess the on-region argument `tempo-insert' is given.

Is a workaround the problem that tempo's user handlers don't get
passed the on-region argument.

Ripped from `adoc-tempo-on-region'."
  (let* (
         ;; try to determine the arg with which the tempo-template-xxx was
         ;; called that eventually brought us here. If we came here not by an
         ;; interactive call to tempo-template-xxx we can't have a clue - assume
         ;; nil.
         (arg (if (string-match "^tempo-template-" (symbol-name this-command))
                  current-prefix-arg
                nil))
         ;; copy from tempo-define-template
         (on-region (if tempo-insert-region (not arg) arg)))
    ;; copy from tempo-insert-template
    (when (or (and (bound-and-true-p transient-mark-mode)
		   mark-active)
	      (and (featurep 'xemacs)
		   (and zmacs-regions (mark))))
      (setq on-region t))
    on-region))

;;;###autoload
(defun tempo-ext-tempo-handler (element)
  "Handler for user elements.
ELEMENT is a user element.

When `tempo-ext-tempo-handler' is an element of
`tempo-user-elements', the following elements become
available:

- (S <NAME> [:prefix <PREFIX>] [:suffix <SUFFIX>]): Inserts
  text previously read with the (p ...) construct.  If
  :prefix is provided, then prepends PREFIX to the text
  under NAME.  Aside from that difference, this acts like
  `s'.
- (&repeat <COUNT> <BODY...>): Evaluates BODY COUNT number
  of times.
- (&if <CONDITION> <BODY...>): Evaluates BODY if CONDITION
  evaluates to non-nil.  CONDITION can be a single condition
  or multiple conditions wrapped in `and' or `or'.
  Conditions can also be nested.  Within this construct,
  certain forms are available (see below).
- (y-or-n-p <PROMPT> <NAME>): If `tempo-interactive' is non-nil,
  the user is asked a \"y or n\" question via `y-or-n-p'. <PROMPT>
  is displayed as a question.  It should end in a space.  The
  result is then saved under NAME.  Because most NAMEs are strings,
  it is wise to use distinctive names that denote their boolean
  values.

Conditions:
- (named <NAME>): Non-nil if text was previously saved under
  NAME via constructs like (p ...).  Returns nil if NAME
  returns nil or an empty string.
- (flag <NAME>): Non-nil if the value under NAME is non-nil.
  NAME is assumed to be a boolean value, otherwise this
  would not work correctly.
- (and <CONDITION>...): Evaluates the CONDITIONs until one
  of them yields nil; unless they all return non-nil, in
  which case this value of the last condition.
- (or <CONDITION>...): Evaluates the CONDITIONs until one of
  them yields non-nil; unless they all return non-nil, in
  which case this returns nil.
- (not <CONDITION>): Evaluates to t if CONDITION evaluates
  to nil.
- (- [<N>] <CHAR>): Evaluates to non-nil if the character N
  position from point (default: 1) is CHAR. For example,
  \\=`(- 2 91) returns non-nil if the second character
  behind point is \"[\"."
  (let ((on-region (tempo-ext-on-region)))
    (cl-ext-cond
      ;; (S name [:prefix STRING] [:suffix STRING])
      ((and (consp element)
	    (eq (car element) 'S))
       (-let* (((_ name . plist) element)
	       ((&plist :prefix prefix :suffix suffix) plist)
	       (value (tempo-lookup-named name)))
	 (if (string-empty-p value)
	     "" (concat (or prefix "") value (or suffix "")))))
      ;; (y-or-n-p PROMPT NAME)
      ((and (consp element)
	    (eq (car element) 'y-or-n-p))
       (-let* (((_ prompt name) element))
	 (tempo-save-named name (y-or-n-p prompt))
	 ""))
      ;; (&repeat COUNT BODY...)
      ((and (consp element)
	    (eq (car element) '&repeat))
       (cl-destructuring-bind (count &rest body) (cdr element)
	 (cl-check-type count integer)
	 (dotimes (_ count "")
	   (mapc (##tempo-insert %1 on-region) body))
	 ""))
      ;; (if CONDITION BODY...)
      ((and (consp element)
	    (eq (car element) '&if))
       (cl-destructuring-bind (condition &rest body) (cdr element)
	 (setq condition (tempo-ext--handle-if-condition condition))
	 (cl-assert condition)
	 (when (eval condition)
	   (mapc (##tempo-insert %1 on-region) body))
	 "")))))

(defun tempo-ext--handle-if-condition (condition &optional top-level)
  (let ((ivfun
	 (lambda ()
	   (signal-invalid-argument
	    condition
	    (format "See documentation of `%S' for valid elements"
		    user-ext-tempo-define-function))))
	(cfun (lambda (cond1 &rest condn)
		(cl-loop
		 for cond in (cons cond1 condn)
		 collect (cl-ext-progn
			   (tempo-ext--handle-if-condition cond)))))
	result conditions)
    (pcase condition
      (`(and ,cond1 . ,condn)
       (setq conditions (apply cfun cond1 condn)
	     result `(and ,@conditions)))
      (`(or ,cond1 . ,condn)
       (setq conditions (apply cfun cond1 condn)
	     result `(or ,@conditions)))
      (`(not ,cond)
       (setq cond (tempo-ext--handle-if-condition cond)
	     result `(not ,cond)))
      (`(flag ,name)
       (unless (symbolp name)
	 (signal-type-error name (type-of name) 'symbolp))
       (setq result `(tempo-lookup-named ',name)))
      (`(named ,name)
       (unless (symbolp name)
	 (signal-type-error name (type-of name) 'symbolp))
       (setq result `(and (setq value (tempo-lookup-named ',name))
			  (not (string-empty-p value)))))
      (`(- . ,args)
       ;; (- [N] CHAR)
       (cl-ecase (length args)
	 (1 (let ((char (car args)))
	      (setq result `(eql (char-before) ,char))))
	 (2 (setq result `(save-excursion
			    (left-char ,(car args))
			    (eql (char-after) (cadr args)))))))
      (_ (funcall ivfun)))
    (if top-level
	`(let (value)
	   ,result)
      result)))

(extension-provide 'tempo-ext)
;;; tempo-ext.el ends here

;; Local Variables:
;; eval: (abbrev-ext-install-local-abbrev-functions)
;; eval: (abbrev-ext-define-local-abbrev "tx" "tempo-ext")
;; End:

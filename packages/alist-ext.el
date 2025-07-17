;; -*- lexical-binding: t; -*-

(require 'ert)

(eval-when-compile
  (require 'cl-lib))

;; ### Functions

;;;###autoload
(defun alist-ext-from-list (list)
  "Turn a regular list LIST into an alist.
LIST must have an even number of elements."
  (declare (pure t))
  (unless (= (mod (length list) 2) 0)
    (user-error "Must have even number of arguments."))
  (seq-partition list 2))

;;;###autoload
(defmacro alist-ext-define (&rest pairs)
  "Construct an alist with PAIRS, where each key precedes its value.

Each key can be any valid lisp object, but symbols have to
be quoted.

\(fn KEY VALUE ...)"
  (unless (= (mod (length pairs) 2) 0)
    (error "alist-ext-define requires an even number of arguments (key-value pairs)."))
  (let (res key value)
    (setq res (cl-loop while pairs
		       collect (cl-ext-progn
				 (setq key (pop pairs)
				       value (pop pairs))
				 `(cons ,key ,value))))
    `(list ,@res)))

;;;###autoload
(cl-defmacro alist-ext-dolist ((kvar vvar alistform &optional resultform) &rest body)
  "Loop over an alist.
Evaluate BODY with KVAR and VVAR bound the key and value of
each association from ALIST.  Then evaluate RESULT to get
the return value, defaulting to nil.

\(fn (KEY-VAR VAL-VAR ALIST [RESULT]) BODY...\)"
  (declare (indent 1)
	   (debug ((symbolp symbolp form &optional form) body)))
  (cl-check-type kvar symbol)
  (cl-check-type vvar symbol)
  (cl-ext-unless lexical-binding
    (error "lexical binding is required"))
  (let ((temp '--dolist-tail--))
    `(let ((,temp ,alistform))
       (while ,temp
	 (let* ((elt (car ,temp))
		(,kvar (car elt))
		(,vvar (cdr elt)))
	   ,@body
	   (setq ,temp (cdr ,temp))))
       ,resultform)))

;; ### Tests

(ert-deftest alist-ext-test-define ()
  "Tests the result of `alist-ext-define'."
  (let ((al (alist-ext-define 'a 1 'b 2)))
    (should (equal al '((a . 1) (b . 2))))))

(ert-deftest alist-ext-test-dolist ()
  "Tests the result of `alist-ext-dolist'."
  (let* ((lexical-binding t)
	 (al (alist-ext-define 'a 1 'b 2))
	 (form (macroexpand '(alist-ext-dolist (k v al) (list k v))))
	 lst)
    (pcase form
      (`(let ((--dolist-tail-- al))
	  (while --dolist-tail--
	    (let* ((elt (car --dolist-tail--))
		   (k (car elt))
		   (v (cdr elt)))
	      (list k v)
	      (setq --dolist-tail-- (cdr --dolist-tail--))))
	  nil)
       t)
      (_ (ert-fail `(ert-test-failed
		     "Invalid form"
		     :form ,form))))
    (setq lst (alist-ext-dolist (k v al (nreverse lst))
		(push k lst)
		(push v lst)))
    (should (equal lst '(a 1 b 2)))))

(provide 'alist-ext)
;;; alist-ext.el ends here

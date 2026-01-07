;; -*- lexical-binding: t; -*-

(require 'ert)

(eval-when-compile
  (require 'cl-lib))

(defconst user-ext-alist-quit-variable '--alist-ext-quit--
  "The name of a variable used in `alist-ext-dolist'.")

(defconst user-ext-alist-tail-variable '--alist-ext-tail--)

(defvar-local user-ext-alist--catch nil)

;; ### Functions

;;;###autoload
(defun alist-ext-keys (alist)
  "Return the keys of ALIST."
  (cl-loop for (k . v) in alist
	   collect k))

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
		       collect (progn
				 (setq key (pop pairs)
				       value (pop pairs))
				 `(cons ,key ,value))))
    `(list ,@res)))

;;;###autoload
(defun alist-ext-quit (&optional no-break)
  "Does nothing when called.
When this is found in the body of `alist-ext-dolist', the
loop is broken.  NO-BREAK controls how the loop is broken:
if non-nil, an internal quit variable is set, thus breaking
the loop naturally; otherwise, `throw' is called, and the
loop body is wrapped in a `catch' form to accomadate."
  (ignore no-break))

(defun alist-ext--modify-body-form (form)
  "Modify the body form FORM, doing neccessary substitutions.
This recurses through FORM, performing substitutions or
returning FORM unchanged, depending on its type.

If FORM is a list, then it is checked to see if it matches
one of the forms listed belowed, otherwise it is returned
unchanged.  If FORM is not a list, it is simply returned."
  (pcase form
    (`(alist-ext-quit)
     (setq user-ext-alist--catch t)
     `(progn
	(setq ,user-ext-alist-quit-variable t)
	(throw 'alist-ext-dolist nil)))
    (`(alist-ext-quit ,no-break)
     (setq user-ext-alist--catch (not no-break))
     `(progn
	(setq ,user-ext-alist-quit-variable t)
	,(unless no-break
	   (list 'throw 'alist-ext-dolist nil))))
    (`(quote . ,_) form)
    (`(function . ,_) form)
    (`(\` ,_) form)
    (`(,head . ,tail)
     (cons (alist-ext--modify-body-form head)
	   (mapcar #'alist-ext--modify-body-form tail)))
    (_ form)))

;;;###autoload
(cl-defmacro alist-ext-dolist ((kvar vvar alistform &optional resultform) &rest body)
  "Loop over an alist.
Evaluate BODY with KVAR and VVAR bound the key and value of
each association from ALIST.  Then evaluate RESULT to get
the return value, defaulting to nil.

BODY is checked to see if it contains any calls to
`alist-ext-quit'.  Any such calls cause the loop to be
broken.  See the documentation for `alist-ext-quit'.

\(fn (KEY-VAR VAL-VAR ALIST [RESULT]) BODY...\)"
  (declare (indent 1)
	   (debug ((symbolp symbolp form &optional form) body)))
  (cl-check-type kvar symbol)
  (cl-check-type vvar symbol)
  ;; (unless lexical-binding
  ;;     (error "lexical binding is required"))
  (setq user-ext-alist--catch nil)
  (setq body (alist-ext--modify-body-form body))
  (let ((inner-body
	 `((let* ((elt (car ,user-ext-alist-tail-variable))
		  (,kvar (car elt))
		  (,vvar (cdr elt)))
	     ,@body
	     (setq ,user-ext-alist-tail-variable
		   (cdr ,user-ext-alist-tail-variable))))))
    `(let ((,user-ext-alist-tail-variable ,alistform)
	   ,user-ext-alist-quit-variable)
       (while (and ,user-ext-alist-tail-variable
		   (not ,user-ext-alist-quit-variable))
	 ,(if user-ext-alist--catch
	      `(catch 'alist-ext-dolist
		 ,@inner-body)
	    `(progn ,@inner-body)))
       ,resultform)))

;;;###autoload
(cl-defun alist-ext-rget (key alist &optional default (testfn #'eq))
  "Return the car of the first element whose cdr matches KEY.
If KEY is not found in ALIST, return DEFAULT.  Equality with
KEY is tested with TESTFN, which defaults to `eq'."
  (let (result)
    (alist-ext-dolist (k v alist (or result default))
      (when (funcall testfn key v)
	(setq result k)
	(alist-ext-quit t)))))

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

;; Local Variables:
;; eval: (abbrev-ext-install-local-abbrev-functions)
;; eval: (abbrev-ext-define-local-abbrev "ax" "alist-ext")
;; eval: (abbrev-ext-define-local-abbrev "ux" "user-ext-alist")
;; End:

;; -*- lexical-binding: t; -*-

(check-emacs-minimum-version "27.4")
(extension-check-requires debug-ext)

(require 'kmacro)


;; ### Macro type

(cl-defstruct (macro-ext--macro (:type list))
  (function nil :type function :documentation "Function definition")
  (key nil :type string :documentation "Key macro is bound to.")
  (documentation nil :type nil :documentation "Function documentation."))

(defmacro macro-ext--macro-unwrap (macro &rest body)
  "Do BODY with the slots in macro bound to variables.
The slot :key is bound to \\=`key'.
The slot :documentation is bound to \\=`doc'.
The slot :function is bound to \\=`func'.
You should probably use \\=`doc' and \\=`func'; however,
\\=`key' may be ignored if not needed."
  (declare (indent 1))
  `(let ((key (macro-ext--macro-key ,macro))
	 (doc (macro-ext--macro-documentation ,macro))
	 (func (macro-ext--macro-function ,macro)))
     (ignore key)
     ,@body))


;; ### Variables

(defvar user-ext-macro-history nil
  "Macro history for completion.")

(defconst user-ext-macro-macros
  nil
  "An alist of saved macros.
Each element has the form (NAME . MACRO).
NAME is the name of the macro function to be defined, a
symbol.
MACRO is a list of the form (FUNCTION [KEY]).
FUNCTION is a lambda expression used as the function
definition for NAME.
Optional element KEY is a key sequence that FUNCTION will
be bound to; it must be in a format compatible with `kbd'
(which see).

Each MACRO should be created using `make-macro-ext--macro'.")


;; ### Functions

(defun macro-ext-query (arg)
  "Prompt for user input using minibuffer during macro execution.
With prefix ARG, allows you to choose what prompt string to
use.  If the input is non-empty, it is inserted at point."
  ;; FIXME: "exit-minibuffer: Not in most nested command loop"
  (interactive "P")
  (let* ((prompt (if arg
		     (read-from-minibuffer "PROMPT: ")
		   "Input: "))
	 (input (minibuffer-with-setup-hook
		    (lambda ()
		      (kbd-macro-query t))
		  (read-from-minibuffer prompt))))
    (unless (string= input "")
      (insert input))))
(put #'macro-ext-query 'disabled t)

(defsubst macro-ext-read-macro ()
  "Complete a macro name in the minibuffer."
  (unless user-ext-macro-macros
    (error "No macros saved"))
  (list (intern
	 (completing-read
	  "Macro: "
	  user-ext-macro-macros
	  nil
	  t
	  nil
	  'user-ext-macro-history))))

(defun macro-ext-dump-macro-definition (macro arg)
  "Dump the function definition of MACRO at point.
With no prefix argument or an ARG of 1, simply print the
definition of MACRO.  With \\[universal-argument] or an ARG
of 4, also print the name of the macro."
  (interactive "aMacro: \np")
  (cl-check-type macro symbol)
  (cl-check-type arg integer)
  (cl-case arg
    (1 (cl-prettyprint (symbol-function macro)))
    (4 (insert (format "'%S " macro))
       (cl-prettyprint (symbol-function macro)))
    (t (user-error "Invalid ARG %d" arg))))

(defun macro-ext-enable-macro (macro)
  "Enable macro MACRO.
MACRO is a symbol; it must be one of the keys in
`user-ext-macro-macros'."
  (interactive (macro-ext-read-macro))
  (cl-check-type macro symbol)
  (if-let ((symbol macro)
	   (macro (alist-get symbol user-ext-macro-macros)))
      (macro-ext--macro-unwrap macro
	(defalias symbol func doc)
	(if key
	    (cl-ext-progn
	      (global-set-key (kbd key) symbol)
	      (message "Bound `%S' to %S" symbol key))
	  (message "Enabled `%S'" symbol)))
    (user-error "No function for %S" macro)))

(defun macro-ext-disable-macro (macro)
  "Disable macro MACRO.
MACRO is a symbol; it must be one of the keys in
`user-ext-macro-macros'."
  (interactive (macro-ext-read-macro))
  (cl-check-type macro symbol)
  (if-let ((symbol macro)
	   (macro (alist-get symbol user-ext-macro-macros)))
      (macro-ext--macro-unwrap macro
	(ignore doc func)
	(--destroy-function symbol)
	(when key
	  (global-set-key (kbd key) nil)
	  (message "Disabled `%S'" symbol)))
    (user-error "No function for %S" macro)))

(defmacro macro-ext-enable-macro-maybe (macro)
  "Enable macro MACRO if it isn't already enabled."
  `(unless (fboundp ',macro)
     (macro-ext-enable-macro ',macro)))


(extension-provide 'macro-ext)
;;; macro-ext.el ends here

;; Local Variables:
;; eval: (abbrev-ext-install-local-abbrev-functions)
;; eval: (abbrev-ext-define-local-abbrev "macronote" "Note: This is a keyboard macro.")
;; eval: (abbrev-ext-define-local-abbrev "mx" "macro-ext")
;; eval: (abbrev-ext-define-local-abbrev "mxm" "macro-ext--macro")
;; eval: (abbrev-ext-define-local-abbrev "ux" "user-ext-macro")
;; End:

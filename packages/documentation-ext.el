;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'alist-ext)

(defmacro document-ext--check-type (form type &optional data)
  `(unless (cl-typep ,form (quote ,type))
     ,(if data
	  `(error "%S does not match type \"%S\" (Complete form: %S)"
		  ,form (quote ,type) (quote ,data))
	`(error "%S does not match type \"%S\""
		,form (quote ,type)))))

(defun document-ext--process-item (item &optional v1)
  (let (check-sym-type)
    (setq check-sym-type (lambda (sym x)
			   (document-ext--check-type sym symbol x)))
    (pcase item
      (`(,sym ,(or 'local 'constant))
       ;; Variable
       (funcall check-sym-type sym item)
       (format "- `%s' (%S)" sym (cadr item)))
      (`(,sym custom)
       ;; Customization variable
       (funcall check-sym-type sym item)
       (format "- `%s' (customization)" sym))
      (`(,sym command)
       (funcall check-sym-type sym item)
       (format "- Command `%S'" sym))
      (_
       ;; Other symbol, likely a function
       (document-ext--check-type item symbol)
       (format "- `%s'" item)))))

(cl-defmacro document-extension (extension
				 preamble
				 &key functions variables types requires advised)
  "Create documentation for EXTENION starting with PREAMBLE.
PREAMBLE is the documentation for EXTENSION.

The rest of the argument list consists of keyword
arguments.  Said keyword arguments are as follows:

:advised (FUN-SPEC...)   A list of EXTENSION's advised
                         functions.
:functions (FUN-SPEC...) A list of EXTENSION's public
                         functions.
:variables (VAR-SPEC...) A list of EXTENSION's public
                         variables.
:types (SYMBOL...)       A list of EXTENSION's public
                         types.
:requires (SYMBOL...)    A list of EXTENSION's required
                         packages.

FUN-SPEC is either a symbol that is the name of a function
or a list that takes the form
   (SYMBOL [command])

VAR-SPEC is either a symbol that is the name of a variable
or a list wherein the car is a symbol and the cdr is the
symbol `local', symbol `constant', or the symbol `custom'.

\(fn EXTENSION PREAMBLE [KEYWORD VALUE]...)"
  (declare (indent 1) (doc-string 2)
	   (debug (stringp stringp
			   [":functions" (&rest symbol)]
			   [":variables" (&rest symbol)])))
  (cl-check-type extension string)
  (cl-check-type functions list)
  (cl-check-type variables list)
  (let* ((var (intern (format "user-ext-%s-documentation" extension)))
	 (process-item #'document-ext--process-item)
	 (docstring preamble)
	 ;; (value (alist-ext-define 'functions functions
	 ;; 			  'variables variables))
	 v1)
    (when advised
      (setq v1 "These functions are advised:"
	    docstring (format "%s\n\n%s\n\n%s" docstring v1
			      (mapconcat process-item advised "\n"))))
    (when variables
      (setq docstring
	    (format "%s\n\nVariables:\n\n%s" docstring
		    (mapconcat process-item variables "\n"))))
    (when functions
      (setq docstring
	    (format "%s\n\nFunctions:\n\n%s" docstring
		    (mapconcat process-item functions "\n"))))
    (when types
      (setq docstring
	    (format "%s\n\nTypes:\n\n%s" docstring
		    (mapconcat process-item types "\n"))))
    (when requires
      (setq v1 "For this extension to work, the following packages must be\ninstalled:"
	    docstring (format "%s\n\n%s\n\n%s" docstring v1
			      (mapconcat process-item requires "\n"))))
    `(defconst ,var nil ,docstring)))

(provide 'documentation-ext)

;;; documentation-ext.el ends here

;;; documentation-ext.el --- Extension documentation.  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'alist-ext)

(defmacro document-ext--check-type (form type &optional data)
  `(unless (cl-typep ,form (quote ,type))
     ,(if data
	  `(error "%S does not match type \"%S\" (Complete form: %S)"
		  ,form (quote ,type) (quote ,data))
	`(error "%S does not match type \"%S\""
		,form (quote ,type)))))

(cl-defmacro document-extension (extension
				 preamble
				 &key functions variables types requires)
  "Create documentation for EXTENION starting with PREAMBLE.
PREAMBLE is the documentation for EXTENSION.

The rest of the argument list consists of keyword
arguments.  Said keyword arguments are as follows:

:functions (SYMBOL...) A list of EXTENSION's public
                       functions.
:variables (SYMBOL...) A list of EXTENSION's public
                       variables.
:types (SYMBOL...)     A list of EXTENSION's public
                       types.
:requires (SYMBOL...)  A list of EXTENSION's required
                       packages.

For each list, SYMBOL can be replaced with a sublist of the
form
   (SYMBOL TAG)

The following tags are supported:

local     Means SYMBOL is a buffer-local variable.
custom    Means SYMBOL is a customization variable.
constant  Means SYMBOL is a constant variable.

\(fn EXTENSION PREAMBLE [KEYWORD VALUE]...)"
  (declare (indent 1) (doc-string 2)
	   (debug (stringp stringp
			   [":functions" (&rest symbol)]
			   [":variables" (&rest symbol)])))
  (cl-check-type extension string)
  (cl-check-type functions list)
  (cl-check-type variables list)
  (let* ((var (intern (format "user-ext-%s-documentation" extension)))
	 (process-item
	  (lambda (x)
	    (pcase x
	      (`(,sym ,(or 'local 'constant))
	       (document-ext--check-type sym symbol x)
	       (format "- `%s' (%S)" sym (cadr x)))
	      (`(,sym custom)
	       (document-ext--check-type sym symbol x)
	       (format "- `%s' (customization)" sym))
	      (_
	       (document-ext--check-type x symbol)
	       (format "- `%s'" x)))))
	 (docstring preamble)
	 ;; (value (alist-ext-define 'functions functions
	 ;; 			  'variables variables))
	 v1)
    (when functions
      (setq docstring
	    (format "%s\n\nFunctions:\n\n%s" docstring
		    (mapconcat process-item functions "\n"))))
    (when variables
      (setq docstring
	    (format "%s\n\nVariables:\n\n%s" docstring
		    (mapconcat process-item variables "\n"))))
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

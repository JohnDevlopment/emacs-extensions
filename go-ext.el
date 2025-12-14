;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'comint)
(require 'dash)
(require 'go-mode)
(require 'hideshow)
(require 'llama)
(require 'lsp-mode)
(require 'tempo)
(require 'tree-sitter)
(require 'compat-28)

(use-package company-capf
  :defer t
  :commands
  company-capf)

(use-package tempo-ext
  :autoload
  tempo-ext-on-region
  tempo-ext--handle-if-condition
  tempo-ext-tempo-handler)

(eval-when-compile
  (require 'cl-ext)
  (require 'generator)
  (require 'debug-ext)
  (require 'easymenu))

(cl-declaim (optimize (safety 2)))

(eval-and-compile
  (embed-doc-document-symbol go-ext
    "Go extension."
    :commands
    go-ext-add-dependency
    go-ext-autotype-file
    go-ext-buffer-types
    go-ext-builtin-types
    go-ext-docedit
    go-ext-docedit-apply
    go-ext-docedit-save
    go-ext-godoc
    go-ext-godoc-server
    go-ext-godoc-server-stop
    go-ext-goto-function-or-method
    go-ext-goto-package
    go-ext-goto-struct
    go-ext-hide-function
    go-ext-hide-method
    go-ext-hide-struct
    go-ext-menu--goto-function
    go-ext-menu--goto-function-name
    go-ext-process-kill-buffer
    go-ext-process-no-edit
    go-ext-project-rename-buffer
    :functions
    go-ext-debug-body
    go-ext-define-auxillery-skeleton
    go-ext-define-hide-all-x-function
    go-ext-define-skeleton
    go-ext-erase-buffers
    go-ext-go-process-run-command
    go-ext-in-field-p
    go-ext-in-function-p
    go-ext-in-lambda-p
    go-ext-in-method-p
    go-ext-in-struct-p
    go-ext-in-type-p
    go-ext-kill-buffer
    go-ext-kill-buffer-on-error
    go-ext-make-node-from-x-function
    go-ext-pos-at-line-col
    go-ext-project-current
    go-ext-project-package
    go-ext-project-parent
    go-ext-read-type
    go-ext-rx
    go-ext-skeleton-no-newline
    go-ext-tempo-define-template
    go-ext-tree-sitter-body-region
    go-ext-try-project
    :customs
    user-ext-go-godoc-default-args
    user-ext-go-indent-tabs
    user-ext-go-tab-width
    :variables
    user-ext-go-godoc-args-history
    user-ext-go-hide-map
    user-ext-go-package-history
    user-ext-go-read-type-history
    user-ext-go-skeleton-map
    :constants
    go-ext-docedit-buffer
    go-ext-docedit-mode-map
    go-ext-docedit-overlay-category
    go-ext-docedit-register
    go-ext-go-process-mode-map
    user-ext-go--optimize
    user-ext-go-field-re
    user-ext-go-godoc-error-buffer
    user-ext-go-godoc-help-buffer
    user-ext-go-godoc-output-buffer
    user-ext-go-godoc-server-process-buffer
    user-ext-go-process-buffer
    user-ext-go-struct-decl-re))


;; ### Customization

(defgroup go-ext nil
  "Go language extension."
  :group 'user-extensions)

(defcustom user-ext-go-indent-tabs nil
  "Controls the local value of `indent-tabs-mode'."
  :type '(choice (const :tag "Tabs" t)
		 (const :tag "Spaces" nil))
  :group 'go-ext)

(defcustom user-ext-go-tab-width 4
  "Tab width in columns."
  :type 'integer
  :group 'go-ext)

(defcustom user-ext-go-custom-types nil
  "A list of types to be returned by `go-ext-custom-types'."
  :type '(repeat string)
  :safe #'list-of-strings-p
  :group 'go-ext)

(defcustom user-ext-go-godoc-default-args
  "-cmd"
  "Default commandline arguments for Godoc."
  :group 'go-ext
  :type 'string)


;; ### Variables

(defconst user-ext-go-process-buffer "*go*"
  "The name of the process buffer for generic Go processes.")

(defvar user-ext-go-package-history nil
  "History for `go-ext--package-complete'.")

(defvar user-ext-go-read-type-history nil)

(defconst user-ext-go--optimize 1)


;; --- Godoc

(define-obsolete-variable-alias 'user-ext-go-godoc-process-buffer
  'user-ext-go-godoc-output-buffer "2025-09-24")
(defconst user-ext-go-godoc-output-buffer "*godoc output*"
  "The name of the output buffer for godoc.")

(defconst user-ext-go-godoc-error-buffer "*godoc error*"
  "Buffer for errors produced by Godoc.")

(defconst user-ext-go-godoc-help-buffer "*godoc help*"
  "Help buffer for Go commands.")

(defvar user-ext-go-godoc-args-history nil
  "History of Godoc command-line arguments.")
(define-obsolete-variable-alias 'user-ext-go-godoc-args-history
  'user-ext-go-godoc-server-args-history "2025-09-21")

(defconst user-ext-go-godoc-server-process-buffer "*godoc server*"
  "The name of the process buffer for a godoc server.")


;; --- Regular Expressions

(defmacro go-ext-rx (&rest regexps)
  "Translate Go-specific regular expressions REGEXPS to a string.

In addition to the constructs of `rx', the following
constructs are available.

keyword     Match a Go keyword
identifier  Match a valid Go identifier"
  `(rx-let ((keyword (seq word-start
			  (or "break" "default" "func" "interface" "select"
			      "case" "defer" "go" "map" "struct"
			      "chan" "else" "goto" "package" "switch"
			      "const" "fallthrough" "if" "range" "type"
			      "continue" "for" "import" "return" "var")
			  word-end))
	    (identifier (seq word-start (+ (or word digit)) word-end)))
     (rx ,@regexps)))

(defconst user-ext-go-struct-decl-re
  (go-ext-rx bol (* space) "type" (+ space)
	     (group identifier) (+ space) "struct")
  "Regular expression for struct declarations.
Group 1 matches the name of the struct.")

(defconst user-ext-go-field-re
  (go-ext-rx bol (+ space) (group identifier)
	     (+ space) (group identifier))
  "Regular expression for struct fields.
Group 1 matches the name.
Group 2 matches the type.")


;; ### Types

(defmacro go-ext-make-node-from-x-function (type &rest body)
  "Define a function for creating a struct TYPE from a node.
The function created will be named make-go-ext-TYPE-from-node
and will contain BODY.

The function accepts one argument called NODE.  In BODY,
NODE is available.

This macro includes some code in the function definition
that make sure it's safe to use tree sitter functions; that
is, tree sitter must be enabled and the tree valid.

The function is declared to be side effect free, so its up
to the user to actually make it so.

The function is expected to return an object of type TYPE,
but it is up to the user to make it so."
  (declare (indent 1) (debug (&define name def-body)))
  (cl-check-type type symbol)
  (let ((fname (intern (format "make-%S-from-node" type))))
    `(progn
       (defun ,fname (node)
	 ,(format "Construct an object of type `%S' from NODE." type)
	 (declare (side-effect-free t))
	 (cl-check-type node tsc-node)
	 (go-ext-tree-sitter--assert-valid-state)
	 ,@body))))


;; --- Identifier

(cl-defstruct go-ext-identifier
  "Representation of an indentifier."
  (string nil :type string)
  (start nil :type integer-or-marker)
  (end nil :type integer-or-marker)
  (node nil :type tsc-node))

(go-ext-make-node-from-x-function go-ext-identifier
  (-let* ((string (tsc-node-text node))
	  ((beg . end) (tree-sitter-ext-region-from-node node)))
    (make-go-ext-identifier :string string :start beg :end end)))


;; --- Parameter list

(cl-defstruct go-ext-parameter-list
  "Representation of a \"parameter list\"."
  (parameters nil :documentation "List of parameters")
  (start nil :type integer-or-marker)
  (end nil :type integer-or-marker)
  (node nil :type tsc-node :documentation "The original syntax tree node."))

(go-ext-make-node-from-x-function go-ext-parameter-list
  (tree-sitter-ext-with-region node
      ((fn-by-type
	(alist-ext-define 'parameter_list #'make-go-ext-type-from-node))
       (parameters
	(cl-loop with count = (tsc-count-named-children node)
		 with child
		 for i below count
		 collect
		 (cl-ext-progn
		   (setq child (tsc-get-nth-named-child node i))
		   (tree-sitter-ext-assert-node-type child parameter_declaration)
		   (let ((child (tsc-get-first-named-child child)))
		     (funcall
		      (alist-get
		       (--print-expr sexp (tsc-node-type child))
		       fn-by-type)
		      child))))))
    (make-go-ext-parameter-list :parameters parameters :start beg :end end)))

(cl-defgeneric make-go-ext-parameter-list--parameter-type (node type)
  "XXX")


;; --- Type

(cl-defstruct go-ext-type
  "Representation of a type."
  (name nil :type go-ext-identifier)
  (start nil :type integer-or-marker)
  (end nil :type integer-or-marker)
  (node nil :type tsc-node)
  (subtype nil :type symbol
	   :documentation "Subtype, indicating if type is a pointer, slice, etc.

Pointers are represented by the symbol `pointer_type'.
Slices are represented by the symbol `slice_type'.
Variadic parameters in functions are represented by the
 symbol `variadic_parameter_declaration'."))

(go-ext-make-node-from-x-function go-ext-type
  (tree-sitter-ext-assert-node-type
   node type_declaration pointer_type slice_type type_identifier)
  (cl-block result
    (tree-sitter-ext-with-region node nil
      (pcase (tsc-node-type node)
	('type_identifier
	 (make-go-ext-type
	  :name (make-go-ext-identifier-from-node node)
	  :start beg :end end :node node))
	('pointer_type
	 (make-go-ext-type
	  :name (make-go-ext-identifier-from-node node)
	  :subtype 'pointer
	  :start beg :end end :node node))
	('slice_type
	 (make-go-ext-type
	  :name (make-go-ext-identifier-from-node node)
	  :subtype 'slice
	  :start beg :end end :node node))))))


;; --- Block

(cl-defstruct go-ext-block
  "Representation of a block."
  (start nil :type integer-or-marker)
  (end nil :type integer-or-marker)
  (node nil :type tsc-node
	:documentation "Original node this was made from."))

(go-ext-make-node-from-x-function go-ext-block
  (tree-sitter-ext-assert-node-type node block)
  (tree-sitter-ext-with-region node nil
    (make-go-ext-block :start beg :end end :node node)))


;; --- Struct

(cl-defstruct go-ext-struct
  "Representation of a struct type."
  (name nil :type go-ext-identifier
	:documentation "The name of the struct.")
  (start nil :type integer-or-marker)
  (end nil :type integer-or-marker)
  (node nil :type tsc-node
	:documentation "Original node this was made from."))

(go-ext-make-node-from-x-function go-ext-struct
  (tree-sitter-ext-assert-node-type node type_declaration)
  (-let* ((root node)
	  (node (tsc-get-first-named-child root)) ; `type_spec' node
	  (sname (->> (tsc-get-child-by-field node :name)
		      (make-go-ext-identifier-from-node)))
	  ((beg . end) (->> (tsc-get-child-by-field node :type)
			    (tsc-get-first-named-child)
			    (tree-sitter-ext-region-from-node))))
    (make-go-ext-struct :name sname :start beg :end end :node root)))


;; --- Field

(cl-defstruct go-ext-field
  "Representation of a struct field declaration."
  (name nil :type go-ext-identifier
	:documentation "The name of the field.")
  (owner nil :type go-ext-struct
	 :documentation "The struct this belongs to.")
  (start nil :type integer-or-marker)
  (end nil :type integer-or-marker)
  (node nil :type tsc-node
	:documentation "Original node this was made from."))

(go-ext-make-node-from-x-function go-ext-field
  (tree-sitter-ext-assert-node-type node field_declaration)
  (let ((c (tsc-make-cursor (tsc-root-node tree-sitter-tree))))
    (tsc-goto-first-child-for-byte c (tsc-node-start-byte node))
    (tree-sitter-ext-assert-node-type (tsc-current-node c) type_declaration)
    (make-go-ext-field :name (make-go-ext-identifier-from-node
			      (tsc-get-child-by-field node :name))
		       :owner (make-go-ext-struct-from-node
			       (tsc-current-node c))
		       :start (tsc-node-start-position node)
		       :end (tsc-node-end-position node)
		       :node node)))


;; --- Function

(cl-defstruct go-ext-function
  "Representation of a function."
  (name nil :type go-ext-identifier
	:documentation "The name of the function.")
  (start nil :type integer-or-marker)
  (body-start nil :type integer-or-marker)
  (end nil :type integer-or-marker)
  (body-end nil :type integer-or-marker)
  (result nil :documentation "The return type.")
  (node nil :type tsc-node
	:documentation "Original node this was made from."))

(go-ext-make-node-from-x-function go-ext-function
  (tree-sitter-ext-assert-node-type node func_literal function_declaration)
  (pcase (tsc-node-type node)
    ('function_declaration
     (tree-sitter-ext-with-region node
	 ((body-region (go-ext-tree-sitter-body-region node)))
       (make-go-ext-function
	:name (make-go-ext-identifier-from-node
	       (tsc-get-child-by-field node :name))
	:start beg
	:body-start (car body-region)
	:end end
	:body-end (cdr body-region)
	:result (when-let ((result (tsc-get-child-by-field node :result)))
		  (if (tree-sitter-ext-type-p result 'parameter_list)
		      (make-go-ext-parameter-list-from-node result)
		    (make-go-ext-type-from-node result)))
	:node node)))
    ('func_literal
     (tree-sitter-ext-with-region node
	 ((body-region (go-ext-tree-sitter-body-region node)))
       (make-go-ext-function
	:start beg
	:body-start (car body-region)
	:end end
	:body-end (cdr body-region)
	:result (when-let ((result (tsc-get-child-by-field node :result)))
		  (if (tree-sitter-ext-type-p result 'parameter_list)
		      (make-go-ext-parameter-list-from-node result)
		    (make-go-ext-type-from-node result)))
	:node node)))))

(defsubst make-go-ext-function-from-node--result (node)
  "Construct something from NODE.
NODE is assumed to be the :result node from its parent."
  (declare (side-effect-free t))
  )


;; --- Method

(cl-defstruct (go-ext-method
	       (:include go-ext-function))
  "Representation of a function."
  (receiver nil :type go-ext-variable))

(go-ext-make-node-from-x-function go-ext-method
  (tree-sitter-ext-assert-node-type node method_declaration)
  (tree-sitter-ext-with-region node
      ((ident-node (tsc-get-child-by-field node :name))
       (body-region (go-ext-tree-sitter-body-region node))
       (rcv-node (--> (tsc-get-child-by-field node :receiver)
		      (tsc-get-first-named-child it)
		      (tsc-get-child-by-field it :name))))
    (make-go-ext-method :name (make-go-ext-identifier-from-node ident-node)
			:start beg
			:body-start (car body-region)
			:end end
			:body-end (cdr body-region)
			:node node
			:receiver (make-go-ext-identifier-from-node rcv-node))))


;; ### Functions

(defmacro go-ext-debug-body (opt-level &rest body)
  "Do BODY if the optimization level is < OPT-LEVEL.
OPT-LEVEL is compared with `user-ext-go--optimize': if the
latter is less than the former, BODY is evaluated."
  (declare (indent 1) (debug (integerp &rest form)))
  (unless (> opt-level user-ext-go--optimize)
    (cons 'progn body)))

(defsubst go-ext--combine-lists (list1 list2 &optional compare-fn)
  (let* ((-compare-fn (or compare-fn)))
    (-union list1 list2)))

(define-obsolete-function-alias 'go-ext-tree-sitter-assert-node-type
  #'tree-sitter-ext-assert-node-type "2025-10-01")

(defun go-ext-pos-at-line-col (line column)
  "Return the buffer position at LINE and COLUMN."
  (save-excursion
    (goto-char (point-min))
    (forward-line line)
    (move-to-column column)
    (point)))

(defmacro go-ext-tempo-define-template (name doc elements)
  "Define a template NAME which inserts ELEMENTS.
The template will be called tempo-template-go-ext-NAME.
DOCSTRING is the documentation string for the command.
ELEMENTS is a list of elements recognized by
`tempo-define-template', which see.

The Go extension adds `tempo-ext-tempo-handler' to
`tempo-user-elements', so additional elements are available.
\(See the documentation for `tempo-ext-tempo-handler'.)

\(fn NAME DOCSTRING ELEMENTS)"
  (declare (indent defun) (doc-string 2)
	   (debug (&define name stringp sexp)))
  (cl-check-type name string)
  (cl-check-type doc string)
  (cl-check-type elements list)
  (let* ((name (format "go-ext-%s" name))
	 (fname (format "tempo-template-%s" name)))
    `(prog1 ',(intern fname)
       (tempo-define-template ,name ',elements nil ,doc))))

(defmacro go-ext-define-auxillery-skeleton (name nl &rest skel)
  "Define an auxillery skeleton NAME which inserts SKELETON.
The command will be called go-ext-skeleton--NAME.
DOCSTRING will be its documentation.
NEWLINE will be used as the value of `skeleton-end-newline'.

\(fn COMMAND NEWLINE SKELETON...)"
  (declare (debug (&define name booleanp skeleton-edebug-spec))
	   (indent defun))
  (cl-check-type name symbol)
  (let ((fname (intern (format "go-ext-skeleton--%S" name)))
	(doc-name (string-replace "-" " "
				  (symbol-name name))))
    `(prog1 ',fname
       (defun ,fname (&optional str arg)
	 ,(format "Insert a %s.

This is an auxillery skeleton command (see ‘skeleton-insert’).
Normally the skeleton text is inserted at point, with nothing \"inside\".
If there is a highlighted region, the skeleton text is wrapped
around the region text.
%s

A prefix argument ARG says to wrap the skeleton around the next ARG words.
A prefix argument of -1 says to wrap around region, even if not highlighted.
A prefix argument of zero says to wrap around zero words---that is, nothing.
This is a way of overriding the use of a highlighted region."
		  doc-name
		  (if nl "This inserts a newline at the end."
		    "This does not insert a newline at the end."))
	 (interactive "*P\nP")
	 (when (y-or-n-p ,(format "Insert %s? " doc-name))
	   (let ((skeleton-end-newline ,nl))
	     (skeleton-proxy-new ',skel str arg)))))))

(defmacro go-ext-define-skeleton (name doc &rest skel)
  "Define a skeleton NAME which inserts SKELETON.
The command will be called go-ext-skeleton-NAME.
DOCSTRING will be its documentation.

\(fn COMMAND DOCSTRING SKELETON...)"
  (declare (debug (&define name stringp skeleton-edebug-spec))
	   (indent defun) (doc-string 2))
  (cl-check-type name symbol)
  (cl-check-type doc string)
  (let ((fname (intern (format "go-ext-skeleton-%S" name))))
    `(prog1 ',fname
       (define-skeleton ,fname ,doc ,@skel))))

(define-obsolete-function-alias 'go-ext-tempo-on-region
  #'tempo-ext-on-region "2025-09-19")

(define-obsolete-function-alias 'go-ext--tempo-handle-if-condition
  #'tempo-ext--handle-if-condition "2025-09-19")

(define-obsolete-function-alias 'go-ext-tempo-handler
  #'tempo-ext-tempo-handler "2025-09-19")

(defun go-ext-autotype-file ()
  "Auto insert a basic file template."
  (interactive)
  (let ((bn (buffer-name))
	(bfn (buffer-file-name)))
    (cl-ext-cond
      ((and (string-match-p "\\`main\\.go" bn)
	    (y-or-n-p "Is this the main package? "))
       ;; main.go
       (call-interactively #'go-ext-skeleton-package-main))
      ((string-match "_test\\.go\\'" bn)
       (call-interactively #'tempo-template-go-ext-test-file))
      ((and (string-match-p "\\.go\\'" bn)
	    (not (string-match-p "\\`main\\.go" bn)))
       (call-interactively #'tempo-template-go-ext-go-file)))))

(defun go-ext-kill-buffer (buffer-or-name)
  "Kill the buffer specified by BUFFER-OR-NAME.
The argument is the same as for `kill-buffer' (which see),
except that BUFFER-OR-NAME cannot be nil."
  (cl-check-type buffer-or-name (or buffer string))
  (when (or (bufferp buffer-or-name)
	    (get-buffer buffer-or-name))
    (kill-buffer buffer-or-name)))

(defun go-ext-erase-buffers (&rest buffers-or-names)
  "Erase the contents of BUFFERS-OR-NAMES.
Each argument is either a buffer or the name of a buffer.
Erase each buffer listed as argument."
  (save-current-buffer
    (dolist (buffer-or-name buffers-or-names)
      (when (or (bufferp buffer-or-name)
		(get-buffer buffer-or-name))
	(set-buffer buffer-or-name)
	(and (bound-and-true-p view-mode)
	     (View-exit))
	(erase-buffer)))))

(defmacro go-ext-kill-buffer-on-error (buffer-name &rest body)
  "Evaluate BODY and kill BUFFER-NAME on error.
If, during the evaluation of BODY, there is an error,
display a message about it and kill BUFFER-NAME.
BUFFER-NAME is a string denoting a name of a buffer."
  (declare (indent 1) (debug ([&or stringp symbolp] body)))
  `(condition-case err
       (progn ,@body)
     (error (message "Error: %S" err)
	    (go-ext-kill-buffer ,buffer-name))))

(declare-function advice-ext--around-go-import-add "lsp-ext")
(fext-defadvice go-import-add (around go-import-add (oldfun arg import))
  (interactive (cons current-prefix-arg (go-ext--package-complete)))
  (funcall oldfun arg import))

(defun go-ext--buffer-string-visible ()
  "Return the visible parts of the buffer."
  (cl-loop with temp = (generate-new-buffer " *temp*")
	   with beg = (point-min)
	   with end = (point-max)
	   while (/= beg end)
	   do
	   (when (invisible-p beg)	; better than (get-char-property beg 'invisible)?
	     (setq beg (next-single-char-property-change
			beg 'invisible nil end)))
	   (let* ((next (next-single-char-property-change
			 beg 'invisible nil end))
		  (substring (buffer-substring beg next)))
	     (with-current-buffer temp (insert substring))
	     (setq beg next))
	   finally return
	   (cl-ext-progn
	     (deactivate-mark)
	     (prog1 (with-current-buffer temp
		      (s-chop-suffix "\n" (buffer-string)))
	       (kill-buffer temp)))))

(defun go-ext-read-type (&optional prompt-prefix use-default)
  "Prompt the user for a type.
The prompt will start with PROMPT-PREFIX.  PROMPT-PREFIX
should not end with whitespace or a colon.  If PROMPT-PREFIX
is nil, it defaults to \"Type\".
Optional argument USE-DEFAULT a."
  (let ((types (append (go-ext-custom-types)
		       (go-ext-buffer-types)
		       (go-ext-builtin-types)))
	(prompt-prefix (or prompt-prefix "Type"))
	(prompt (if (and use-default
			 (car-safe user-ext-go-read-type-history))
		    (format "%s (%s): " prompt-prefix (car user-ext-go-read-type-history))
		  (format "%s: " prompt-prefix))))
    (completing-read prompt
		     types
		     nil
		     nil
		     nil
		     'user-ext-go-read-type-history
		     (and use-default (car-safe user-ext-go-read-type-history)))))


;; --- Syntax Functions

(eval-when-compile
  (defmacro go-ext--forward-word (&optional arg)
    (if (fboundp 'forward-word-strictly)
	`(forward-word-strictly ,arg)
      `(forward-word ,arg))))

;; Needs to be here since it is a macro
(defmacro go-ext-tree-sitter--assert-valid-state ()
  "Verify that it's safe to use Tree Sitter functions."
  `(progn
     (or go-ext-tree-sitter-mode
	 (user-error "`go-ext-tree-sitter-mode' must be enabled"))
     (unless tree-sitter-tree
       (error "`tree-sitter-tree' is nil"))))

(defun go-ext-in-struct-p (&optional pos)
  "Return non-nil if POS is inside a struct.
Return a `go-ext-struct' if POS is inside a struct, nil
otherwise.  If POS is nil, default to point."
  (go-ext-tree-sitter--assert-valid-state)
  (tsc--save-context
    (when-let ((p (or pos (point)))
	       (node (tree-sitter-node-at-pos 'type_declaration)))
      (make-go-ext-struct-from-node node))))

(defun go-ext-in-field-p (&optional pos)
  "Return non-nil if POS is inside a struct field.
Return a `go-ext-field' if POS is inside a struct field, nil
otherwise.  If POS is nil, default to point."
  (go-ext-tree-sitter--assert-valid-state)
  (tsc--save-context
    (when-let ((p (or pos (point)))
	       (node (tree-sitter-node-at-pos 'field_declaration)))
      (make-go-ext-field-from-node node))))

(defun go-ext-in-function-p (&optional pos)
  "Return non-nil if POS is inside a function.
If POS is not provided, then it defaults to point.

This returns a `go-ext-function' object if POS is inside a
function."
  (go-ext-tree-sitter--assert-valid-state)
  (tsc--save-context
    (when-let ((p (or pos (point)))
	       (node (tree-sitter-node-at-pos 'function_declaration)))
      (make-go-ext-function-from-node node))))

(defun go-ext-in-lambda-p (&optional pos)
  "Return non-nil if POS is inside a lambda.
If POS is not provided, then it defaults to point.

This returns a `go-ext-function' object if POS is inside a
lambda."
  (go-ext-tree-sitter--assert-valid-state)
  (tsc--save-context
    (when-let ((p (or pos (point)))
	       (node (tree-sitter-node-at-pos 'func_literal)))
      (make-go-ext-function-from-node node))))

(defun go-ext-in-method-p (&optional pos)
  "Return non-nil if POS is inside a method.
If POS is not provided, then it defaults to point.

This returns a `go-ext-method' object if POS is inside a
method."
  (go-ext-tree-sitter--assert-valid-state)
  (tsc--save-context
    (when-let ((p (or pos (point)))
	       (node (tree-sitter-node-at-pos 'method_declaration)))
      (make-go-ext-method-from-node node))))

(defun go-ext-in-type-p (&optional pos)
  "Return non-nil if POS is on a type.
If POS is not provided, then it defaults to point.

This returns a `go-ext-type' object if POS is inside a
function."
  (go-ext-tree-sitter--assert-valid-state)
  (tsc--save-context
    (when-let ((p (or pos (point)))
	       (node (tree-sitter-node-at-pos 'type_declaration p)))
      (make-go-ext-type-from-node node))))

(defun go-ext-in-block-p (&optional pos)
  "Return non-nil if POS is inside a block.
If POS is not provided, then it defaults to point.

This returns a `go-ext-block' if non-nil."
  (go-ext-tree-sitter--assert-valid-state)
  (tsc--save-context
    (when-let ((pos (or pos (point)))
	       (node
		(tree-sitter-ext-node-at-position 'block pos)))
      (make-go-ext-block-from-node node))))

(defun go-ext-custom-types ()
  "Return a list of types from `user-ext-go-custom-types'."
  (interactive)
  (go-ext-tree-sitter--assert-valid-state)
  user-ext-go-custom-types)

(defun go-ext-buffer-types ()
  "Return a list of this buffer's type declarations."
  (interactive)
  (go-ext-tree-sitter--assert-valid-state)
  (let ((q (tsc-make-query tree-sitter-language
			   [(type_declaration
			     (type_spec
			      name: (type_identifier) @name
			      type: (_)))])))
    (tsc--save-context
      (cl-loop with captures = (tsc-query-captures
				q (tsc-root-node tree-sitter-tree)
				#'tsc--buffer-substring-no-properties
				go-ext-tree-sitter--query-cursor)
	       with node
	       for capture across captures
	       collect (cl-ext-progn
			 (setq node (cdr capture))
			 (tsc-node-text node))))))

(defun go-ext-builtin-types ()
  "Return a list of Go's builtin types."
  (declare (pure t) (side-effect-free t))
  (interactive)
  (eval-when-compile
    (cl-macrolet ((add-suffix
		   (a b)
		   (let ((result
			  (--map (concat a it) b)))
		     `',result)))
      `(,@(add-suffix "float" ("32" "64"))
	,@(add-suffix "int" ("" "8" "16" "32" "64"))
	,@(add-suffix "uint" ("" "8" "16" "32" "64"))
	,@(add-suffix "complete" ("64" "128"))
	"any" "comparable" "error"
	"uintptr" "rune" "string" "byte" "bool"))))

(fext-defadvice go-goto-docstring
    (override go-goto-docstring (&optional _arg))
  "Go to the top of the docstring of the current form.
Form in this case refers to a a function, struct, constant,
or variable--If there is none, add one beginning with the
name of the current form."
  (interactive "P")
  (cl-macrolet
      ((set-vars (type)
		 (let ((in-x-p (intern-soft (format "go-ext-in-%S-p" type)))
		       (x-node (intern-soft (format "go-ext-%S-node" type))))
		   (cl-assert in-x-p)
		   (cl-assert x-node)
		   `(setq cl-x (,in-x-p)
			  node (and cl-x (,x-node cl-x)))))
       (goto-x-doc (type)
		   (let ((x-name (intern-soft (format "go-ext-%S-name" type))))
		     (cl-assert x-name)
		     `(unless (go-ext--goto-doc node cl-x)
			(when-let ((ident (,x-name cl-x)))
			  (funcall insert-doc ident))))))
    (let ((insert-doc (lambda (ident &optional no-insert)
			(forward-line -1)
			(newline-and-indent)
			(unless no-insert
			  (insert "// " (go-ext-identifier-string ident)))))
	  cl-x node)
      (cond ((set-vars function)
	     (goto-x-doc function))
	    ;; Method
	    ((set-vars method)
	     (goto-x-doc method))
	    ;; Field
	    ((set-vars field)
	     (goto-x-doc field))
	    ;; Type
	    ((set-vars type)
	     (goto-x-doc type))
	    ;; Struct
	    ((set-vars struct)
	     (goto-x-doc struct))
	    (t (user-error "Nothing to do here"))))))

(defun go-ext--clx-start (cl-x)
  (cl-etypecase cl-x
    (go-ext-function
     (go-ext-function-start cl-x))
    (go-ext-struct
     (go-ext-struct-start cl-x))
    (go-ext-field
     (go-ext-field-start cl-x))
    (go-ext-type
     (go-ext-type-start cl-x))))

(defun go-ext--goto-doc (node cl-x)
  "Move point to where the doc comments are.
Return non-nil if there are any."
  (cl-macrolet ((tsc-node-is-type
		 (node type)
		 `(equal (tsc-node-type ,node) ,type)))
    (cl-loop
     with in-comment
     with cur-node = node
     with next-node = (tsc-get-prev-named-sibling cur-node)
     initially do
     (goto-char (go-ext--clx-start cl-x))
     while (and next-node (tsc-node-is-type next-node 'comment))
     do
     (setq in-comment t)
     (goto-char (tsc-node-start-position next-node))
     (setq cur-node next-node
	   next-node (tsc-get-prev-named-sibling cur-node))
     finally return
     (prog1 in-comment
       (goto-char (tsc-node-start-position cur-node))))))

(defun go-ext-get-function-name (&optional pos no-error)
  "Return the name of the function or method at POS.
POS defaults to point."
  (let ((pos (or pos (point)))
	cl-x)
    (cl-ext-cond
      ((setq cl-x (go-ext-in-method-p pos))
       (go-ext-identifier-string (go-ext-method-name cl-x)))
      ((setq cl-x (go-ext-in-function-p pos))
       (go-ext-identifier-string (go-ext-function-name cl-x)))
      (t (unless no-error
	   (error "Neither in a function or a method"))))))


;; --- Motion Functions

(defun go-ext-goto-struct (&optional arg)
  "Go to the struct definition surrounding point.
If ARG is nil, move point to the opening curly bracket,
otherwise move point to the opening type keyword.

When called interactively, ARG is the prefix argument."
  (interactive "P")
  (go-ext-tree-sitter--assert-valid-state)
  (if-let ((s (go-ext-in-struct-p)))
      (cl-ext-progn
	(if arg
	    (let ((node (go-ext-struct-node s)))
	      (cl-assert node)
	      (goto-char (byte-to-position (tsc-node-start-byte node))))
	  (goto-char (go-ext-struct-start s))))
    (user-error "Not inside a struct")))

(defun go-ext-goto-function-or-method ()
  "Go to the function or method surrounding point.
Depending on the actual key sequence, jump to different
parts of the function.

Keys:
- \\[go-goto-map] f - start of function/method block
- \\[go-goto-map] n - function/method name"
  (interactive)
  (go-ext-tree-sitter--assert-valid-state)
  (let (cl-x)
    (cl-ext-cond
      ;; Method
      ((setq cl-x (go-ext-in-method-p))
       (cl-ecase last-command-event
	 (?f (goto-char (go-ext-method-start cl-x)))
	 (?n (goto-char (go-ext-identifier-start
			 (go-ext-method-name cl-x))))))
      ;; Function
      ((setq cl-x (or (go-ext-in-lambda-p)
		      (go-ext-in-function-p)))
       (cl-ecase last-command-event
	 (?f (goto-char (go-ext-function-start cl-x)))
	 (?n (if-let ((ident (go-ext-function-name cl-x)))
		 (cl-ext-progn
		   (goto-char (go-ext-identifier-start ident)))
	       (user-error "Not inside a named function"))))))))

(defun go-ext-goto-package ()
  "Move point to the file's package declaration."
  (interactive)
  (go-ext-tree-sitter--assert-valid-state)
  (cl-macrolet ((type () (macroexp-quote 'package_clause)))
    (let ((cursor (tsc-make-cursor tree-sitter-tree))
	  node node-type)
      (tree-sitter-ext-goto first-child cursor)
      (until (equal node-type (type))
	(setq node (tsc-current-node cursor)
	      node-type (and node (tsc-node-type node)))
	(tree-sitter-ext-goto next-sibling cursor))
      (goto-char (tsc-node-start-position node))
      node)))

(defun go-ext-goto-block ()
  "Move point to the start of the block at point."
  (interactive)
  (go-ext-tree-sitter--assert-valid-state)
  (cl-ext-save-point
    (let ((block (go-ext-in-block-p))
	  done)
      (while (and (not done) block)
	(cl-ext-cond
	  ((= (point) (go-ext-block-start block))
	   (left-char 1)
	   (setq block (go-ext-in-block-p)))
	  (t (setq user-ext-cl--point (go-ext-block-start block)
		   done t)))))))
(--ignore
 (prog1 nil
   (with-current-buffer (get-buffer-create "*output*")
     (emacs-lisp-mode)
     (cl-prettyprint (symbol-function #'go-ext-goto-block))
     (run-with-idle-timer 0.2 nil #'activate-view-mode 1)
     (set-buffer-modified-p nil))
   (pop-to-buffer "*output*" t)
   (call-interactively #'menu-bar--toggle-truncate-long-lines))
 t)


;; --- Tree Sitter

(defvar-local go-ext-tree-sitter--query-cursor nil)

(defun go-ext-tree-sitter--setup ()
  (unless go-ext-tree-sitter--query-cursor
    (setq go-ext-tree-sitter--query-cursor (tsc-make-query-cursor)))
  (tree-sitter-ext-set-menu go-mode-map))

(defun go-ext-tree-sitter--teardown ()
  (when go-ext-tree-sitter--query-cursor
    (kill-local-variable 'go-ext-tree-sitter--query-cursor))
  (tree-sitter-ext-unset-menu go-mode-map))

(define-minor-mode go-ext-tree-sitter-mode
  "Syntax parsing of Go using tree sitter."
  :group 'go-ext
  (unless (eq major-mode 'go-mode)
    (go-ext-tree-sitter-mode 0)
    (user-error "`go-ext-tree-sitter-mode' only works in Go mode"))
  (tree-sitter--handle-dependent go-ext-tree-sitter-mode
    #'go-ext-tree-sitter--setup
    #'go-ext-tree-sitter--teardown))

;;;###autoload
(add-hook 'tree-sitter-query-mode-hook #'tree-sitter-query--extra-hook)


;; --- Tree Sitter Helper Functions

(defsubst go-ext-tree-sitter-body-region (node)
  "Return the region of NODE's :body field.
Return a cons of the form (START . END)."
  (go-ext-tree-sitter--assert-valid-state)
  (->> (tsc-get-child-by-field node :body)
       (tree-sitter-ext-region-from-node)))


;; --- Godoc

(fext-defadvice godoc--read-query (override godoc--read-query)
  "Attempt to use LSP when possible."
  (let* ((default (car go-godoc-history)))
    (cl-symbol-macrolet
	((prompt (format "Godoc (default %s): " default))
	 (packages (go-packages)))
      (when-let ((good (bound-and-true-p lsp-mode))
		 (result (go-ext-lsp--symbol-at-point)))
	(setq default result))
      (if godoc-use-completing-read
	  (completing-read prompt packages nil nil nil
			   'go-godoc-history default)
	(read-from-minibuffer prompt nil nil nil
			      'go-godoc-history default)))))

(defun go-ext-godoc--markup-string (str)
  (with-temp-buffer
    (with-demoted-errors "Error during doc rendering: %S"
      (insert str)
      (delay-mode-hooks (go-mode))
      (ignore-errors (font-lock-ensure)))
    (go-ext--buffer-string-visible)))

(defun go-ext-godoc--parse-string (str)
  "Parse STR for a Godoc buffer.

This returns either a list of the form (GO-STRING POST-STRING)
or a string."
  (if (string-match
       (go-ext-rx string-start
		  (group "package"
			 (*? anything)
			 line-start
			 (or (seq "func" (+ nonl))
			     (seq "type"
				  (+? anything)
				  line-start
				  ?\}
				  line-end)))
		  (group (* anything)))
       str)
      (let ((gstr (match-string-no-properties 1 str))
	    (pstr (match-string-no-properties 2 str)))
	(go-ext-debug-body 0
	  (--print-expr var gstr)
	  (--print-expr var pstr))
	(list (go-ext-godoc--markup-string gstr) pstr))
    str))

(define-obsolete-function-alias 'go-ext-godoc-server--prompt-args
  'go-ext-godoc--prompt-args "2025-09-24")
(defun go-ext-godoc--read-args (prompt &optional arg)
  (thread-last
      (if arg
	  (read-string prompt
		       user-ext-go-godoc-default-args
		       'user-ext-go-godoc-args-history
		       user-ext-go-godoc-default-args)
	user-ext-go-godoc-default-args)
    (split-string-and-unquote)))

(defun go-ext-godoc (query &optional args)
  "Show Go documentation for QUERY."
  (interactive (list (godoc--read-query)
		     (go-ext-godoc--read-args "Godoc: " current-prefix-arg)))
  (cl-check-type query string)
  (let ((outbuf (get-buffer-create user-ext-go-godoc-output-buffer))
	(errbuf (get-buffer-create user-ext-go-godoc-error-buffer))
	(tmpfile (make-temp-file "godoc")))
    (go-ext-erase-buffers outbuf errbuf)
    (unwind-protect
	(when-let ((command (append (split-string-and-unquote godoc-command)
				    args
				    (list query)))
		   (insert-error
		    (lambda ()
		      (with-current-buffer errbuf
			(insert-file-contents tmpfile)
			(save-excursion
			  (goto-char (point-max))
			  (insert (format "\ncommand: %S [%S]"
					  (combine-and-quote-strings command)
					  command)))
			(activate-view-mode)))))
	  (setf (car command) (executable-find (car command)))
	  (or (car command)
	      (error "go binary not in PATH"))
	  (let ((default-directory (go-ext-project-root (go-ext-project-current))))
	    (pcase (call-process-shell-command
		    (combine-and-quote-strings command)
		    nil
		    (list outbuf tmpfile))
	      ((and (pred integerp)
		    code
		    (guard (/= code 0)))
	       (funcall insert-error)
	       (display-buffer errbuf t)
	       (error "Godoc exited with status %d" code))
	      ((and (pred stringp)
		    sigstr)
	       (error "Godoc received signal: %s" sigstr))))
	  (funcall insert-error)
	  (let (output)
	    (with-current-buffer outbuf
	      (let ((bstr (buffer-string)))
		(setq output
		      (pcase (go-ext-godoc--parse-string bstr)
			(`(,gstr ,pstr)
			 (concat gstr pstr))
			('nil (--print-expr sexp (go-ext-godoc--parse-string bstr))
			      (error "Failed to parse documentation string"))
			(str str))))
	      (erase-buffer)
	      (save-excursion (insert output))
	      (activate-view-mode))
	    (display-buffer outbuf t)))
      (with-demoted-errors "Error deleting temp file: %S"
	(go-ext-debug-body 1
	  (message "Deleting %s" tmpfile))
	(delete-file tmpfile)))))
(advice-add #'godoc :override #'go-ext-godoc)

(defun go-ext-godoc-server--running ()
  (when-let ((buffer (get-buffer user-ext-go-godoc-server-process-buffer))
	     (process (get-buffer-process buffer)))
    t))

(defun go-ext-godoc-server-stop ()
  "Stop a running Godoc server."
  (interactive)
  (when (go-ext-godoc-server--running)
    (go-ext-kill-buffer user-ext-go-godoc-server-process-buffer)))

(defun go-ext-godoc-server ()
  "Start Go documentation server.

The output is shown in a process buffer with the name taken
from `user-ext-go-godoc-server-process-buffer'."
  (interactive)
  (go-ext-kill-buffer-on-error user-ext-go-godoc-server-process-buffer
    (let* ((command
	    (append (split-string-and-unquote godoc-command)
		    (list "-http")))
	   (buffer (go-ext-go-process-run-command
		    command user-ext-go-godoc-server-process-buffer
		    :working-directory (go-ext-project-get-root))))
      (and (bufferp buffer)
	   (display-buffer buffer t)))))


;; --- Packages

(defun go-ext--package-complete ()
  (list
   (replace-regexp-in-string
    "^[\"']\\|[\"']$" ""
    (completing-read "Package: " (go-packages) nil nil nil 'user-ext-go-package-history))))

(defun go-ext-add-dependency (pkg)
  "Add PKG to the project using go get."
  (interactive (go-ext--package-complete))
  (go-ext-kill-buffer-on-error user-ext-go-process-buffer
    (let* ((command (append (split-string-and-unquote go-command)
			    (list "get" pkg)))
	   (buffer (go-ext-go-process-run-command
		    command user-ext-go-process-buffer
		    :working-directory (go-ext-project-get-root))))
      (and (bufferp buffer)
	   (display-buffer buffer t)))))


;; --- Docedit scratch buffer

(defgroup go-ext-docedit nil
  "Group for Go docedit."
  :group 'go-ext)

(defface go-ext-docedit-edit-overlay-face
  '((((min-colors 88) (background dark))
     (:background "yellow1" :foreground "black"))
    (((background dark)) (:background "yellow" :foreground "black"))
    (((min-colors 88)) (:background "yellow1"))
    (t (:background "yellow")))
  "Face for `go-ext-docedit-overlay-category'."
  :group 'go-ext-docedit)

(defconst go-ext-docedit-overlay-category
  (eval-and-compile
    (let ((cat 'go-ext-overlay-edit))
      (setplist cat '(face go-ext-docedit-edit-overlay-face start cursor-intangible t))
      cat)))

(defconst go-ext-docedit-buffer "*go docedit*")

(defvar-local go-ext-docedit--type nil)

(defvar-local go-ext-docedit--indentation nil)

(defvar-local go-ext-docedit--start nil)

(defvar-local go-ext-docedit--end nil)
(make-obsolete-variable 'go-ext-docedit--end nil "2025-09-17")

(defvar-local go-ext-docedit--source-buffer nil)

(defconst go-ext-docedit-register ?g)

(defconst go-ext-docedit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'go-ext-docedit-apply)
    (define-key map [remap save-buffer] #'go-ext-docedit-save)
    map)
  "Keymap for docedit minor mode.")

(define-minor-mode go-ext-docedit-mode
  "Text scratch mode."
  :group 'go-ext
  :lighter " Go Docedit"
  :keymap go-ext-docedit-mode-map
  (if go-ext-docedit-mode
      (cl-ext-progn
	(auto-fill-mode 1)
	(display-fill-column-indicator-mode 1))
    (auto-fill-mode 0)
    (display-fill-column-indicator-mode 0)))

(defun go-ext-docedit--find-overlay (&optional pos)
  (cl-loop with p = (or pos (point))
	   for ov being the overlays from p
	   do
	   (when (eq (overlay-get ov 'category)
		     go-ext-docedit-overlay-category)
	     (cl-return ov))))

(defun go-ext-docedit-save ()
  ;; TODO: documentation string
  (interactive "*")
  (or (buffer-modified-p)
      (user-error "Nothing to save"))
  (let (string)
    (cl-declare (special p))
    (go-ext-docedit--validate-vars)
    (cl-ecase go-ext-docedit--type
      (?s (setq string (buffer-string-no-properties))
	  (let ((beg go-ext-docedit--start) end
		ov)
	    (with-current-buffer go-ext-docedit--source-buffer
	      (when-let ((ov (go-ext-docedit--find-overlay beg))
			 (start (overlay-start ov))
			 (end (overlay-end ov)))
		(delete-overlay ov)
		(delete-region start end))
	      (goto-char beg)
	      (princ string (current-buffer))
	      (setq end (point-marker)
		    ov (make-overlay beg end))
	      (overlay-put ov 'category go-ext-docedit-overlay-category)))
	  (set-buffer-modified-p nil))
      (?c (setq string (buffer-string-no-properties))
	  (with-temp-buffer
	    ;; Add "// " to the start of each line, then get the string
	    (save-excursion (princ string (current-buffer)))
	    (until (eobp)
	      (beginning-of-line)
	      (insert "// ")
	      (forward-line 1))
	    (setq string (buffer-string-no-properties)))
	  (let ((beg go-ext-docedit--start) end p ov)
	    (cl-assert beg)
	    (with-current-buffer go-ext-docedit--source-buffer
	      (when-let ((ov (go-ext-docedit--find-overlay beg))
			 (start (overlay-start ov))
			 (end (overlay-end ov)))
		;; Delete the "edit" overlay
		(delete-overlay ov)
		(delete-region start end))
	      (setq p (cl-ext-progn
			(goto-char beg)
			(point-marker))
		    end (cl-ext-progn
			  (princ string (current-buffer))
			  (point-marker))
		    ov (make-overlay beg end))
	      (overlay-put ov 'category go-ext-docedit-overlay-category)
	      (indent-region beg end))
	    (set-buffer-modified-p nil))))))

(defun go-ext-docedit-apply ()
  "\"Apply\" the contents of the buffer and close the buffer."
  (interactive)
  (let ((pos go-ext-docedit--start)
	string)
    (cl-ecase go-ext-docedit--type
      (?s (setq string (buffer-string-no-properties))
	  (kill-region (point-min) (point-max))
	  (kill-buffer)
	  (jump-to-register go-ext-docedit-register)
	  (when-let ((ov (go-ext-docedit--find-overlay pos))
		     (start (overlay-start ov))
		     (end (overlay-end ov)))
	    (delete-overlay ov)
	    (delete-region start end))
	  (princ string (current-buffer)))
      (?c (goto-char (point-min))
	  (until (eobp)
	    (insert "// ")
	    (forward-line 1))
	  (setq string (buffer-string-no-properties))
	  (let (p)
	    (kill-buffer)
	    (jump-to-register go-ext-docedit-register)
	    (when-let ((ov (go-ext-docedit--find-overlay pos))
		       (start (overlay-start ov))
		       (end (overlay-end ov)))
	      (delete-overlay ov)
	      (delete-region start end))
	    (setq p (point-marker))
	    (princ string (current-buffer))
	    (indent-region p (point)))))))

(defun go-ext-docedit--validate-vars ()
  (cl-macrolet ((assert-non-nil
		 (symbol)
		 `(or ,symbol
		      (error ,(format "`%S' is nil" symbol)))))
    (assert-non-nil go-ext-docedit--type)
    (cl-check-type go-ext-docedit--type character)
    (assert-non-nil go-ext-docedit--start)
    (cl-check-type go-ext-docedit--start marker)
    ;; (assert-non-nil go-ext-docedit--end)
    ;; (cl-check-type go-ext-docedit--end marker)
    (assert-non-nil go-ext-docedit--source-buffer)
    (cl-check-type go-ext-docedit--source-buffer buffer)))
(cl-define-compiler-macro go-ext-docedit--validate-vars
    (&whole form)
  (when (or (not (cl--compiling-file))
	    (< cl--optimize-speed 3) (= cl--optimize-safety 3))
    form))

(defun go-ext-docedit ()
  "Edit the string at point."
  (interactive)
  (let ((old-buffer (current-buffer))
	bounds string beg end)
    (cond
     ;; Inside a line comment
     ((save-excursion
	(end-of-line)
	(eq (go-in-comment-p) t))
      (let ((indent (current-indentation)))
	(beginning-of-line)
	(setq beg (cl-ext-progn
		    ;; Go up line by line until a non-comment line,
		    ;; then go to the beginning of the first comment line
		    (while (and (not (bobp))
				(looking-at-p "^\\s-*//"))
		      (forward-line -1))
		    (unless (bobp)
		      (forward-line 1))
		    (point-marker))
	      end (cl-ext-progn
		    ;; Find the end point of the comment lines
		    (while (looking-at-p "^\\s-*//")
		      (forward-line 1))
		    (forward-line -1)
		    (end-of-line)
		    (point-marker))
	      string (buffer-substring-no-properties beg end))
	(window-configuration-to-register go-ext-docedit-register)
	(condition-case err
	    (cl-ext-progn
	      (and (> end beg) (kill-region beg end))
	      (go-ext-docedit--buffer)
	      (setq go-ext-docedit--type ?c
		    go-ext-docedit--indentation indent
		    go-ext-docedit--source-buffer old-buffer
		    go-ext-docedit--start beg
		    ;; go-ext-docedit--end end
		    )
	      (princ string (current-buffer))
	      (go-ext-docedit--validate-vars)
	      (goto-char (point-min))
	      (save-excursion
		(until (eobp)
		  (when (looking-at "^[ \t]*//[ \t]*\\(.*\\)")
		    (replace-match "\\1"))
		  (forward-line 1))))
	  (error (message "`go-ext-docedit' error: %S" err)
		 (go-ext-kill-buffer go-ext-docedit-buffer)
		 (jump-to-register go-ext-docedit-register)))))
     ;; Inside a string
     ((and (setq bounds (thing-at-point-ext-bounds-of-string-at-point)
		 beg (let ((it (car-safe bounds)))
		       (and it (1+ it)))
		 end (let ((it (cdr bounds)))
		       (and it (1- it)))
		 string (if (and beg end (> end beg))
			    (buffer-substring-no-properties beg end)
			  ""))
	   beg end string)
      (window-configuration-to-register go-ext-docedit-register)
      (condition-case err
	  (cl-ext-progn
	    (and (> end beg) (kill-region beg end))
	    (go-ext-docedit--buffer)
	    (let ((beg beg)
		  (end end))
	      (with-current-buffer old-buffer
		(setq beg (save-excursion
			    (goto-char beg)
			    (point-marker))
		      end (save-excursion
			    (goto-char end)
			    (point-marker))))
	      (setq go-ext-docedit--source-buffer old-buffer
		    go-ext-docedit--type ?s
		    go-ext-docedit--start beg
		    ;; go-ext-docedit--end end
		    ))
	    (princ string (current-buffer))
	    (go-ext-docedit--validate-vars))
	(error (message "`go-ext-docedit' error: %S" err)
	       (go-ext-kill-buffer go-ext-docedit-buffer)
	       (jump-to-register go-ext-docedit-register)
	       (princ string old-buffer))))
     (t (user-error "Cannot do anything here")))))

(define-scratch-buffer-function go-ext-docedit--buffer "go docedit" nil
  "Buffer for `go-ext-docedit'."
  nil
  (text-mode)
  (go-ext-docedit-mode 1))


;; --- Hs Mode Integration

(define-obsolete-function-alias 'go-ext-hide-range
  #'hs-ext-hide-range "2025-10-06")

(defun go-ext-hide-struct ()
  "Hide the struct at point.
If point is not inside function, then do nothing.

Upon completion, point is repositioned and the normal hook
`hs-hide-hook' is run.  See documentation for `run-hooks'."
  (interactive)
  (hs-minor-mode 1)
  (go-ext-tree-sitter--assert-valid-state)
  (when-let ((f (go-ext-in-struct-p))
	     (beg (1+ (go-ext-struct-start f)))
	     (end (1- (go-ext-struct-end f))))
    (hs-make-overlay beg end 'code)
    (goto-char beg)
    (run-hooks 'hs-hide-hook)))

(defun go-ext-hide-function ()
  "Hide the function at point.
If point is not inside function, then do nothing.

Upon completion, point is repositioned and the normal hook
`hs-hide-hook' is run.  See documentation for `run-hooks'."
  (interactive)
  (hs-minor-mode 1)
  (go-ext-tree-sitter--assert-valid-state)
  (let (f beg end)
    (cl-ext-cond
      ;; Anonymous function
      ((setq f (go-ext-in-lambda-p)
	     beg (and f (go-ext-function-body-start f))
	     end (and beg (go-ext-function-body-end f)))
       (hs-ext-hide-range (1+ beg) (1- end) 'code))
      ;; Named function
      ((setq f (go-ext-in-function-p)
	     beg (and f (go-ext-function-body-start f))
	     end (and beg (go-ext-function-body-end f)))
       (hs-ext-hide-range (1+ beg) (1- end) 'code))
      (t (user-error "Not inside a function or lambda")))))

(defun go-ext-hide-block ()
  "Hide the block at point.

Upon completion, point is repositioned and the normal hook
`hs-hide-hook' is run.  See documentation for `run-hooks'."
  (interactive)
  (hs-minor-mode 1)
  (go-ext-tree-sitter--assert-valid-state)
  (if-let ((f (go-ext-in-block-p)))
      (let ((beg (1+ (go-ext-block-start f)))
	    (end (1- (go-ext-block-end f))))
	(hs-ext-hide-range beg end 'code))
    (user-error "Not inside a block")))

(defun go-ext-hide-method ()
  "Hide the method at point.
If point is not inside method, then do nothing.

Upon completion, point is repositioned and the normal hook
`hs-hide-hook' is run.  See documentation for `run-hooks'."
  (interactive)
  (hs-minor-mode 1)
  (go-ext-tree-sitter--assert-valid-state)
  (if-let ((f (go-ext-in-method-p))
	   (beg (1+ (go-ext-method-body-start f)))
	   (end (1- (go-ext-method-body-end f))))
      (cl-ext-progn
	(hs-ext-hide-range beg end 'code))
    (user-error "Not inside a method")))

(cl-defmacro go-ext-define-hide-all-x-function
    (what pattern &key (tag 'block) beg-forms end-forms)
  "Define a function for hiding all WHAT forms in the buffer.
The function will be named go-ext-hide-all-WHAT.
WHAT must a string that ends with \"s\".
PATTERN is used to search the syntax tree for the WHAT forms;
it is either an S-expression pattern or a vector of
S-expression patterns (see `tsc-make-query').

The created function hides the text in the range [S,E],
inclusive, where S is the start of the range and E is the
end.  It finds the text to hide by searching the syntax for
nodes using PATTERN.
The pattern is expected to capture the node that represents
the body of WHAT by using a tag.  The name of the tag is
assumed to be `block' (to change this behavior, provide the
:tag keyword).

Here is an example of a valid query:

   (type_declaration
    (type_spec
     name: (type_identifier)
     type: (struct_type
            (field_declaration_list) @block)))

The rest of the arguments are keyword arguments.
- The keyword :tag, if provided, changes the name of the
tag used to capture the node of interest.
- The keyword :beg-forms, if provided, can be used to change
S; it is a list of Lisp forms which are then wrapped is an
`save-excursion' form.  The last FORM becomes the new value
of S.
- The keyword :end-forms, if provided, can be used to change
E; it has the same syntax and usage as :beg-forms, but for E
instead.

See also: <https://emacs-tree-sitter.github.io/syntax-highlighting/queries/>.

\(fn WHAT PATTERN ARG...)"
  (declare (indent defun)
	   (debug (&define stringp vectorp (def-body) (def-body))))
  (cl-check-type what string)
  (cl-check-type pattern (or vector list))
  (cl-assert (string-match-p "s\\'" what) nil "function must end in \"s\"")
  (let ((fname (intern (format "go-ext-hide-all-%s" what)))
	(pattern (if (vectorp pattern) pattern
		   (vector pattern))))
    `(progn
       (defun ,fname ()
	 ,(s-lex-format "Hide all ${what} in the buffer.")
	 (interactive)
	 (hs-minor-mode 1)
	 (go-ext-tree-sitter--assert-valid-state)
	 (cl-ext-save-point
	   (save-restriction
	     (widen)
	     (let ((spew (make-progress-reporter
			  ,(s-lex-format "Hiding all ${what}...")
			  (point-min)
			  (point-max)))
		   (q (tsc-make-query tree-sitter-language
				      ,(macroexp-quote pattern))))
	       (cl-loop with root = (tsc-root-node tree-sitter-tree)
			with captures = (tsc-query-captures
					 q
					 root
					 #'tsc--buffer-substring-no-properties)
			for capture across captures
			do
			(when (and (consp capture)
				   (eq (car capture) ',tag))
			  (-let* ((node (cdr capture))
				  ((beg . end) (tree-sitter-ext-region-from-node node)))
			    ,@(when beg-forms
				`((setq beg
					(save-excursion
					  (goto-char beg)
					  ,@beg-forms))))
			    ,@(when end-forms
				`((setq end
					(save-excursion
					  (goto-char end)
					  ,@end-forms))))
			    (or hs-allow-nesting
				(hs-discard-overlays beg end))
			    (hs-ext-hide-range (1+ beg) (1- end) 'code nil nil t)
			    (progress-reporter-update spew end)
			    (when (<= beg user-ext-cl--point end)
			      (setq user-ext-cl--point beg))))
			finally do
			(progress-reporter-done spew)
			(run-hooks 'hs-hide-hook)))))))))

(go-ext-define-hide-all-x-function "interfaces"
  (type_declaration
   (type_spec
    name: (_)
    type: (interface_type) @block))
  :beg-forms ((skip-chars-forward "^{")
	      (point)))

(go-ext-define-hide-all-x-function "structs"
  (type_declaration
   (type_spec
    name: (type_identifier)
    type: (struct_type
	   (field_declaration_list) @block))))

(go-ext-define-hide-all-x-function "functions"
  [(function_declaration
    "func" @keyword
    body: (block) @block)
   (method_declaration
    ("func") @keyword
    body: (block) @block)])


;; --- LSP

(defun go-ext-lsp--symbol-at-point ()
  (cl-flet ((dump-hash
	     (hash)
	     (cl-loop for key being the hash-keys in hash
		      using (hash-values val)
		      do
		      (message "%S = %S" key val))))
    (when-let ((response (->>
			  (lsp--text-document-position-params)
			  (lsp--make-request "textDocument/hover")
			  (lsp--send-request)
			  (lsp:hover-contents)))
	       (contents (gethash "value" response)))
      (cl-ext-cond
	;; Methods
	;; func (b *BoolArgVar) Set(s string) error
	((string-match "^func (.+ \\*?\\(.+\\)) \\(.+\\)(" contents)
	 (format "%s.%s"
		 (match-string-no-properties 1 contents)
		 (match-string-no-properties 2 contents)))
	;; Package functions
	((string-match "^func \\(.+\\)(" contents)
	 (match-string-no-properties 1 contents))))))


;; ### Project


;; --- Customization

(defgroup go-ext-project nil
  "Go projects."
  :group 'go-ext)

(defcustom user-ext-go-project-ignores nil
  "List of glob patterns to ignore in a Go project.
This has the same format as `project-vc-ignores', which see."
  :group 'go-ext-project
  :type '(repeat string)
  :safe #'list-of-strings-p)


;; --- Functions

(cl-defstruct (go-ext-project (:type list)
			      :named)
  (root default-directory :type string
	:documentation "Root directory of the project.")
  (name "" :type string
	:documentation "Name of the project."))

(defsubst go-ext-project-get-root ()
  "Return the root of the current project."
  (go-ext-project-root (go-ext-project-current)))

(defun go-ext-project-current (&optional maybe-prompt no-error)
  (let ((project (project-current maybe-prompt)))
    (if (and project (go-ext-project-p project))
	project
      (unless no-error
	(user-error "Not in Go project")))))
(--ignore
 (prog1 nil
   (with-current-buffer (get-buffer-create "*output*")
     (emacs-lisp-mode)
     (cl-prettyprint (symbol-function #'go-ext-project-current))
     (run-with-idle-timer 0.2 nil #'activate-view-mode 1)
     (set-buffer-modified-p nil))
   (pop-to-buffer "*output*" t)
   (call-interactively #'menu-bar--toggle-truncate-long-lines))
 t)

(defun go-ext-project-parent (file)
  "Return the parent of FILE within the Go project."
  (let* ((file (abbreviate-file-name file))
	 (project (go-ext-project-current t))
	 (root (project-root project))
	 (parent (f-common-parent (list root file))))
    parent))

(defun go-ext-project-rename-buffer ()
  (interactive)
  (when-let ((file (abbreviate-file-name (buffer-file-name)))
	     (parent (abbreviate-file-name (go-ext-project-parent file))))
    (rename-buffer (f-no-ext
		    (string-remove-prefix parent file)))))

(defun go-ext-try-project (dir)
  (when-let ((root (locate-dominating-file default-directory "go.mod")))
    (make-go-ext-project :root root
			 :name (f-filename root))))

(defun go-ext-project-package ()
  "Return the package this file belongs in."
  (let* ((file (buffer-file-name))
	 (path (f-split file)))
    (cl-ext-cond
      ((let ((main (f-full "main.go")))
	 (and (f-exists-p main)
	      (f-same-p (f-dirname (buffer-file-name))
			(f-dirname main))))
       ;; This file's in the same directory as main.go
       "main")
      ((go-ext-project-parent file)
       (car (last path 2))))))

(cl-defmethod project-root ((project (head go-ext-project)))
  (go-ext-project-root project))

(cl-defmethod project-ignores ((project (head go-ext-project)) dir)
  (let* ((root (go-ext-project-root project))
         (backend 'Git))
    (append
     (when (and backend (file-equal-p dir root))
       (delq
        nil
        (mapcar
         (lambda (entry)
           (cond
            ((eq ?! (aref entry 0))
             ;; No support for whitelisting (yet).
             nil)
            ((string-match "\\(/\\)[^/]" entry)
             ;; FIXME: This seems to be Git-specific.
             ;; And / in the entry (start or even the middle) means
             ;; the pattern is "rooted".  Or actually it is then
             ;; relative to its respective .gitignore (of which there
             ;; could be several), but we only support .gitignore at
             ;; the root.
             (if (= (match-beginning 0) 0)
                 (replace-match "./" t t entry 1)
               (concat "./" entry)))
            (t entry)))
         (condition-case nil
             (vc-call-backend backend 'ignore-completion-table root)
           (vc-not-supported () nil)))))
     (project--value-in-dir 'user-ext-go-project-ignores root)
     (mapcar
      (lambda (dir)
        (concat dir "/"))
      vc-directory-exclusion-list))))

(cl-defmethod project-files ((project (head go-ext-project)) &optional dirs)
  (mapcan
   (lambda (dir)
     (let ((ignores (project--value-in-dir 'user-ext-go-project-ignores dir))
	   (backend 'Git))
       (require 'vc-git)
       (if (and (file-equal-p dir (go-ext-project-root project))
		(cl-ext-cond
                  ((eq backend 'Hg) t)
                  ((and (eq backend 'Git)
			(or
                         (not ignores)
                         (version<= "1.9" (vc-git--program-version))))
		   t)))
	   (project--vc-list-files dir backend ignores)
	 (project--files-in-directory
          dir
          (project--dir-ignores project dir)))))
   (or dirs (list (project-root project)))))

(add-hook 'project-find-functions #'go-ext-try-project)
(--ignore
 (remove-hook 'project-find-functions #'go-ext-try-project)
 t)


;; ### Process buffer

(defconst go-ext-go-process-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map comint-mode-map)
    (define-key map [remap self-insert-command] #'go-ext-process-no-edit)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "k") #'go-ext-process-kill-buffer)
    map))

(defun go-ext-process-no-edit ()
  "Prohibit the user from editing the buffer."
  (interactive)
  (user-error "Cannot to edit this buffer"))

(defun go-ext-process-kill-buffer ()
  "Kill the process associated with the current buffer.

Internally, this sends a SIGINT signal to the subprocess via
`comint-interrupt-subjob', then kills the buffer and quits.

This only works when the curent buffer's major mode is
`go-ext-go-process-mode'."
  (interactive)
  (let ((buffer (current-buffer)))
    (when (eq major-mode 'go-ext-go-process-mode)
      (with-demoted-errors "Kill buffer error: %S"
	(comint-interrupt-subjob))
      (while (comint-check-proc buffer)
	(sleep-for 0.1 100)))
    (kill-and-quit)))

(define-derived-mode go-ext-go-process-mode comint-mode
  "Go Process"
  "Major mode for Go processes.

\\{go-ext-go-process-mode-map}"
  (setq-local comint-process-echoes nil))

(cl-defun go-ext-go-process-run-command
    (command buffer-name &key working-directory query-on-exit)
  "Run COMMAND in a process buffer called BUFFER-NAME.
COMMAND is a list of the form (PROGRAM ARG...), where
PROGRAM is a string denoting an executable program, and each
ARG is an argument to PROGRAM.  If WORKING-DIRECTORY is
non-nil, set it as the working directory for the process,
otherwise use `default-directory'.

BUFFER-NAME and its process are killed intially before
creating a new one."
  (when-let ((buffer (get-buffer buffer-name))
	     (process (get-buffer-process buffer)))
    (and (buffer-live-p process)
	 (delete-process process)))
  (go-ext-kill-buffer buffer-name)
  (-let* ((buffer (get-buffer-create buffer-name))
	  ((program . args) command)
	  (program-name (f-filename program))
	  (wd (or working-directory default-directory)))
    (with-current-buffer buffer
      (go-ext-go-process-mode)
      (setq default-directory (file-name-as-directory wd)
	    header-line-format (format "Status: %%s | Working directory: %s"
				       default-directory)))
    (comint-exec buffer program-name program nil args)
    (run-with-idle-timer 0.5 nil
			 (lambda ()
			   (when-let ((process (get-buffer-process buffer)))
			     (cl-ext-progn
			       (set-process-query-on-exit-flag process query-on-exit)))))
    buffer))
(define-obsolete-function-alias 'go-ext-process-run-command
  'go-ext-go-process-run-command "2025-09-24")


;; ### Templates/Skeletons

(defun go-ext--before-ts-skeleton (&rest _r)
  "Assertions and error checks done before a skeleton."
  (go-ext-tree-sitter--assert-valid-state))

(defun go-ext-skeleton-no-newline (fun &rest r)
  "Set `skeleton-end-newline' to nil."
  (let (skeleton-end-newline)
    (apply fun r)))

(go-ext-define-auxillery-skeleton type-parameter-list
  nil nil
  ?\[ ("Type Parameter %s: "
       (unless (equal ?\[ (char-before)) ", ")
       str " " (skeleton-read "Constraint: "))
  resume:
  ?\])

(go-ext-define-skeleton function
  "Insert a function definition."
  "Name: "
  "func " str
  '(go-ext-skeleton--type-parameter-list)
  ?\( ("Parameter: "
       (unless (equal ?\( (char-before)) ", ")
       str ?\ (go-ext-read-type "Parameter Type" t))
  ?\) (progn (setq v1 (go-ext-read-type "Return Type"))
	     (unless (string-empty-p v1)
	       (concat " " v1)))
  " {" \n
  _ \n
  ?\} >)

(go-ext-define-skeleton test-function
  "Insert a test function."
  "Name: "
  "func Test" str "(t *testing.T) {" \n
  _ \n
  ?\} >)

(go-ext-define-skeleton lambda
  "Insert an anonymous function."
  str
  "func(" ("Parameter: "
	   (unless (equal ?\( (char-before)) ", ")
	   str ?\ (go-ext-read-type "Parameter Type" t))
  ?\) (progn (setq v1 (go-ext-read-type "Return Type"))
	     (and (not (string-empty-p v1))
		  (concat " " v1)))
  " {" \n
  _ \n
  ?\} >)
(advice-add 'go-ext-skeleton-lambda :around #'go-ext-skeleton-no-newline)

(go-ext-define-skeleton method
  "Insert a struct method."
  "Name: "
  "func (" (read-string "Receiver: " nil t)
  ?\  (go-ext-read-type "Receiver Type" t)
  ") " str ?\( ("Parameter: "
		(unless (equal ?\( (char-before)) ", ")
		str ?\ (go-ext-read-type "Parameter Type" t))
  ?\) (progn (setq v1 (go-ext-read-type "Return Type"))
	     (and (not (string-empty-p v1))
		  (concat " " v1)))
  " {" \n _ \n ?\} >)
(advice-add 'go-ext-skeleton-method :before #'go-ext--before-ts-skeleton)

(go-ext-define-skeleton package-main
  "Insert a package main."
  nil
  "package main" \n \n
  "func main() {" \n
  _ \n
  ?\} >)

(go-ext-define-skeleton struct
  "Insert a struct definition."
  "Name: "
  "type " str " struct {" \n
  ("Field Name: " str ?\  (go-ext-read-type "Field Type" t) \n)
  ?\} >)

(go-ext-define-skeleton anonymous-struct
  "Insert an anonymous struct."
  nil
  "struct {" \n
  ("Field Name: " str ?\  (read-string "Type: " nil t) \n)
  ?\} >)

(go-ext-define-skeleton interface
  "Insert an integerface."
  "Name: "
  "type " str " interface {" \n
  ("Method Name: " str ?\( ("(Optional Parameter) & Type: "
			    (unless (equal ?\( (char-before)) ", ")
			    str)
   ?\) \n)
  resume:
  ?\} >)

(go-ext-tempo-define-template "go-file"
  "Insert a Go file for the current package."
  ("package " (go-ext-project-package) \n
   \n r \n))

(go-ext-tempo-define-template "test-file"
  "Insert a test file for the current package."
  ("package " (go-ext-project-package) \n
   \n "import \"testing\"" \n))


;; ### Keymaps

(eval-when-compile
  (defvar user-ext-go-skeleton-map)
  (defvar user-ext-go-hide-map)
  (declare-function elisp-ext-yank-and-indent "elisp-ext"))

(define-prefix-command 'user-ext-go-skeleton-map)
(define-key go-mode-map (kbd "C-c C-s") #'user-ext-go-skeleton-map)
(define-key user-ext-go-skeleton-map (kbd "f") #'go-ext-skeleton-function)
(define-key user-ext-go-skeleton-map (kbd "m") #'go-ext-skeleton-method)
(define-key user-ext-go-skeleton-map (kbd "s") #'go-ext-skeleton-struct)
(define-key user-ext-go-skeleton-map (kbd "M-s") #'go-ext-skeleton-anonymous-struct)
(define-key user-ext-go-skeleton-map (kbd "i") #'go-ext-skeleton-interface)
(define-key user-ext-go-skeleton-map (kbd "l") #'go-ext-skeleton-lambda)

(define-prefix-command 'user-ext-go-hide-map)
(define-key go-mode-map (kbd "C-c f") #'user-ext-go-hide-map)
(define-key user-ext-go-hide-map (kbd "b") #'go-ext-hide-block)
(define-key user-ext-go-hide-map (kbd "s") #'go-ext-hide-struct)
(define-key user-ext-go-hide-map (kbd "C-s") #'go-ext-hide-all-structs)
(define-key user-ext-go-hide-map (kbd "C-i") #'go-ext-hide-all-interfaces)
(define-key user-ext-go-hide-map (kbd "f") #'go-ext-hide-function)
(define-key user-ext-go-hide-map (kbd "C-f") #'go-ext-hide-all-functions)
(define-key user-ext-go-hide-map (kbd "m") #'go-ext-hide-method)
(define-key user-ext-go-hide-map (kbd "@") #'hs-show-block)

(define-key go-mode-map (kbd "C-c C-f s") #'go-ext-goto-struct)
(define-key go-mode-map [remap go-goto-function] #'go-ext-goto-function-or-method)
(define-key go-mode-map [remap go-goto-function-name] #'go-ext-goto-function-or-method)
(define-key go-mode-map (kbd "C-c C-f p") #'go-ext-goto-package)
(define-key go-mode-map (kbd "C-c C-u") #'go-ext-goto-block)

(define-key go-mode-map (kbd "C-x p r") #'go-ext-project-rename-buffer)

(define-key go-mode-map (kbd "M-SPC") #'company-capf)


;; --- Menu

(easy-menu-define user-ext-go-menu-map
  go-mode-map
  "Go Extension"
  '("Go Extension"
    ["Go Tree Sitter" go-ext-tree-sitter-mode
     :style toggle :selected go-ext-tree-sitter-mode]
    "---"
    ("Hide/Show"
     :active go-ext-tree-sitter-mode
     ["Hide Function" go-ext-hide-function]
     ["Hide All Functions" go-ext-hide-all-functions]
     ["Hide Struct" go-ext-hide-struct]
     ["Hide All Structs" go-ext-hide-all-structs])
    ("Up Movement"
     :active go-ext-tree-sitter-mode
     ["Struct" go-ext-goto-struct]
     ["Function " go-ext-menu--goto-function
      :keys "\\[go-goto-function]"]
     ["Function Name" go-ext-menu--goto-function-name
      :keys "C-c C-f n"]
     ["Package Declaration" go-ext-goto-package]
     ["Block" go-ext-goto-block])
    ("Project"
     ["Find File" project-find-file]
     ["Rename Buffer" go-ext-project-rename-buffer])
    ("Skeletons"
     ["Struct" go-ext-skeleton-struct]
     ["Anonymouse Struct" go-ext-skeleton-anonymous-struct]
     ["Interface" go-ext-skeleton-interface]
     ["Function" go-ext-skeleton-function]
     ["Test Function" go-ext-skeleton-test-function]
     ["Anonymous Function" go-ext-skeleton-lambda]
     ["Method" go-ext-skeleton-method
      :active go-ext-tree-sitter-mode])
    "---"
    ["Edit Doc Comment" go-ext-docedit]
    "---"
    ["Godoc" go-ext-godoc]
    ("Godoc Server"
     ["Start Godoc Server" go-ext-godoc-server
      :active (not (go-ext-godoc-server--running))]
     ["Stop Active Godoc Server" go-ext-godoc-server-stop
      :active (go-ext-godoc-server--running)])))

(defun go-ext-menu--goto-function ()
  (interactive)
  (let ((last-command-event ?f))
    (go-ext-goto-function-or-method)))

(defun go-ext-menu--goto-function-name ()
  (interactive)
  (let ((last-command-event ?n))
    (go-ext-goto-function-or-method)))


;; ### Mode Hook

;;;###autoload
(defun go--extra-hook ()
  (setq-local indent-tabs-mode user-ext-go-indent-tabs
	      tab-width user-ext-go-tab-width
	      tempo-interactive t
	      tempo-user-elements (cons #'tempo-ext-tempo-handler
					tempo-user-elements))
  (add-hook 'before-save-hook #'gofmt-before-save nil t)
  (go-ext-tree-sitter-mode 1)
  (tree-sitter-hl-mode 1))

;;;###autoload
(add-hook 'go-mode-hook #'go--extra-hook)

(provide 'go-ext)
;;; go-ext.el ends here

;; Local Variables:
;; eval: (abbrev-ext-install-local-abbrev-functions)
;; eval: (abbrev-ext-define-local-abbrev "ux" "user-ext-go")
;; eval: (abbrev-ext-define-local-abbrev "uxg" "user-ext-go-godoc")
;; eval: (abbrev-ext-define-local-abbrev "gx" "go-ext")
;; eval: (abbrev-ext-define-local-abbrev "gxp" "go-ext-project")
;; eval: (abbrev-ext-define-local-abbrev "gxg" "go-ext-godoc")
;; eval: (abbrev-ext-define-local-abbrev "gxgp" "go-ext-go-process")
;; eval: (abbrev-ext-define-local-abbrev "gts" "go-ext-tree-sitter")
;; eval: (abbrev-ext-define-local-abbrev "tse" "tree-sitter-ext")
;; eval: (local-lambda-define-local-defun eval-test-hide-all-x nil "Test: eval `go-ext-define-hide-all-x-function'. In order to work, the following setup is required: - At least two tabs: left one in go-ext.el, split in two windows, one with the point to where it can evaluate a defining form, the other with the point where it can evaluate my \"prettyprint function def in other window\" form." (interactive) (let ((tl (syntax-ppss-toplevel-pos (make-ppss-easy (syntax-ppss)))) ow) (when tl (save-excursion (goto-char tl) (elisp-ext-forward-or-backward-sexp) (call-interactively #'eval-last-sexp)) (tab-previous) (and (y-or-n-p "Other window? ") (setq ow t) (call-interactively #'other-window)) (call-interactively #'eval-last-sexp) (call-interactively #'other-window) (call-interactively #'eval-last-sexp))))
;; End:

;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-lib)
  (require 'function-ext))

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
		       (tsc-node-type child)
		       fn-by-type
		       (lambda (node)
			 (run-with-timer
			  1
			  nil
			  #'message "Unsupported node type %S" (tsc-node-type child))))
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


;; ### Tree Sitter

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

;; ### Functions

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

(defun go-ext-delete-dwim ()
  "Call the deletion function you want (Do What I Mean)."
  (interactive)
  ;; INFO: Under construction
  (let ((node (tree-sitter-node-at-pos :named)))
    ))


(cl-pushnew 'syntax user-ext-go-subextensions)
;;; syntax.el ends here

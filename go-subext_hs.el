;; -*- lexical-binding: t; -*-

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
    (hs-ext-hide-range beg end 'code)
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
			    (hs-ext-hide-range (1+ beg) (1- end) 'code nil nil nil t)
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


(cl-pushnew 'hs user-ext-go-subextensions)
;;; hs.el ends here

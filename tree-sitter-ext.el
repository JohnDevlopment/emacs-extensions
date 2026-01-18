;; -*- lexical-binding: t; -*-

(check-emacs-minimum-version "27.4")
(extension-check-requires tree-sitter tree-sitter-query)

(require 'tree-sitter)
(require 'tree-sitter-query)
(require 'button)
(require 'debug-ext)

(embed-doc-document-symbol tree-sitter-ext
  "Tree Sitter extension."
  :functions
  tree-sitter-ext-assert-node-type
  tree-sitter-ext-assert-valid-state
  tree-sitter-ext-get-first-child
  tree-sitter-ext-get-first-named-child
  tree-sitter-ext-goto
  tree-sitter-ext-region-from-node
  tree-sitter-ext-set-menu
  tree-sitter-ext-set-mode
  tree-sitter-ext-type-p
  tree-sitter-ext-unset-menu
  tree-sitter-ext-with-mode-enabled
  tree-sitter-ext-with-region
  tree-sitter-ext-find-sibling
  tree-sitter-ext-find-ancestor
  :commands
  tree-sitter-ext-debug-buffer
  tree-sitter-ext-debug-node-at-position
  tree-sitter-ext-debug-reparse-buffer
  tree-sitter-query-quit)

(cl-deftype tsc-node () '(satisfies tsc-node-p))


;; ### Variables

(defvar-local user-ext-tree-sitter--optimize 1)

(defvar user-ext-tree-sitter--mode nil)

(defvar user-ext-tree-sitter-menu
  (easy-menu-create-menu
   "Tree Sitter"
   '(["Debug Buffer" tree-sitter-ext-debug-buffer]
     ["Node At Position" tree-sitter-ext-debug-node-at-position]))
  "Menu keymap general tree sitter commands.")


;; ### Functions

(defmacro tree-sitter-ext-set-mode (mode)
  "Set the Tree Sitter minor mode current being used to MODE.
MODE is a symbol."
  (cl-check-type mode symbol)
  (prog1 nil
    (setq user-ext-tree-sitter--mode mode)))

(defmacro tree-sitter-ext-assert-valid-state (&optional mode)
  "Verify that it's safe to use Tree Sitter functions.
Check if two conditions are met, namely: whether the
appropriate minor mode is enabled, and whether
`tree-sitter-tree' is non-nil.
Unless MODE is specified, check if the value of
`user-ext-tree-sitter--mode' (a minor mode symbol) is bound
and non-nil.
If MODE is provided, it specifies the minor mode that must
be enabled.

See also: `tree-sitter-ext-set-mode'."
  (cl-check-type mode (or symbol null))
  (let ((mode (or mode user-ext-tree-sitter--mode)))
    (or mode (error "MODE and `user-ext-tree-sitter--mode' are nil"))
    `(progn
       (or (bound-and-true-p ,mode)
	   (user-error ,(format "`%S' must be enabled" mode)))
       (or tree-sitter-tree
	   (error "`tree-sitter-tree' is nil")))))

(defmacro tree-sitter-ext-with-mode-enabled (&rest body)
  "Temporarily enable Tree Sitter mode and evaluate BODY."
  (declare (indent defun) (debug (body)))
  `(let ((was-enabled tree-sitter-mode))
     (unwind-protect
	 (progn
	   (unless was-enabled
	     (tree-sitter-mode 1))
	   ,@body)
       (unless was-enabled
	 (tree-sitter-mode 0)))))

(defsubst tree-sitter-ext-debug-buffer--buffer-name (&optional buffer)
  (format "*tree sitter debug: %s*" (buffer-name buffer)))

;;;###autoload
(defun tree-sitter-ext-debug-buffer ()
  "Display the buffer's syntax tree in a buffer.
Display a buffer that contains the current buffer's abstract
syntax tree.

Each line represents a node in the syntax tree:
   TYPE (extra: EXTRA) (range: [START,END])

TYPE is the node type.  EXTRA is a flag indicating whether
the node is neccessary to the grammar.  START and END
represent the region of the node in the source buffer.

Each line is indented according to its place in the tree: if
node B is a child of node A, then it is indented by one
level accordingly.
   source_file (extra: nil) (range: [1,2565])
     comment (extra: t) (range: [1,49])

The TYPE on each line is made into a button that, when
activated, moves point in the source buffer to the starting
position of that node."
  (interactive)
  (let ((src-buffer (current-buffer))
	strings buffer)
    (setq strings (tree-sitter-ext-debug-buffer--setup)
	  buffer (get-buffer-create
		  (tree-sitter-ext-debug-buffer--buffer-name)))
    (with-current-buffer buffer
      (tsx-debug-mode)
      (goto-char (point-min))
      (save-excursion
	(setq tsx-debug-source-buffer src-buffer)
	(let ((inhibit-read-only t))
	  (delete-region (point-min) (point-max))
	  (princ strings (current-buffer))))
      (set-buffer-modified-p nil))
    (run-with-idle-timer 0.01 nil #'tsx-debug--buttonize-text buffer)
    (display-buffer buffer t)))

(defun tree-sitter-ext-debug-buffer--setup (&optional buffer)
  "Return a string containing BUFFER's syntax tree.
If BUFFER is nil, default to the current buffer."
  (let (strings root)
    (save-current-buffer
      (and buffer
	   (set-buffer buffer))
      (tree-sitter-ext-with-mode-enabled
	(cl-assert tree-sitter-tree)
	(setq root (tsc-root-node tree-sitter-tree))
	(tsc-traverse-do ([type field depth named-p extra-p start-byte end-byte] root)
	  (when named-p
	    (if field
		(push (format "%s%s %S (extra: %S) (range: [%d,%d])"
			      (make-string (* depth 2) 32)
			      field type extra-p
			      (byte-to-position start-byte)
			      (byte-to-position end-byte))
		      strings)
	      (push (format "%s%S (extra: %S) (range: [%d,%d])"
			    (make-string (* depth 2) ?\ )
			    type extra-p
			    (byte-to-position start-byte)
			    (byte-to-position end-byte))
		    strings))))))
    (s-join "\n" (nreverse strings))))

;;;###autoload
(defun tree-sitter-ext-debug-node-at-position (&optional pos named)
  "Print the node at position POS.
If POS is nil, it defaults to point.
If NAMED is non-nil, get the named named at POS, otherwise
get the named or anonymous node at POS.

If the current buffer has a debug buffer spawned from it
(via `tree-sitter-ext-debug-buffer'), then its point is
moved to the matched node.

When called interactively, POS is point and NAMED is the
inverse of the prefix argument (i.e., with no prefix arg,
NAMED is non-nil)."
  (interactive (list (point) (let ((it current-prefix-arg))
			       (if it nil '(4)))))
  (tree-sitter-ext-with-mode-enabled
    (cl-assert tree-sitter-tree)
    (when-let ((p (or pos (point)))
	       (node (if named (tree-sitter-node-at-pos :named)
		       (tree-sitter-node-at-pos)))
	       (type (tsc-node-type node))
	       (beg (tsc-node-start-position node))
	       (end (tsc-node-end-position node)))
      (when-let ((buffer (get-buffer
			  (tree-sitter-ext-debug-buffer--buffer-name)))
		 (stype (cl-ext-progn
			  (cl-check-type type (or string symbol))
			  (cl-typecase type
			    (symbol (symbol-name type))
			    (string type))))
		 (regex (rx-let ((symbol (or (syntax word)
					     (syntax symbol)))
				 (s-space (syntax whitespace)))
			  (rx bol (* s-space)
			      (opt (seq ?: (+ symbol)))
			      (* s-space)
			      (group (literal stype))
			      (+ nonl) "(range: [" (literal
						    (number-to-string beg))))))
	(when named
	  (with-demoted-errors "Error `tree-sitter-ext-debug-node-at-position': %S"
	    (pop-to-buffer buffer)
	    (goto-char (point-min))
	    (widen)
	    (re-search-forward regex)
	    (goto-char (match-beginning 1)))))
      (princ (format-message
	      "%S (extra: %S) [%d,%d]"
	      (tsc-node-type node) (tsc-node-extra-p node) beg end)))))

(defun tree-sitter-ext-debug-reparse-buffer ()
  "Reparse the buffer."
  (interactive)
  (tree-sitter--do-parse))

(defun tree-sitter-ext-node-at-position
    (node-type-or-types &optional pos ignore-invalid-type)
  "Return the smallest syntax node of type TYPE at POS.
This acts largely the same as `tree-sitter-node-at-pos' but
with one key difference.  TYPE can be a list of symbols and
strings, in which case the node whose type matches one of
them is returned.

\(fn TYPE &optional POS IGNORE-INVALID-TYPE)"
  (when-let ((node (tree-sitter-node-at-pos
		    (unless (listp node-type-or-types) node-type-or-types)
		    pos
		    ignore-invalid-type)))
    (cl-etypecase node-type-or-types
      (null node)
      (symbol node)
      (list
       (cl-loop with this = node
		with types = node-type-or-types
		with result
		while this
		do
		(if (and this
			 (-any (##equal (tsc-node-type this) %1) types)
			 (not (= (point) (tsc-node-start-position this))))
		    (setq result this
			  this nil)
		  (setq this (tsc-get-parent this)))
		finally return result)))))

;;;###autoload
(defsubst tree-sitter-ext-region-from-node (node)
  "Return the region of NODE.
Return a cons cell of the form (START . END), with each
element being a buffer position."
  (cons (tsc-node-start-position node)
	(tsc-node-end-position node)))

;;;###autoload
(defmacro tree-sitter-ext-get-first-child (node)
  "Return NODE's first child."
  (declare (debug t))
  `(tsc-get-nth-child ,node 0))

;;;###autoload
(defmacro tree-sitter-ext-type-p (node type)
  "Return non-nil if NODE's type matches TYPE."
  `(equal (tsc-node-type ,node) ,type))

;;;###autoload
(defalias 'tsc-get-first-child #'tree-sitter-ext-get-first-child)

;;;###autoload
(defmacro tree-sitter-ext-get-first-named-child (node)
  "Return NODE's first named child."
  (declare (debug t))
  `(tsc-get-nth-named-child ,node 0))

;;;###autoload
(defalias 'tsc-get-first-named-child #'tree-sitter-ext-get-first-named-child)

(function-put 'tsc-node-start-position 'byte-optimizer
	      'byte-compile-inline-expand)
(function-put 'tsc-node-end-position 'byte-optimizer
	      'byte-compile-inline-expand)

;;;###autoload
(defmacro tree-sitter-ext-with-region (node binds &rest body)
  "Bind variables to the region of NODE and evaluate BODY.
Evaluate BODY with `region' bound to the region of NODE, and
`beg' and `end' bound to the car and cdr of `region',
respectively.
If BINDS is non-nil, set extra bindings for BODY.  BINDS is
the same format as for `let*'."
  (declare (indent 2)
	   (debug (form (&rest [&or symbolp (symbolp form)])
			&rest form)))
  `(when-let ((region (tree-sitter-ext-region-from-node ,node))
	      (beg (car region))
	      (end (cdr region)))
     ,(if binds
	  `(let* (,@binds)
	     ,@body)
	`(progn
	   ,@body))))

(define-error 'invalid-tsc-node-type "Invalid node type")
(defmacro tree-sitter-ext-assert-node-type (node type &rest types)
  "Assert that NODE is of a given type.
If NODE's type is not one of the TYPEs provided, signal
`invalid-tsc-node-type' error.

This does nothing if `user-ext-tree-sitter--optimize' is
greater than 1.

\(fn NODE TYPE ...)"
  (unless (> user-ext-tree-sitter--optimize 1)
    (if types
	`(unless (cl-member (tsc-node-type ,node)
			    '(,type ,@types)
			    :test #'equal)
	   (signal 'invalid-tsc-node-type
		   (list "expected one of"
			 '(,type ,@types)
			 "got"
			 (tsc-node-type ,node))))
      `(unless (equal (tsc-node-type ,node) ,(macroexp-quote type))
	 (signal 'invalid-tsc-node-type
		 (list "expected" ,(macroexp-quote type)
		       "got" (tsc-node-type ,node)))))))


;; --- Menu

;;;###autoload
(defun tree-sitter-ext-set-menu (keymap)
  ;; TODO: documentation string
  (let ((menu-name (nth 1 user-ext-tree-sitter-menu)))
    (define-key-after keymap
      (vector 'menu-bar (easy-menu-intern menu-name))
      (easy-menu-binding user-ext-tree-sitter-menu menu-name))))

;;;###autoload
(defun tree-sitter-ext-unset-menu (keymap)
  ;; TODO: documentation string
  (let ((menu-name (nth 1 user-ext-tree-sitter-menu)))
    (define-key-after keymap
      (vector 'menu-bar (easy-menu-intern menu-name))
      nil)))


;; --- Search

(defsubst tree-sitter-ext--check-node-and-type (node type)
  (cl-check-type type (and (not keyword) (or symbol string list)))
  (cl-check-type node tsc-node))
(--ignore
  (--symbol-plist 'seconds-type)

  (cl-prettyexpand '(cl-deftype tsc-node () '(satisfies tsc-node-p)))

  (--symbol-plist 'integer)

  (cl-prettyexpand '(cl-deftype unsigned-byte (&optional bits)
		      (list 'integer 0 (if (eq bits '*) bits (1- (ash 1 bits))))))
(progn
  (define-symbol-prop 'unsigned-byte
		      'cl-deftype-handler
		      #'(lambda (&rest --cl-rest--) "

(fn &optional BITS)" (let* ((bits (if --cl-rest--
				      (car-safe
				       (prog1 --cl-rest--
					 (setq --cl-rest-- (cdr --cl-rest--))))
				    '*)))
		       (progn
			 (if --cl-rest--
			     (signal 'wrong-number-of-arguments (list nil (+ 1 (length --cl-rest--)))))
			 (list 'integer 0 (if (eq bits '*) bits (1- (ash 1 bits)))))))))

  (prog1 nil
    (with-current-buffer (get-buffer-create "*output*")
      (emacs-lisp-mode)
      (cl-prettyprint (symbol-function #'tree-sitter-ext--check-node-and-type))
      (run-with-idle-timer 0.2 nil #'activate-view-mode 1)
      (set-buffer-modified-p nil))
    (pop-to-buffer "*output*" t)
    (call-interactively #'menu-bar--toggle-truncate-long-lines))
  t)

(defun tree-sitter-ext--node-type-p (node type)
  (declare (side-effect-free t))
  (tree-sitter-ext--check-node-and-type node type)
  (pcase type
    ((or (pred stringp) (pred symbolp))
     (equal (tsc-node-type node) type))))
(--ignore
 (prog1 nil
   (with-current-buffer (get-buffer-create "*output*")
     (emacs-lisp-mode)
     (cl-prettyprint (symbol-function 'tree-sitter-ext--node-type-p))
     (run-with-idle-timer 0.2 nil #'activate-view-mode 1)
     (set-buffer-modified-p nil))
   (pop-to-buffer "*output*" t))
 t)

(defun tree-sitter-ext--find-node (node type func)
  (declare (side-effect-free t))
  (cl-check-type func function)
  (tree-sitter-ext--check-node-and-type node type)
  (cl-loop with done
	   until done
	   do
	   (setq node (funcall func node)
		 done (or (null node)
			  (and node
			       (tree-sitter-ext--node-type-p node type))))
	   finally return node))

(defun tree-sitter-ext-find-sibling (type node &optional named)
  (declare (side-effect-free t))
  (tree-sitter-ext--check-node-and-type node type)
  (tree-sitter-ext-assert-valid-state tree-sitter-mode)
  (tree-sitter-ext--find-node node type
			      (if named #'tsc-get-next-named-sibling
				#'tsc-get-next-sibling)))

(defun tree-sitter-ext-find-ancestor (type node)
  "Return the ancestor of NODE that has the given TYPE.
If no such node is found, return nil."
  (declare (side-effect-free t))
  (tree-sitter-ext--check-node-and-type node type)
  (tree-sitter-ext-assert-valid-state tree-sitter-mode)
  (tree-sitter-ext--find-node node type #'tsc-get-parent))

(defmacro tree-sitter-ext-if-match-query (node pattern cursor &rest body)
  "If NODE matches PATTERN, do BODY, else return nil.
PATTERN is an S-expression used to match a node; it is the
same format as the same-name argument in `tsc-make-query'
except it is not a vector of S-expressions.
In BODY, `tag' is bound to the tag of the match and `node'
is bound to the captured node."
  (declare (indent 3)
	   (debug (form sexp form body)))
  `(when-let ((query (tsc-make-query tree-sitter-language [,pattern]))
	      (matches
	       (tsc-query-captures
		query ,node #'tsc--buffer-substring-no-properties))
	      (tag (car matches))
	      (node (cdr matches)))
     ,@body))
(--ignore
 (prog1 nil
   (with-current-buffer (get-buffer-create "*output*")
     (emacs-lisp-mode)
     (cl-prettyexpand
      '(tree-sitter-ext-if-match-query
	   node
	   (parameter_declaration
	    type: (pointer_type) @p)
	   qc
	 1
	 t))
     (run-with-idle-timer 0.2 nil #'activate-view-mode 1)
     (set-buffer-modified-p nil))
   (pop-to-buffer "*output*" t))
 t)

(defmacro tree-sitter-ext-if-match-queries (node patterns &rest body)
  "If NODE matches one of PATTERNS, do BODY, else return nil.
PATTERNS is a vector S-expressionS used to match a node; it
is the same format as the same-name argument in `tsc-make-query'
In BODY, `matches' is bound to the vector of matches."
  (declare (indent 3)
	   (debug (form [&rest sexp] form body)))
  (cl-check-type patterns vector)
  `(when-let ((query (tsc-make-query tree-sitter-language ,patterns))
	      (matches
	       (tsc-query-captures
		query ,node #'tsc--buffer-substring-no-properties)))
     ,@body))


;; --- Cursor

(defmacro tree-sitter-ext-goto (rel cursor &rest r)
  "Move CURSOR to the node of relationship REL to current node.
Call (tsc-goto-REL ARGS...) and report an error if the
result is nil.

REL can be one of `parent', `first-child', or `next-sibling',
which return t on a successful move, or `first-child-for-position',
which returns the index of the child on a successful move.

\(fn REL CURSOR ARG...)"
  (cl-check-type rel symbol)
  (cl-macrolet ((rel
		 ()
		 (macroexp-quote
		  '(parent first-child
			   first-child-for-position
			   next-sibling))))
    (or (memq rel (rel))
	(signal-wrong-argument rel (rel)))
    (let ((fn (intern-soft
	       (format "tsc-goto-%S" rel))))
      (cl-assert fn)
      `(let ((node (,fn ,cursor ,@r)))
	 (or node
	     (error ,(format "Failed to reach %S"
			     rel)))))))


;; ### Debug Buffer Mode

(defun tsx-debug--jump-to-source (button)
  (cl-assert tsx-debug-source-buffer)
  (let ((p (button-get button 'point)))
    (cl-assert p)
    (pop-to-buffer tsx-debug-source-buffer)
    (goto-char p)
    (message "Jumped to position %s" p)))

(defun tsx-debug--buttonize-text (buffer)
  (with-current-buffer buffer
    (with-silent-modifications
      (save-excursion
	(cl-loop with inhibit-read-only = t
		 with regex = (rx-let ((symbol (or (syntax word)
						   (syntax symbol)))
				       (-space (syntax whitespace)))
				(rx bol (* -space)
				    (opt (seq ?: (+ symbol)))
				    (* -space)
				    (group (+ symbol)) ; G1: node type
				    (* nonl) "(range: ["
				    (group (+ digit)) ; G2: start
				    ?, (group (+ digit)) ; G3: end
				    "])" eol))
		 initially do
		 (goto-char (point-min))
		 while (re-search-forward regex nil t)
		 do
		 (make-button (match-beginning 1) (match-end 1)
			      :type 'tsx-debug-button
			      'point (string-to-number
				      (match-string-no-properties 2))))))))

(define-button-type 'tsx-debug-button 'action #'tsx-debug--jump-to-source)

(defconst tsx-debug-mode-map
  (let ((map (make-composed-keymap nil special-mode-map)))
    (define-key map (kbd "k") #'kill-and-quit)
    map))

(defvar-local tsx-debug-source-buffer nil
  "The source buffer this buffer takes from.")

(defun tsx-debug--mode-revert-buffer (&optional ignore-auto noconfirm)
  (cl-assert tsx-debug-source-buffer)
  (ignore ignore-auto noconfirm)
  (let ((strings (tree-sitter-ext-debug-buffer--setup
		  tsx-debug-source-buffer))
	(inhibit-read-only t))
    (save-excursion
      (delete-region (point-min) (point-max))
      (princ strings (current-buffer)))
    (set-buffer-modified-p nil)
    (run-with-idle-timer 0.1 nil #'tsx-debug--buttonize-text (current-buffer))))

(define-derived-mode tsx-debug-mode special-mode
  "TS Debug"
  "Major mode for displaying a buffer's source tree."
  (setq-local revert-buffer-function #'tsx-debug--mode-revert-buffer)
  (visual-line-mode 0)
  (setq word-wrap nil)
  (toggle-truncate-lines 1))


;; ### Tree Sitter Query Builder

;;;###autoload
(defun tree-sitter-query--extra-hook ()
  (setq-local indent-line-function #'lisp-indent-line))

(defun tree-sitter-query-quit (&optional arg)
  "Kill the buffer text and then the buffer itself.
ARG is passed directly to `kill-and-quit'.

When called interactively, ARG is the prefix argument."
  (interactive "P")
  (kill-region (point-min) (point-max))
  (kill-and-quit arg))

(define-key tree-sitter-query-mode-map (kbd "C-c C-c") #'tree-sitter-query-quit)

(provide 'tree-sitter-ext)
;;; tree-sitter-ext.el ends here

;; Local Variables:
;; eval: (abbrev-ext-install-local-abbrev-functions)
;; eval: (abbrev-ext-define-local-abbrev "ts" "tree-sitter")
;; eval: (abbrev-ext-define-local-abbrev "tse" "tree-sitter-ext")
;; eval: (abbrev-ext-define-local-abbrev "ux" "user-ext-tree-sitter")
;; End:

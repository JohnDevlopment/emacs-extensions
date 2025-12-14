;; -*- lexical-binding: t; -*-

;; (require 'python-mode)
(require 'python)
(require 'hideshow)
(require 'tree-sitter)
(require 'tree-sitter-indent)

(eval-when-compile
  (require 'function-ext))

(load-extension "tree-sitter-ext")

(tree-sitter-ext-set-mode python-ext-tree-sitter-mode)


;; ### Variables

(defconst user-ext-python-syntax-block-nodes
  '(if_statement
    elif_clause else_clause
    for_statement while_statement with_statement
    match_statement class_definition function_definition)
  "A list of syntax nodes representing blocks.")

(defconst user-ext-python-syntax-decorated-nodes
  '(class_definition function_definition)
  "A list of syntax nodes representing blocks that can be decorated.
The decorator is represented by the `decorated_definition'
node.")


;; ### Customization/Variables

(defcustom user-ext-python-tree-sitter-indent-scopes
  '((indent-all . ())
    (indent-rest . (function_definition for_statement))
    (indent-body . (assignment))
    (paren-indent . (argument_list))
    (align-char-to . ((?. . (call field_expression))))
    (aligned-siblings . ())
    (multi-line-text . ())
    (outdent . ()))
  "List of scopes controlling indentation in Python.

Each element has the form (SCOPE . NODES).
SCOPE is a symbol--supported scopes are listed below.
NODES is either a list of symbols denoting nodes to be
indented, or (in the case of `align-char-to'), an alist
mapping characters to lists of symbols.

For every SCOPE except `align-char-to', NODES has the form
(NODE ...).  SCOPE is one of the following:
- `indent-all' - Every NODE is indented.
- `indent-rest' - The children of every NODE is indented
except for its first child.
- `indent-body' - For every NODE, the children in the middle
of it is indented.
- `paren-indent' - For every NODE, its children are indented
to the opening parenthesis.
- `align-char-to' - NODES is an alist where each element
takes the form (CHAR . (NODE)). [Under construction]
- `aligned-siblings' - Every NODE's first child is aligned
with by its siblings.
- `multi-line-text' - Every NODE is exempt from indentation.
- `outdent' - Every NODE outdents (shifts in opposite
direction)."
  :type '(list
	  (cons (const :tag "Indent All" indent-all)
		(repeat (choice (symbol :tag "Named Node")
				(string :tag "Anonymous Node"))))
	  (cons (const :tag "Indent Rest" indent-rest)
		(repeat (choice (symbol :tag "Named Node")
				(string :tag "Anonymous Node"))))
	  (cons (const :tag "Indent Body" indent-body)
		(repeat (choice (symbol :tag "Named Node")
				(string :tag "Anonymous Node"))))
	  (cons (const :tag "Paren Indent" paren-indent)
		(repeat (choice (symbol :tag "Named Node")
				(string :tag "Anonymous Node"))))
	  (cons (const :tag "Align Char To" align-char-to)
		(alist :key-type character
		       :value-type (repeat symbol)))
	  (cons (const :tag "Aligned Siblings" aligned-siblings)
		(repeat (choice (symbol :tag "Named Node")
				(string :tag "Anonymous Node"))))
	  (cons (const :tag "Multi Line Text" multi-line-text)
		(repeat (choice (symbol :tag "Named Node")
				(string :tag "Anonymous Node"))))
	  (cons (const :tag "Outdent" outdent)
		(repeat (choice (symbol :tag "Named Node")
				(string :tag "Anonymous Node")))))
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (setq tree-sitter-indent-python-scopes value))
  :group 'python-ext)
(defvar tree-sitter-indent-python-scopes
  user-ext-python-tree-sitter-indent-scopes)
(--ignore
 (--destroy-variable 'tree-sitter-indent-python-scopes)
 t)


;; ### Functions

(cl-defmacro python-ext-define-motion-commands
    (form node-fn &key dec assert-type)
  "Define motion commands for FORM.
NODE-FN is used to get the node at point.

The keyword :dec, if non-nil, means that FORM is a decorated
form (i.e., a form which supports decorators).
If the keyword :assert-type is provided, assertions are
added to check the type of nodes.  TYPE can be a symbol or a
list of symbols.

The functions defined are:
- python-ext-backward-FORM
- python-ext-backward-FORM-2
- python-ext-forward-FORM
- python-ext--beginning-of-FORM-p
- python-ext--beginning-of-FORM-p-2
- python-ext-mark-FORM

\(fn FORM NODE-FN [:dec BOOLEAN] [:assert-type TYPE])"
  (declare (indent defun)
	   (debug (&define symbol form
			   [&optional ":dec" boolean]
			   [&optional ":assert-type"
				      [&or symbolp
					   (&rest symbolp)]])))
  (let ((backward-fun (intern (format "python-ext-backward-%S" form)))
	(backward2-fun (intern (format "python-ext-backward-%S-2" form)))
	(forward-fun (intern (format "python-ext-forward-%S" form)))
	(beg-fun (intern (format "python-ext--beginning-of-%S-p" form)))
	(beg2-fun (intern (format "python-ext--beginning-of-%S-p-2" form)))
	(mark-fun (intern (format "python-ext-mark-%S" form))))
    `(progn
       (defun ,backward-fun (&optional orig bol ret-node)
	 ,(s-lex-format "Go to beginning of ${form}.
Return position or node if successful, nil otherwise.
If ORIG is non-nil, use it as starting position; it defaults
to point.
If BOL is non-nil, move point to beginning of line of
beginning of ${form}.  Then return that position.
If RET-NODE is non-nil, return the node instead of position.")
	 (interactive)
	 (when-let ((node (,node-fn orig)))
	   ;; ,(when dec
	   ;;    `(when (and (not py-mark-decorators)
	   ;; 		  (tree-sitter-ext-type-p node 'decorated_definition))
	   ;; 	 (setq node (tsc-get-child-by-field node :definition))))
	   ,(when assert-type
	      (cl-typecase assert-type
		(list
		 `(cl-assert (member (tsc-node-type node) ',assert-type) t))
		(symbol
		 `(cl-assert (equal (tsc-node-type node) (macroexp-quote ,assert-type)) t))
		(otherwise
		 (error "Invalid :assert-type %S, must be a symbol or list" assert-type))))
	   (goto-char (tsc-node-start-position node))
	   (and bol (goto-char (line-beginning-position)))
	   (if ret-node node (point))))
       (defun ,backward2-fun ()
	 ,(s-lex-format "Go to the beginning of ${form} body.")
	 (let* (py-mark-decorators
		(node (,backward-fun nil nil t))
		(body-node (and node (tsc-get-child-by-field node :body))))
	   (if body-node
	       (prog1 (goto-char (tsc-node-start-position body-node))
		 (skip-syntax-backward " >")))))
       (defun ,forward-fun (&optional orig bol)
	 ,(s-lex-format "Go to end of ${form}.
Return end of ${form} if successful, nil otherwise.
If ORIG is provided, it specifies the start position; it
defaults to point.
If BOL is non-nil, go to beginning of line following end-position.")
	 (interactive)
	 (when-let ((node (,node-fn orig)))
	   (goto-char (tsc-node-end-position node))
	   (and bol (goto-char (line-beginning-position)))
	   (point)))
       (defun ,beg-fun (&optional pos get-node)
	 ,(s-lex-format "If at the beginning of a ${form}, return its position.")
	 (when-let ((node (,node-fn pos))
		    (beg (tsc-node-start-position node)))
	   (and (<= (line-beginning-position) (point) beg)
		(if get-node node beg))))
       (defun ,beg2-fun (&optional pos)
	 (when-let ((node (,beg-fun pos t))
		    (body-node (tsc-get-child-by-field node :body)))
	   (goto-char (tsc-node-start-position body-node))
	   (skip-syntax-backward " >")))
       (defun ,mark-fun ()
	 ,(s-lex-format "Mark ${form}.")
	 (interactive)
	 (when-let ((beg (save-excursion (,backward-fun)))
		    (end (,forward-fun)))
	   (push-mark beg nil t)))
       ,@(let ((bf (intern-soft (format "py-backward-%S" form)))
	       (bf2 (intern (format "py-backward-%S-2" form)))
	       (bof (intern (format "py--beginning-of-%S-p" form)))
	       (bof2 (intern (format "py--beginning-of-%S-p-2" form)))
	       body)
	   (if bf
	       (cl-ext-progn
		 (push `(advice-add (function ,bf)
				    :override (function ,backward-fun))
		       body))
	     (lwarn 'python-ext :warning
		    "py-backward-%S is not defined" form))
	   (push `(defalias ',bf2 #',backward2-fun) body)
	   (push `(defalias ',bof #',beg-fun) body)
	   (push `(defalias ',bof2 #',beg2-fun) body)
	   (nreverse body)))))

(defmacro python-ext--form-at-pos (name type &optional form dec-p)
  (declare (indent 2)
	   (debug (&define stringp [&or symbolp stringp] &optional sexp form)))
  (cl-check-type name string)
  (let ((fname (intern (format "python-ext--%s-at-pos" name)))
	(form (or form
		  `(tree-sitter-node-at-pos ,type pos))))
    `(defun ,fname (&optional pos)
       ,(let ((is-dec (if dec-p (s-lex-format "
If ${name} is decorated and `py-mark-decorators' is non-nil,
mark the decorator along with the ${name}.")
			"")))
	  (s-lex-format "Return the ${name} at POS.${is-dec}
POS defaults to point."))
       (declare (side-effect-free t))
       (let ((pos (or pos (point))))
	 (save-excursion
	   (goto-char pos)
	   (when current-prefix-arg
	     (setq pos (+ pos (skip-syntax-backward " >"))))
	   (when (= pos (line-end-position))
	     (cl-decf pos)))
	 (let ((node ,form))
	   (if (and node
		    (member (tsc-node-type node)
			    user-ext-python-syntax-decorated-nodes)
		    py-mark-decorators)
	       (let ((parent (tsc-get-parent node)))
		 (when (tree-sitter-ext-type-p parent 'decorated_definition)
		   (setq node parent))))
	   node)))))

;; Block
;; (declare-function python-ext-backward-block "_python-ext-syntax")
;; (declare-function python-ext-backward-block-2 "_python-ext-syntax")
;; (declare-function python-ext-forward-block "_python-ext-syntax")
;; (declare-function python-ext--beginning-of-block-p "_python-ext-syntax")
;; (declare-function python-ext--beginning-of-block-p-2 "_python-ext-syntax")
;; (declare-function python-ext-mark-block "_python-ext-syntax")
(python-ext--form-at-pos "block" nil
  (tree-sitter-ext-node-at-position user-ext-python-syntax-block-nodes pos)
  t)
(python-ext-define-motion-commands block python-ext--block-at-pos
  :dec t)
(--ignore
 (prog1 nil
   (with-current-buffer (get-buffer-create "*output*")
     (emacs-lisp-mode)
     (cl-prettyprint
      (macroexpand-1 '(python-ext-define-motion-commands block python-ext--block-at-pos
			:dec t)))
     (run-with-idle-timer 0.2 nil #'activate-view-mode 1)
     (set-buffer-modified-p nil))
   (pop-to-buffer "*output*" t))
 t)

;; Class
;; (declare-function python-ext-backward-class "_python-ext-syntax")
;; (declare-function python-ext-backward-class-2 "_python-ext-syntax")
;; (declare-function python-ext-forward-class "_python-ext-syntax")
;; (declare-function python-ext--beginning-of-class-p "_python-ext-syntax")
;; (declare-function python-ext--beginning-of-class-p-2 "_python-ext-syntax")
;; (declare-function python-ext-mark-class "_python-ext-syntax")
(python-ext--form-at-pos "class" 'class_definition nil t)
(python-ext-define-motion-commands class python-ext--class-at-pos
  :assert-type (decorated_definition class_definition))
(advice-add #'py-backward-class :override #'python-ext-backward-class)
(defalias 'py-backward-class-2 #'python-ext-backward-class-2)
(defalias 'py--beginning-of-class-p #'python-ext--beginning-of-class-p)
(defalias 'py--beginning-of-class-p-2 #'python-ext--beginning-of-class-p-2)

;; Def
;; (declare-function python-ext-backward-def "_python-ext-syntax")
;; (declare-function python-ext-backward-def-2 "_python-ext-syntax")
;; (declare-function python-ext-forward-def "_python-ext-syntax")
;; (declare-function python-ext--beginning-of-def-p "_python-ext-syntax")
;; (declare-function python-ext--beginning-of-def-p-2 "_python-ext-syntax")
;; (declare-function python-ext-mark-def "_python-ext-syntax")
(python-ext--form-at-pos "def" 'function_definition nil t)
(python-ext-define-motion-commands def python-ext--def-at-pos
  :assert-type (decorated_definition function_definition))
(advice-add #'py-backward-def :override #'python-ext-backward-def)
(defalias 'py-backward-def-2 #'python-ext-backward-def-2)
(defalias 'py--beginning-of-def-p #'python-ext--beginning-of-def-p)
(defalias 'py--beginning-of-def-p-2 #'python-ext--beginning-of-def-p-2)


;; --- String

(cl-defstruct (python-ext--docstring
	       (:type list))
  (string-delim "" :type string
		:documentation "Characters used to delimit the string.")
  (start nil :type marker
	 :documentation "Beginning of the string, before delimiter.")
  (end nil :type 1marker
       :documentation "End of the string, after delimiter.")
  (content-start nil :type marker
		 :documentation "Position right after the starting delimiter.")
  (content-end nil :type marker
	       :documentation "Position of closing delimiter.")
  (ml nil :type boolean :documentation "Whether string is multiline."))

(defun python-ext--string-at-pos (&optional pos)
  (declare (side-effect-free t))
  (when-let ((node (tree-sitter-node-at-pos 'string pos)))
    (-let* (((sbeg . send) (tree-sitter-ext-region-from-node node))
	    ((cbeg . cend) (tree-sitter-ext-region-from-node
			    (tsc-get-nth-named-child node 1))))
      (make-python-ext--docstring
       :string-delim (tsc-node-text (tsc-get-first-named-child node))
       :start (copy-marker sbeg)
       :end (copy-marker send)
       :content-start (copy-marker cbeg)
       :content-end (copy-marker cend)))))


;; --- Python hs integration

(fext-defadvice py-hide-base (override py-hide-base (form &optional beg end))
  "Hide form at point."
  (hs-minor-mode 1)
  (save-excursion
    (cl-macrolet ((intern-format
		   (fmt &rest args)
		   `(intern-soft (format ,fmt ,@args))))
      (let* ((beg (or beg
		      (funcall-safe (intern-format "py--beginning-of-%S-p-2" form))
		      (funcall-safe (intern-format "py--beginning-of-%S-p" form))
		      (funcall-safe (intern-format "py-backward-%S-2" form))
		      (funcall-safe (intern-format "py-backward-%S" form))))
	     (end (or end
		      (funcall-safe (intern-format "py-forward-%S" form))))
	     (modified (buffer-modified-p))
	     (inhibit-read-only t))
	(if (and beg end)
	    (cl-ext-progn
	      (hs-ext-hide-range beg end 'code)
	      (set-buffer-modified-p modified))
	  (error "No %s at point" form))))))

(defun python-ext-show ()
  "Toggle visibility of existing forms at point."
  (interactive)
  (hs-minor-mode 1)
  (save-excursion
    (let* ((ov (hs-overlay-at (point)))
	   (beg (and ov (overlay-start ov)))
	   (end (and ov (overlay-end ov)))
	   (modified (buffer-modified-p))
           (inhibit-read-only t))
      (and ov beg end
	   (hs-discard-overlays beg end))
      (set-buffer-modified-p modified))))

(defun python-ext-hide-all-defs ()
  "Hide all def forms in buffer."
  (interactive)
  (hs-minor-mode 1)
  (tree-sitter-ext-assert-valid-state)
  (let ((pos (point)))
    (save-excursion
      (cl-loop with q = (tsc-make-query tree-sitter-language
					[(function_definition
					  body: (block) @block)])
	       with captures = (tsc-query-captures
				q
				(tsc-root-node tree-sitter-tree)
				#'tsc--buffer-substring-no-properties
				python-ext-tree-sitter--query-cursor)
	       with spew = (make-progress-reporter "Hiding all functions..."
						   0 (length captures))
	       with i = 0
	       with node
	       for cap across captures
	       do
	       (setq node (cdr cap))
	       (tree-sitter-ext-with-region node nil
		 (when (<= beg pos end)
		   (setq pos (save-excursion
			       (goto-char beg)
			       (goto-char (line-beginning-position)))))
		 (goto-char beg)
		 (skip-syntax-backward " >")
		 (or hs-allow-nesting
		     (hs-discard-overlays (point) end))
		 (hs-ext-hide-range (point) end 'code nil nil t)
		 (progress-reporter-update spew (cl-incf i)))
	       finally do
	       (progress-reporter-done spew)
	       (run-hooks 'hs-hide-hook)))
    (goto-char pos)))
(--ignore
 (prog1 nil
   (with-current-buffer (get-buffer-create "*output*")
     (emacs-lisp-mode)
     (cl-prettyprint (symbol-function #'python-ext-hide-all-defs))
     (run-with-idle-timer 0.2 nil #'activate-view-mode 1)
     (set-buffer-modified-p nil))
   (pop-to-buffer "*output*" t)
   (call-interactively #'menu-bar--toggle-truncate-long-lines))
 t)


;; --- Tree Sitter

(defvar-local python-ext-tree-sitter--query-cursor nil)

(defvar-local python-ext-tree-sitter--menu-added nil)

(defun python-ext-tree-sitter--setup ()
  (unless python-ext-tree-sitter--menu-added
    (tree-sitter-ext-set-menu python-mode-map)
    (setq python-ext-tree-sitter--menu-added t))
  (or python-ext-tree-sitter--query-cursor
      (setq python-ext-tree-sitter--query-cursor (tsc-make-query-cursor))))

(defun python-ext-tree-sitter--teardown ()
  (when python-ext-tree-sitter--menu-added
    (tree-sitter-ext-unset-menu python-mode-map)
    (kill-local-variable 'python-ext-tree-sitter--menu-added))
  (and python-ext-tree-sitter--query-cursor
       (kill-local-variable 'python-ext-tree-sitter--query-cursor)))

(define-minor-mode python-ext-tree-sitter-mode
  "Syntax parsing of Python using tree sitter."
  :group 'python-ext
  (when (and python-ext-tree-sitter-mode
	     (not (eq major-mode 'python-mode)))
    (run-with-idle-timer 0.1 nil #'python-ext-tree-sitter-mode 0)
    (user-error "`python-ext-tree-sitter-mode' only works in Python mode."))
  (tree-sitter--handle-dependent python-ext-tree-sitter-mode
    #'python-ext-tree-sitter--setup
    #'python-ext-tree-sitter--teardown))


;; ### Keymaps

;; Hide/show commands
(eval-and-compile
  (define-prefix-command 'user-ext-python-hide-show-map)
  (define-key python-mode-map (kbd "C-c f") #'user-ext-python-hide-show-map)

  (define-key user-ext-python-hide-show-map (kbd "d") #'py-hide-def)
  (define-key user-ext-python-hide-show-map (kbd "D") #'py-hide-def-or-class)
  (define-key user-ext-python-hide-show-map (kbd "c") #'py-hide-class)
  (define-key user-ext-python-hide-show-map (kbd "C-d") #'python-ext-hide-all-defs)
  (define-key user-ext-python-hide-show-map (kbd "s") #'python-ext-show))

;; Motion commands
(eval-and-compile
  (define-prefix-command 'user-ext-python-motion-map)
  (define-key python-mode-map (kbd "C-c C-f") #'user-ext-python-motion-map)

  (define-key user-ext-python-motion-map (kbd "d") #'python-ext-backward-def)
  (define-key user-ext-python-motion-map (kbd "D") #'python-ext-forward-def)
  (define-key user-ext-python-motion-map (kbd "c") #'python-ext-backward-class)
  (define-key user-ext-python-motion-map (kbd "C") #'python-ext-forward-class))


(provide '_python-ext-syntax)
;;; _python-ext-syntax.el ends here

;; Local Variables:
;; eval: (abbrev-ext-install-local-abbrev-functions)
;; eval: (abbrev-ext-define-local-abbrev "pts" "python-ext-tree-sitter")
;; eval: (abbrev-ext-define-local-abbrev "ux" "user-ext-python")
;; eval: (abbrev-ext-define-local-abbrev "px" "python-ext")
;; eval: (abbrev-ext-define-local-abbrev "tse" "tree-sitter-ext")
;; End:

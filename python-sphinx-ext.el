;;; python-sphinx-ext --- Python-Sphinx extension.  -*- lexical-binding: t;  -*-

;;; Commentary:

;;; Code:

(require 'sphinx-doc)

(eval-when-compile
  (require 'python-mode)
  (require 'debug-ext))

;; Customization

;; (defgroup sphinx-ext nil
;;   "Group for `sphinx-doc-mode' extension."
;;   :group 'user-extensions)

(defconst user-ext-sphinx-param-regexp
  (rx bol (* whitespace) ?:
      (or "keyword" "param")
      whitespace
      (+? (not whitespace))
      (? whitespace (group (+? (not whitespace))))
      ?: (+ whitespace) (not whitespace))
  "Regular expression for :param: and :keyword: roles.")

;; Functions

(defun sphinx-ext--style-map-prompt ()
  (concat
   (format ", i = %s"
	   (propertize "italics" 'face 'italic))
   (format ", c = %s"
	   (propertize "code" 'face 'font-lock-type-face))
   (format ", C = %s"
	   (propertize "code block" 'face 'font-lock-type-face))))

(defun sphinx-ext--directive-map-prompt ()
  "Prompt for `sphinx-ext-directive-map'."
  (concat
   "M = automodule, "
   "S = autosummary, "
   "a = attention, "
   "c = caution, "
   "d = danger, "
   "e = error, "
   "h = hint, "
   "n = note, "
   "t = tip, "
   "w = warning, "
   "s = seealso"))

(defun sphinx-ext--role-map-prompt ()
  (let ((type-face 'font-lock-type-face))
    (concat
     (format "r = %s" (propertize ":returns:" 'face type-face))
     (format ", R = %s" (propertize ":rtype:" 'face type-face))
     (format ", k = %s" (propertize ":keyword:" 'face type-face))
     (format ", p = %s" (propertize ":param:" 'face type-face))
     (format ", t = %s" (propertize ":type:" 'face type-face)))))

(defun sphinx-ext--insert-style (what)
  "Insert a style at point.
WHAT is a symbol naming what style to use:
* 'code: inline code
* 'italic : italics"
  (let ()
    (pcase what
      ('code
       (insert "````")
       (left-char 2))
      ('code-block
       (sphinx-ext-skeleton-code-block))
      ('italic
       (insert "``")
       (left-char)))))

(defun sphinx-ext--insert-directive (what &rest args)
  "Insert the directive WHAT."
  (let (spaces)
    (insert (format ".. %s::" what))
    (setq spaces (concat
		  (save-excursion
		    (back-to-indentation)
		    (buffer-substring (line-beginning-position) (point)))
		  "   "))
    (when (string= what "automodule")
      (insert " " (car args)))
    (newline)
    (insert spaces)))

(defmacro sphinx-ext-define-insert-directive-function (name &rest props)
  "Define an interactive function for inserting a Sphinx directive.
The function name is constructed as
sphinx-ext-insert-directive-NAME, where NAME is the provided
directive name.

NAME is the string name of the directive.  The rest of the
arguments shall have the form

   [KEYWORD VALUE]...

where the following keywords are meaningful:

:key	VALUE should be a string accepted by `kbd' to which
	the created function is mapped.
:args	VALUE shall be a list of the form
	   (INT-SPEC VARNAME ...)
	where INT-SPEC is an `interactive' string and
	VARNAME is the corresponding argument.
:doc	VALUE is the documentation string of the created
	function."
  (declare (indent 1))
  (let ((doc (plist-get props :doc))
	(function-name (intern
			(format "sphinx-ext-insert-directive-%s" name)))
	(key (plist-get props :key))
	(args (plist-get props :args))
	(int-prompt "")
	function-arglist)
    (unless doc
      (setq doc (format "Insert a %s directive." name)))
    (when args
      (cl-loop with pl = nil
	       for (p al) on args by #'cddr
	       do
	       (push al function-arglist)
	       (push p pl)
	       finally
	       (setq int-prompt (string-join (nreverse pl) "\n")))
      (setq function-arglist (nreverse function-arglist)))
    `(progn
       (defun ,function-name ,function-arglist
         ,doc
         ,(if (not (string-empty-p int-prompt))
	      `(interactive ,int-prompt)
	    '(interactive))
         (sphinx-ext--insert-directive ,name ,@function-arglist))
       ,(when key
	  `(define-key sphinx-ext-directive-map ,key #',function-name)))))

(defmacro sphinx-ext-define-insert-style-function (name key symbol doc)
  (declare (indent 1))
  (let* ((function-name (intern
			 (format "sphinx-ext-insert-style-%s" name))))
    `(progn
       (defun ,function-name ()
	 ,doc
	 (interactive)
	 (sphinx-ext--insert-style ',symbol))
       (define-key sphinx-ext-style-map ,key #',function-name))))

(defmacro sphinx-ext-define-insert-role-function (name &optional prompt &rest props)
  "Define a function to insert a Sphinx role at point.
The function will be bound to sphinx-ext-insert-role-NAME."
  (let* ((function-name (intern
			 (format "sphinx-ext-insert-role-%s" name)))
	 (int-prompt (when prompt (format "s%s" prompt)))
	 (key (plist-get props :key)))
    (if prompt
	`(progn
	   (defun ,function-name (arg)
	     ,(format "Insert a :%s: role at point." name)
	     (interactive ,int-prompt)
	     (insert (format ":%s %s: " ,name arg)))
	   ,(when key
	      `(define-key sphinx-ext-role-map ,key #',function-name)))
      `(progn
	 (defun ,function-name ()
	   ,(format "Insert a :%s: role at point." name)
	   (interactive)
	   (insert (format ":%s: " ,name)))
	 ,(when key
	    `(define-key sphinx-ext-role-map ,key #',function-name))))))

(defun sphinx-ext-add-reference (&optional arg)
  "Add a reference at point.
If the prefix arg ARG is non-nil, the alternative form of
the ref is inserted, if it has one."
  (interactive "P")
  (sphinx-ext-add-reference--next-arg arg))

(defun sphinx-ext-add-reference--next-arg (arg)
  "Process the next argument.
Called from `sphinx-ext-add-reference'.  ARG is the prefix
arg of `sphinx-ext-add-reference', which see."
  (let ((prompt "a = attribute, c = class, e = exception, f = function, m = method, r = ref: ")
	(chars '(?a ?c ?e ?f ?m ?r))
	char)
    (setq char (read-char-from-minibuffer prompt chars))
    (cond
     ((= char ?c)
      ;; insert :py:class:`...`
      (sphinx-ext--add-py "class"))
     ((= char ?e)
      ;; insert :py:exc:`...`
      (sphinx-ext--add-py "exc"))
     ((= char ?f)
      ;; insert :py:func:`...`
      (sphinx-ext--add-py "func"))
     ((= char ?m)
      ;; insert :py:meth:`...`
      (sphinx-ext--add-py "meth"))
     ((= char ?a)
      ;; insert :py:attr:`...`
      (sphinx-ext--add-py "attr"))
     ((= char ?r)
      ;; insert :ref:`...`
      (if arg
	  (sphinx-ext--add-ref-with-link)
	(sphinx-ext--add-ref)))
     (t
      (error (format "invalid character %c" char))))))

(defun sphinx-ext--add-ref ()
  "Add a reference to an arbitrary location in any document.
Prompts the user for the label."
  (let ((label (read-from-minibuffer "Label: "))
	(buf (current-buffer)))
    (princ (format ":ref:`%s`" label) buf)))

(defun sphinx-ext--add-ref-with-link ()
  "Add a reference to an arbitrary location in any document.
Prompts the user for a label, a link, and a title."
  (let ((label (read-from-minibuffer "Label: "))
	(link (read-from-minibuffer "Link: "))
	(title (read-from-minibuffer "Title: "))
	(buf (current-buffer)))
    (princ (format ":ref:`%s %s <%s>`" link title label) buf)))

(defun sphinx-ext--add-py (field)
  "Insert a Python-domain role at point.
FIELD is the name of the role being inserted.  Additionally,
prompts the user for the content of the role."
  (let ((rolename (format ":py:%s:" field))
	(ref (read-from-minibuffer "Ref: "))
	(buf (current-buffer)))
    (princ (format "%s`%s`" rolename ref) buf)))

;;;###autoload
(defun sphinx-ext-align ()
  "Align the current line with the first line of this role.

Point should be on the second line of a ':param:' or
':keyword:' role after `auto-fill-mode' kicks in, for best
effect."
  (interactive)
  (let ((pos (point-marker))
	beg end indent)
    (save-excursion
      (save-match-data
	(unless (re-search-backward user-ext-sphinx-param-regexp nil t)
	  (user-error "Did not find a :param/keyword: here"))
	(setq beg (match-beginning 0) end (match-end 0)
	      indent (- (1- end) beg))
	(assert (> end beg))
	(goto-char pos)
	(beginning-of-line)
	(insert (make-string indent ?\ ))))))

(defun sphinx-ext--y-or-n-p (prompt)
  (let* ((prompt (format "%s y/n " prompt))
	 (answer (char-to-string
		  (read-char-choice prompt (list ?y ?n)))))
    (string= answer "y")))

(defun sphinx-ext--option (option indent type &optional optional)
  "Insert an option to a directive.
OPTION is an option (i.e., \":linenos:\").  INDENT is the
indent of the option as an integer.  TYPE is a symbol
controlling the type of the argument to OPTION.

If ASK-YES-NO is non-nil, the user is asked whether they
want to add this option."
  nil
  (let* ((indent (make-string indent ?\ ))
	 (prompt (format "Insert %s? " option))
	 res
	 argument)
    (if optional
	(and (sphinx-ext--y-or-n-p
	      (format "Insert %s?" option))
	     res)
      (setq argument
	    (pcase type
	      ('bool
	       (sphinx-ext--y-or-n-p option))
	      ('string
	       (read-minibuffer
		(format "%s: " option))))
	    res (if (eq type 'bool)
		    (and argument (format "%s:%s:" indent option))
		  (format "%s:%s: %s" indent option argument))))))

;; Skeletons

(defmacro sphinx-ext-define-skeleton (name doc &rest skel)
  "Define a `sphinx-doc-mode' skeleton using NAME DOC and SKEL.
The skeleton will be bound to sphinx-ext-skeleton-NAME."
  (declare (indent 1))
  (let* ((name (symbol-name name))
	 (function-name (intern (concat "sphinx-ext-skeleton-" name))))
    `(progn
       (define-abbrev python-mode-abbrev-table ,name "" #',function-name :system t)
       (define-skeleton ,function-name
	 ,doc
	 ,@skel))))

(defmacro sphinx-ext-define-auxiliary-skeleton (name &optional doc &rest skel)
  "Define an auxiliary skeleton for `sphinx-doc-mode'.
The skeleton is defined with NAME DOC and SKEL.  The
skeleton is bound to sphinx-ext--skeleton-NAME."
  (declare (indent 1))
  (let* ((name (symbol-name name))
	 (function-name (intern (concat "sphinx-ext-skeleton--" name)))
	 (msg (format "Add %s? " name)))
    (when (not skel)
      ;; Default skeleton
      (setq skel
	    `(< ,(format "%s: " name) \n \n
		> _ \n)))
    `(progn
       (define-skeleton ,function-name
	 ,(or doc (format "Add %s." name))
	 (unless (y-or-n-p ,msg)
	   (signal 'quit t))
	 ,@skel))))

(sphinx-ext-define-auxiliary-skeleton return
  "Insert a ':returns:'."
  ":returns: " str)

(sphinx-ext-define-skeleton docstring
  "Insert a Sphinx-style docstring at point."
  "First line: "
  str \n \n
  ("Argument: "
   ":param " str ": " \n \n)
  '(sphinx-ext-skeleton--return))

(sphinx-ext-define-skeleton code-block
  "Insert a code block."
  "Language: "
  ".. code:: " str \n
  _ \n)

(sphinx-ext-define-skeleton literalinclude
  "Insert a directive to include code from another file."
  "File: "
  ".. literalinclude:: " str \n
  (sphinx-ext--option "language" 3 'string) \n
  (sphinx-ext--option "emphasize-lines" 3 'string) & \n
  (sphinx-ext--option "linenos" 3 'bool))

;; Key bindings

(define-prefix-command 'sphinx-ext-role-map nil (sphinx-ext--role-map-prompt))
(define-key sphinx-doc-mode-map (kbd "C-c C-R") #'sphinx-ext-role-map)

(define-prefix-command 'sphinx-ext-style-map nil (sphinx-ext--style-map-prompt))
(define-key sphinx-doc-mode-map (kbd "C-c C-S") #'sphinx-ext-style-map)

(sphinx-ext-define-insert-role-function "returns" nil :key "r")

(sphinx-ext-define-insert-role-function "rtype" nil :key "R")

(sphinx-ext-define-insert-role-function "param" "Name: " :key "p")

(sphinx-ext-define-insert-role-function "type" "Parameter: " :key "t")

(sphinx-ext-define-insert-role-function "keyword" "Keyword: " :key "k")

(sphinx-ext-define-insert-style-function
    "italics" "i" italic "Insert italics at point.")

(sphinx-ext-define-insert-style-function
    "code" "c" code "Insert inline code at point.")

(sphinx-ext-define-insert-style-function
    "code-block" "C" code-block "Insert a code block at point.")

(sphinx-ext-define-insert-directive-function "automodule"
  :key "M"
  :args ("sModule: " module)
  :doc "Insert an automodule directive.
MODULE is the name of the module.")

(sphinx-ext-define-insert-directive-function "autosummary"
  :key "S")

(sphinx-ext-define-insert-directive-function "toctree"
  :key "T")

;; Admonitions
(sphinx-ext-define-insert-directive-function "attention" :key "a")
(sphinx-ext-define-insert-directive-function "caution"   :key "c")
(sphinx-ext-define-insert-directive-function "danger"    :key "d")
(sphinx-ext-define-insert-directive-function "error"     :key "e")
(sphinx-ext-define-insert-directive-function "hint"      :key "h")
(sphinx-ext-define-insert-directive-function "important" :key "i")
(sphinx-ext-define-insert-directive-function "note"      :key "n")
(sphinx-ext-define-insert-directive-function "tip"       :key "t")
(sphinx-ext-define-insert-directive-function "warning"   :key "w")
(sphinx-ext-define-insert-directive-function "seealso"   :key "s")

;; TODO: admonition
(define-key sphinx-doc-mode-map (kbd "C-c i d") #'sphinx-ext-skeleton-docstring)
(define-key sphinx-doc-mode-map (kbd "C-c M-r") #'sphinx-ext-add-reference)
(define-key sphinx-doc-mode-map (kbd "<backtab>") #'sphinx-ext-align)

;; Menu

(easy-menu-define user-ext-sphinx-doc-menu-map sphinx-doc-mode-map
  "Sphinx Minor Mode"
  '("Sphinx"
    ["Add reference" sphinx-ext-add-reference]
    ["Align" sphinx-ext-align]
    "---"
    ("Insert Roles"
     [":keyword:" sphinx-ext-insert-role-keyword]
     [":param:" sphinx-ext-insert-role-param]
     [":returns:" sphinx-ext-insert-role-returns]
     [":rtype:" sphinx-ext-insert-role-rtype]
     [":type:" sphinx-ext-insert-role-type])
    ("Insert Directives"
     ["automodule" sphinx-ext-insert-directive-automodule]
     ["autosummary" sphinx-ext-insert-directive-autosummary]
     ["autotoctree" sphinx-ext-insert-directive-toctree]
     "---"
     ["caution" sphinx-ext-insert-directive-caution]
     ["danger" sphinx-ext-insert-directive-danger]
     ["error" sphinx-ext-insert-directive-error]
     ["hint" sphinx-ext-insert-directive-hint]
     ["important" sphinx-ext-insert-directive-important]
     ["note" sphinx-ext-insert-directive-note]
     ["tip" sphinx-ext-insert-directive-tip]
     ["warning" sphinx-ext-insert-directive-warning]
     ["seealso" sphinx-ext-insert-directive-seealso])
    ("Style"
     ["Code" sphinx-ext-insert-style-code]
     ["Italics" sphinx-ext-insert-style-italics]
     ["Code Block" sphinx-ext-insert-style-code-block])
    ("Skeletons"
     ["Docstring" sphinx-ext-skeleton-docstring]
     ["Literal include" sphinx-ext-skeleton-literalinclude])))

;; Hooks

;;;###autoload
(defun sphinx-doc-mode--extra-hook ()
  "Hook for `sphinx-doc-mode'."
  (setq-local skeleton-further-elements
	      '((^ (- (1+ (current-indentation)))))))

;;;###autoload
(add-hook 'sphinx-doc-mode-hook #'sphinx-doc-mode--extra-hook)

(provide 'python-sphinx-ext)

;;; python-sphinx-ext ends here

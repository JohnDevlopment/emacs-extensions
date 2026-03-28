;; -*- lexical-binding: t; -*-

(check-emacs-minimum-version "27.4")

(require 'alist-ext)
(require 'sphinx-doc)


;; ### Functions

(defun python-ext/sphinx--style-map-prompt ()
  "Prompt for `user-ext-python/sphinx-style-map'."
  (concat
   (format "i = %s"
	   (propertize "italics" 'face 'italic))
   (format ", c = %s"
	   (propertize "code" 'face 'font-lock-type-face))
   (format ", C = %s"
	   (propertize "code block" 'face 'font-lock-type-face))))

(defun python-ext/sphinx--directive-map-prompt ()
  "Prompt for `user-ext-python/sphinx-directive-map'."
  (concat
   "M-c = currentmodule, "
   "C = autoclass, "
   "M = automodule, "
   "S = autosummary, "
   "T = toctree, "
   "a = attention, "
   "c = caution, "
   "d = danger, "
   "e = error, "
   "h = hint, "
   "n = note, "
   "t = tip, "
   "w = warning, "
   "s = seealso"))

(defun python-ext/sphinx--role-map-prompt ()
  "Prompt for `user-ext-python/sphinx-role-map'."
  (let ((type-face 'font-lock-type-face))
    (concat
     (format "r = %s" (propertize ":returns:" 'face type-face))
     (format ", R = %s" (propertize ":rtype:" 'face type-face))
     (format ", k = %s" (propertize ":keyword:" 'face type-face))
     (format ", p = %s" (propertize ":param:" 'face type-face))
     (format ", t = %s" (propertize ":type:" 'face type-face)))))

(defun python-ext/sphinx--insert-style (what)
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
       (python-ext/sphinx-skeleton-code-block))
      ('italic
       (insert "``")
       (left-char)))))

(defun python-ext/sphinx--insert-directive (what &rest args)
  "Insert the directive WHAT."
  (let (spaces)
    (insert (format ".. %s::" what))
    (setq spaces (concat
		  (save-excursion
		    (back-to-indentation)
		    (buffer-substring (line-beginning-position) (point)))
		  "   "))
    (pcase what
      ((or "automodule" "currentmodule")
       ;; args: module
       (insert " " (car args)))
      ("autoclass"
       ;; args: class members
       (cl-destructuring-bind (class members) args
	 (insert ?\  class)
	 (newline)
	 (insert spaces (format ":members:%s"
				(if (not (string-empty-p members))
				    (concat " " members)
				  "")))))
      ("toctree"
       ;; args: maxdepth
       (newline)
       (insert spaces (format ":maxdepth: %d" (car args)))))
    (newline 2)
    (insert spaces)))

(defmacro python-ext/sphinx-define-insert-directive-function (name &rest props)
  "Define an interactive function for inserting a Sphinx directive.
The function name is constructed as
python-ext/sphinx-insert-directive-NAME, where NAME is the
provided directive name.

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
			(format "python-ext/sphinx-insert-directive-%s" name)))
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
         (python-ext/sphinx--insert-directive ,name ,@function-arglist))
       ,(when key
	  `(keymaps-ext-set-keymap user-ext-python/sphinx-directive-map ,key #',function-name)))))

(defun python-ext/sphinx-add-reference (&optional arg)
  "Add a reference at point.
If the prefix arg ARG is non-nil, the alternative form of
the ref is inserted, if it has one."
  (interactive "P")
  (python-ext/sphinx-add-reference--next-arg arg))

(defun python-ext/sphinx-add-reference--next-arg (arg)
  "Process the next argument.
Called from `python-ext/sphinx-add-reference'.  ARG is the
prefix arg of `python-ext/sphinx-add-reference', which see."
  (let ((prompt "a = attribute, c = class, e = exception, f = function, m = method, r = ref: ")
	(chars '(?a ?c ?e ?f ?m ?r))
	char)
    (setq char (read-char-from-minibuffer prompt chars))
    (cond
     ((= char ?c)
      ;; insert :py:class:`...`
      (python-ext/sphinx--add-py "class"))
     ((= char ?e)
      ;; insert :py:exc:`...`
      (python-ext/sphinx--add-py "exc"))
     ((= char ?f)
      ;; insert :py:func:`...`
      (python-ext/sphinx--add-py "func"))
     ((= char ?m)
      ;; insert :py:meth:`...`
      (python-ext/sphinx--add-py "meth"))
     ((= char ?a)
      ;; insert :py:attr:`...`
      (python-ext/sphinx--add-py "attr"))
     ((= char ?r)
      ;; insert :ref:`...`
      (if arg
	  (python-ext/sphinx--add-ref-with-link)
	(python-ext/sphinx--add-ref)))
     (t
      (error (format "invalid character %c" char))))))

(defun python-ext/sphinx--add-ref ()
  "Add a reference to an arbitrary location in any document.
Prompts the user for the label."
  (let ((label (read-from-minibuffer "Label: "))
	(buf (current-buffer)))
    (princ (format ":ref:`%s`" label) buf)))

(defun python-ext/sphinx--add-ref-with-link ()
  "Add a reference to an arbitrary location in any document.
Prompts the user for a label, a link, and a title."
  (let ((label (read-from-minibuffer "Label: "))
	(link (read-from-minibuffer "Link: "))
	(title (read-from-minibuffer "Title: "))
	(buf (current-buffer)))
    (princ (format ":ref:`%s %s <%s>`" link title label) buf)))

(defun python-ext/sphinx--add-py (field)
  "Insert a Python-domain role at point.
FIELD is the name of the role being inserted.  Additionally,
prompts the user for the content of the role."
  (let ((rolename (format ":py:%s:" field))
	(ref (read-from-minibuffer "Ref: "))
	(buf (current-buffer)))
    (princ (format "%s`%s`" rolename ref) buf)))

(defun python-ext/sphinx-insert-docstring--returns (type)
  "Insert :returns: with the given TYPE.
If TYPE is provided, an additional :rtype: is added."
  (cl-check-type type string)
  (newline 2)
  (insert ":returns: ...")
  (when (not (string-empty-p type))
    (newline)
    (insert ":rtype: " type)))

(defun python-ext/sphinx-insert-docstring--tag (tag name type)
  "Insert \":TAG:\" with NAME and TYPE.

TAG, NAME and TYPE must be strings."
  (cl-check-type tag string)
  (cl-check-type name string)
  (cl-check-type type string)
  (when (string-empty-p name)
      (user-error "Arg 2 cannot be empty"))
  (newline 2)
  (cond
   ((string-empty-p type)
    ;; No type
    (insert (format ":param %s:" name)))
   ((string-match ".+? +or .+" "int or str")
    ;; Type is "x or y..."
    (insert (format ":param %s:" name))
    (newline)
    (insert (format ":type %s: %s" name type)))
   (t
    ;; Type is anything else
    (insert (format ":param %s %s:" type name)))))

(defun python-ext/sphinx-insert-docstring ()
  "Insert documentation for the enclosing function.

First, the user is prompted for the first line, which
summarizes the function.  Second, they are prompted for a
series of positional or keyword arguments: if they press p,
they are asked for the name and type of the parameter, or
keyword argument if the user presses k. At this stage, the
user can press q to finish the docstring."
  (interactive)
  (let ((chars (list ?k ?p ?q ?r))
	(prompt (string-join '("p = :param:"
			       "k = :keyword:"
			       "r = :returns:"
			       "q = quit") ", "))
	(tags (alist-ext-define ?k "keyword" ?p "param" ?r "returns"))
	first-line c tag str v1)
    (setq first-line (read-string "First Line: ")
	  c (read-char-choice prompt chars))
    (insert first-line)
    (catch 'break
      (while (not (= c ?q))
	(pcase c
	  ((or ?k ?p)
	   (setq tag (alist-get c tags)
		 str (read-string "Name: ")
		 v1 (read-string "Type: "))
	   (cl-assert (not (null tag)))
	   (python-ext/sphinx-insert-docstring--tag tag str v1))
	  (?r
	   (setq str (read-string "Type: "))
	   (python-ext/sphinx-insert-docstring--returns str)
	   (throw 'break nil))
	  (?q t))
	(setq c (read-char-choice prompt chars))))))

(defun python-ext/sphinx--y-or-n-p (prompt)
  "Ask the user a yes or no question with PROMPT."
  (let* ((prompt (format "%s y/n " prompt))
	 (answer (char-to-string
		  (read-char-choice prompt (list ?y ?n)))))
    (string= answer "y")))

(defun python-ext/sphinx--option (option indent type &optional optional)
  "Insert an option to a directive.
OPTION is an option (i.e., \":linenos:\").  INDENT is the
indent of the option as an integer.  TYPE is a symbol
controlling the type of the argument to OPTION.

If OPTIONAL is non-nil, the user is asked whether they
want to add this option."
  nil
  (let* ((indent (make-string indent ?\ ))
	 (prompt (format "Insert %s? " option))
	 res
	 argument)
    (if optional
	(and (python-ext/sphinx--y-or-n-p prompt) res)
      (setq argument
	    (pcase type
	      ('bool
	       (python-ext/sphinx--y-or-n-p option))
	      ('string
	       (read-minibuffer
		(format "%s: " option))))
	    res (if (eq type 'bool)
		    (and argument (format "%s:%s:" indent option))
		  (format "%s:%s: %s" indent option argument))))))


;; --- Style functions

(define-prefix-command 'user-ext-python/sphinx-style-map
		       nil
		       (python-ext/sphinx--style-map-prompt))
(keymaps-ext-set-keymap sphinx-doc-mode-map
			"C-c C-S"
			#'user-ext-python/sphinx-style-map)

(defmacro python-ext/sphinx-define-insert-style-function (name key symbol doc)
  "Define a style insertion function with NAME, KEY, SYMBOL, and DOC.

The function is called python-ext/sphinx-insert-style-NAME.
The function is mapped to `user-ext-python/sphinx-style-map'
with KEY.  DOC is the docstring to the function.  SYMBOL is
the argument to `python-ext/sphinx--insert-style'."
  (declare (indent 1))
  (let* ((function-name (intern
			 (format "python-ext/sphinx-insert-style-%s" name))))
    `(progn
       (defun ,function-name ()
	 ,doc
	 (interactive)
	 (python-ext/sphinx--insert-style ',symbol))
       (keymaps-ext-set-keymap user-ext-python/sphinx-style-map ,key #',function-name))))

(python-ext/sphinx-define-insert-style-function "italics"
  "i"
  italic
  "Insert italics at point.")
(python-ext/sphinx-define-insert-style-function "code"
  "c"
  code
  "Insert inline code at point.")
(python-ext/sphinx-define-insert-style-function "code-block"
  "C"
  code-block
  "Insert a code block at point.")


;; --- Role insertion

;; Has to be here because `python-ext/sphinx-define-insert-role-function' maps
;; its :key to this map.
(define-prefix-command 'user-ext-python/sphinx-role-map
		       nil
		       (python-ext/sphinx--role-map-prompt))
(keymaps-ext-set-keymap sphinx-doc-mode-map "C-c C-R" #'user-ext-python/sphinx-role-map)

(cl-defmacro python-ext/sphinx-define-insert-role-function (name &key prompt key)
  "Define a command to insert a role at point.
The function created will be named python-ext/sphinx-insert-role-NAME.
NAME is the name of the role, a string.
The rest of the arguments are keywords.

:prompt PROMPT - If provided, PROMPT is displayed to the
user, asking for an argument to the rule.
:key KEY - If provided, python-ext/sphinx-insert-role-NAME is
mapped to KEY in `python-subext_sphinx'."
  (let* ((function-name (intern
			 (format "python-ext/sphinx-insert-role-%s" name)))
	 (int-prompt (when prompt (format "s%s" prompt))))
    (if prompt
	`(progn
	   (defun ,function-name (arg)
	     ,(format "Insert a :%s: role at point." name)
	     (interactive ,int-prompt)
	     (insert (format ":%s %s: " ,name arg)))
	   ,(when key
	      `(keymaps-ext-set-keymap user-ext-python/sphinx-role-map
				       ,key
				       #',function-name)))
      `(progn
	 (defun ,function-name ()
	   ,(format "Insert a :%s: role at point." name)
	   (interactive)
	   (insert (format ":%s: " ,name)))
	 ,(when key
	    `(keymaps-ext-set-keymap user-ext-python/sphinx-role-map
				     ,key
				     #',function-name))))))

(python-ext/sphinx-define-insert-role-function "keyword" :prompt "Keyword: " :key "k")
(python-ext/sphinx-define-insert-role-function "param" :prompt "Name: " :key "p")
(python-ext/sphinx-define-insert-role-function "returns" :key "r")
(python-ext/sphinx-define-insert-role-function "rtype" :key "R")
(python-ext/sphinx-define-insert-role-function "type" :prompt "Parameter: " :key "t")


;; --- Directive insertion

(define-prefix-command 'user-ext-python/sphinx-directive-map
		       nil
		       (python-ext/sphinx--directive-map-prompt))
(keymaps-ext-set-keymap sphinx-doc-mode-map
			"C-c C-d"
			#'user-ext-python/sphinx-directive-map)

(python-ext/sphinx-define-insert-directive-function "autoclass"
  :key "C"
  :args ("sClass: " class "sMembers: " members))

(python-ext/sphinx-define-insert-directive-function "automodule"
  :key "M"
  :args ("sModule: " module)
  :doc "Insert an automodule directive.
MODULE is the name of the module.")

(python-ext/sphinx-define-insert-directive-function "autosummary" :key "S")

(python-ext/sphinx-define-insert-directive-function "toctree"
  :key "T"
  :args ("nMaxdepth: " maxdepth)
  :doc "Insert a toctree directive with MAXDEPTH.
MAXDEPTH corresponds to the :maxdepth: property.")

(python-ext/sphinx-define-insert-directive-function "currentmodule"
  :key "M-c"
  :args ("sModule: " module))

;; Admonitions
(python-ext/sphinx-define-insert-directive-function "attention" :key "a")
(python-ext/sphinx-define-insert-directive-function "caution"   :key "c")
(python-ext/sphinx-define-insert-directive-function "danger"    :key "d")
(python-ext/sphinx-define-insert-directive-function "error"     :key "e")
(python-ext/sphinx-define-insert-directive-function "hint"      :key "h")
(python-ext/sphinx-define-insert-directive-function "important" :key "i")
(python-ext/sphinx-define-insert-directive-function "note"      :key "n")
(python-ext/sphinx-define-insert-directive-function "tip"       :key "t")
(python-ext/sphinx-define-insert-directive-function "warning"   :key "w")
(python-ext/sphinx-define-insert-directive-function "seealso"   :key "s")


;; ### Skeletons

(defmacro python-ext/sphinx-define-skeleton (name doc &rest skel)
  "Define a Sphinx skeleton using NAME DOC and SKEL.
The skeleton will be bound to sphinx-ext-skeleton-NAME."
  (declare (indent 1))
  (let* ((name (symbol-name name))
	 (function-name (intern (concat "python-ext/sphinx-skeleton-" name))))
    `(progn
       (define-abbrev python-mode-abbrev-table ,name "" #',function-name :system t)
       (define-skeleton ,function-name
	 ,doc
	 ,@skel))))

(defmacro python-ext/sphinx-define-auxiliary-skeleton (name &optional doc &rest skel)
  "Define a Sphinx auxiliary skeleton.
The skeleton is defined with NAME DOC and SKEL.  The
skeleton is bound to python-ext/sphinx--skeleton-NAME."
  (declare (indent 1))
  (let* ((name (symbol-name name))
	 (function-name (intern (concat "python-ext/sphinx-skeleton--" name)))
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

(python-ext/sphinx-define-skeleton code-block
  "Insert a code block."
  "Language: "
  ".. code:: " str \n
  _ \n)

(python-ext/sphinx-define-skeleton literalinclude
  "Insert a directive to include code from another file."
  "File: "
  ".. literalinclude:: " str \n
  (python-ext/sphinx--option "language" 3 'string) \n
  (python-ext/sphinx--option "emphasize-lines" 3 'string) & \n
  (python-ext/sphinx--option "linenos" 3 'bool))


;; ### Key bindings

(easy-menu-define user-ext-python/sphinx-doc-menu-map sphinx-doc-mode-map
  "Sphinx Minor Mode"
  '("Sphinx"
    ["Add reference" python-ext/sphinx-add-reference]
    "---"
    ("Insert Roles"
     [":keyword:" python-ext/sphinx-insert-role-keyword]
     [":param:" python-ext/sphinx-insert-role-param]
     [":returns:" python-ext/sphinx-insert-role-returns]
     [":rtype:" python-ext/sphinx-insert-role-rtype]
     [":type:" python-ext/sphinx-insert-role-type])
    ("Insert Directives"
     ["autoclass" python-ext/sphinx-insert-directive-autoclass]
     ["automodule" python-ext/sphinx-insert-directive-automodule]
     ["autosummary" python-ext/sphinx-insert-directive-autosummary]
     ["currentmodule" python-ext/sphinx-insert-directive-currentmodule]
     ["toctree" python-ext/sphinx-insert-directive-toctree]
     "---"
     ["caution" python-ext/sphinx-insert-directive-caution]
     ["danger" python-ext/sphinx-insert-directive-danger]
     ["error" python-ext/sphinx-insert-directive-error]
     ["hint" python-ext/sphinx-insert-directive-hint]
     ["important" python-ext/sphinx-insert-directive-important]
     ["note" python-ext/sphinx-insert-directive-note]
     ["tip" python-ext/sphinx-insert-directive-tip]
     ["warning" python-ext/sphinx-insert-directive-warning]
     ["seealso" python-ext/sphinx-insert-directive-seealso])
    ("Style"
     ["Code" python-ext/sphinx-insert-style-code]
     ["Italics" python-ext/sphinx-insert-style-italics]
     ["Code Block" python-ext/sphinx-insert-style-code-block])
    ("Skeletons"
     ["Docstring" python-ext/sphinx-insert-docstring]
     ["Literal include" python-ext/sphinx-skeleton-literalinclude])))

(keymaps-ext-set-keymap sphinx-doc-mode-map "C-c C-M-d" #'python-ext/sphinx-insert-docstring)
(keymaps-ext-set-keymap sphinx-doc-mode-map "C-c M-r"   #'python-ext/sphinx-add-reference)


;; ### Hooks

;;;###autoload
(defun sphinx-doc-mode--extra-hook ()
  "Hook for `sphinx-doc-mode'."
  (setq-local skeleton-further-elements
	      '((^ (- (1+ (current-indentation)))))))

;;;###autoload
(add-hook 'sphinx-doc-mode-hook #'sphinx-doc-mode--extra-hook)


(cl-pushnew 'sphinx user-ext-python-subextensions)
(extension-provide 'python-ext user-ext-python-subextensions)
;;; sphinx.el ends here

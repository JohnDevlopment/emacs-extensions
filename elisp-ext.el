;; -*- lexical-binding: t;  -*-

(require 'autoload)
(require 'button)
(require 'cl-lib)
(require 'function-ext)
(require 'hideshow)
(require 's)

(eval-when-compile
  (require 'cl-ext))

;; ### Abbrevs

(define-abbrev emacs-lisp-mode-abbrev-table "intry" "interactively" #'abbrev-ext-insert-hook :system t)

;; ### Advice

(fext-defadvice eval-region (after eval-region)
  "Deactive mark after evalling region."
  (deactivate-mark))

;; ### Variables

(defmacro elisp-ext--rx (&rest body)
  "Translate regular expressions REGEXPS in sexp form to a regexp string.
This is the same as `rx' but adds two additional constructs.

identifier-char A character that is valid for symbols in
                Emacs Lisp.
identifier      A symbol consisting of one or more character
                that match IDENTIFIER-CHAR."
  `(rx-let ((identifier-char (or (syntax word) (syntax symbol)))
	    (identifier (+ identifier-char)))
     (rx ,@body)))

(defvar-local user-ext-elisp--scratch-minify nil
  "If non-nil, minify the contents of the scratch buffer.")

(defvar user-ext-elisp--doc-scratch nil
  "The buffer being used by `elisp-ext-doc-scratch-buffer'.")

(defconst user-ext-elisp--register ?e
  "Used with `window-configuration-to-register'.")

(defconst user-ext-elisp-section-comment-regexp
  (elisp-ext--rx
   bol (seq ";;" (* (syntax whitespace))
	    "###" (* (syntax whitespace)))
   (group (+ nonl)))
  "Regular expression for section comments.")

(define-obsolete-variable-alias
  'user-ext-elisp-separator-regexp
  'user-ext-elisp-separator-comment-regexp
  "2025-06-22")
(defconst user-ext-elisp-separator-comment-regexp
  (elisp-ext--rx bol ";; " (group (>= 10 ?-)))
  "Regular expression for so-called separator comments.")

(defconst user-ext-elisp-subsection-comment-regexp
  (elisp-ext--rx
   bol (seq ";;" (* (syntax whitespace))
	    "---" (* (syntax whitespace)))
   (group (+ nonl))))

(defconst user-ext-elisp-imenu-expression
  `(("Sections" ,user-ext-elisp-section-comment-regexp 1)))

(defconst user-ext-elisp-valid-occur-types
  '(functions sections types variables))

;; --- Regular expressions

(defconst user-ext-elisp-defun-regexp
  (elisp-ext--rx
   ?\( (group (or "cl-defgeneric" "defun" "defmacro")) (+ (syntax whitespace))
   (group identifier))
  "Regular expression for function-like definitions (e.g. defun).
Group 1 matches the keyword used to start the definition.
Group 2 matches the name of the definition.")

(defconst user-ext-elisp-variable-regexp
  (elisp-ext--rx
   ?\( "def" (group (or "const" "custom" "var" "var-local"))
   (+ (syntax whitespace)) (group identifier))
  "Regular expression for variable definitions.
Group 1 matches the characters after the initial \"def\" (see below).
Group 2 matches the name of the variable.

Group 1 can be one of the following:
- \"const\"
- \"custom\"
- \"var\"
- \"var-local\"")

(defconst user-ext-elisp-face-regexp
  (elisp-ext--rx
   ?\( "defface" (+ (syntax whitespace)) (group identifier))
  "Regular expression for face definitions.
Group 1 matches the name of the face.")

(defconst user-ext-elisp-structure-regexp
  (elisp-ext--rx
   ?\( "cl-defstruct" (+ (syntax whitespace)) (group identifier))
  "Regular expression for structure definitions.
Group 1 matches the name of the face.")

(defconst user-ext-elisp-mode-local-variable-regexp
  (elisp-ext--rx ?\( "def"
		 (group (or "const" "var")) "-mode-local" ; var type
		 (+ (syntax whitespace))
		 (group identifier)	; mode
		 (+ (syntax whitespace))
		 (group identifier)	; name
		 )
  "Regular expression for mode-local variable definitions.
Group 1 matches one of \"const\" \"var\".
Group 2 matches the mode this variable is local to.
Group 3 matches the name.")

;; ### Functions

;; --- Skeletons

(defmacro elisp-ext-define-skeleton (name docstring &rest skel)
  "Define an Emacs Lisp mode command to insert a skeleton.
The command will be named elisp-ext-skeleton-NAME.  DOC is
the documentation of the command.  SKEL is the skeleton
itself.

\(fn NAME DOCSTRING INTERACTOR SKELETON...\)"
  (declare (debug (&define name stringp skeleton-edebug-spec))
	   (indent 1) (doc-string 2))
  (cl-assert (symbolp name))
  (let* ((function-name (intern (format "elisp-ext-skeleton-%S" name))))
    `(progn
       (define-skeleton ,function-name
	 ,docstring
	 ,@skel))))

(elisp-ext-define-skeleton defun
  "Insert a function."
  "Name: "
  "(defun " str " ("
  ("Arg %s: " (unless (= (char-before) ?\()
		" ")
   str)
  ?\) \n
  (progn
    (when (and (y-or-n-p "Docstring? ")
	       (setq v1 (read-string "First line: " nil t))
	       (not (string-empty-p v1)))
      (format "\"%s\"\n" v1)))
  >
  (progn
    (when (y-or-n-p "Interactive? ")
      (setq v1 (read-string "Spec: "))
      (if (not (string-empty-p v1))
	  (concat "(interactive \"" v1 "\")")
	"(interactive)")))
  _ ?\))

(elisp-ext-define-skeleton defmacro
  "Insert a function."
  "Name: "
  "(defmacro " str " ("
  ("Arg %s: " (unless (= (char-before) ?\()
		" ")
   str)
  ?\) \n
  (progn
    (when (and (y-or-n-p "Docstring? ")
	       (setq v1 (read-string "First line: " nil t))
	       (not (string-empty-p v1)))
      (format "\"%s\"\n" v1)))
  >
  _ ?\))

;; --- Occur functions

(defmacro elisp-ext--enable-minor-mode (mode1 &optional mode2)
  `(cl-ext-unless (and (boundp ',mode1) ,mode1)
     (,(or mode2 mode1) t)))

(cl-defgeneric elisp-ext--list-x (_type)
  "Return a list of certain constructs in buffer according to TYPE.
Return a list of sublists of the form (ID ENTRY), where
ENTRY is a vector with the form [NAME TYPE].")

(cl-defmethod elisp-ext--list-x ((_type (eql functions)))
  "Return a list of this buffer's functions."
  (elisp-ext--search-regexp
   user-ext-elisp-defun-regexp
   (lambda ()
     (let (sym name keyword type)
       (setq keyword (match-string-no-properties 1)
	     name (match-string-no-properties 2)
	     sym (intern-soft name)
	     name (propertize name 'length (length name))
	     type (if sym
		      (pcase keyword
			((or "cl-defun" "defun")
			 (if (commandp sym) "Command" "Function"))
			((or "cl-defmacro" "defmacro") "Macro")
			("cl-defgeneric" "Generic Function"))
		    "Undefined"))
       (list (make-symbol name)
	     (vector name type))))))

(cl-defmethod elisp-ext--list-x ((_type (eql variables)))
  "Return a list of this buffer's variables."
  (let (v1 v2)
    (setq v1 (elisp-ext--search-regexp
	      user-ext-elisp-variable-regexp
	      (lambda (&optional type name)
		(setq name (match-string-no-properties 2)
		      name (propertize name 'length (length name))
		      type (pcase (match-string-no-properties 1)
			     ("const" "Constant")
			     ("custom" "User Option")
			     ("var" "Variable")
			     ("var-local" "Buffer-Local Variable")))
		(list (make-symbol name) (vector name type))))
	  v2 (elisp-ext--search-regexp
	      user-ext-elisp-mode-local-variable-regexp
	      (lambda (&optional name type mode)
		(setq name (match-string-no-properties 3)
		      mode (match-string-no-properties 2)
		      name (propertize name 'length (length name))
		      type (pcase (match-string-no-properties 1)
			     ("const" (format "Constant (local to %s)" mode))
			     ("var" (format "Variable (local to %s)" mode))))
		(list (make-symbol name) (vector name type)))))
    (append (if v1 v1 nil)
	    (if v2 v2 nil))))

(cl-defmethod elisp-ext--list-x ((_type (eql sections)))
  "Return a list of this buffer's (sub)sections.

ID is the string name of the (sub)section.  Within ENTRY,
NAME is also the string name of the (sub)section.  TYPE is
either \"Section\" or \"Subsection\"."
  (let ((sections (elisp-ext--get-sections))
	entries)
    (alist-ext-dolist (section subsections sections entries)
      (push (list section (vector section "Section")) entries)
      (dolist (subsection subsections)
	(push (list subsection
		    (vector subsection
			    (if (elisp-ext--occur-separator-string subsection)
				"" "Subsection")))
	      entries)))
    (nreverse entries)))

(define-button-type 'occur-cross-reference
  'action #'elisp-ext--occur-cross-reference)

(defun elisp-ext--occur-cross-reference (button)
  (let* ((label (button-label button))
	 (symbol (intern-soft label))
	 (fun (button-get button 'xref-function)))
    (cl-assert fun t)
    (if symbol
	(funcall fun symbol)
      (funcall fun label))))

(defun elisp-ext--search-regexp (regexp fun)
  (cl-ext-save-point
    (goto-char (point-min))
    (cl-loop
     while (re-search-forward regexp nil t)
     unless (elisp-ext-in-comment-p)
     collect
     (cl-ext-progn
       (funcall fun)))))

(defun elisp-ext--get-sections ()
  "Return an alist of buffer's sections mapping to its subsections.

The return value is an alist mapping each section to its
respective subsections.  Each key is a string containing the
name of a section; it maps to a list of strings each
containing the name of a subsection.

Each (sub)section string contains text properties.
`position'  The buffer position of the (sub)section.
`length'    The length of the string."
  (save-excursion
    (goto-char (point-min))
    (let* ((rx (elisp-ext--rx bol ";;" (syntax whitespace) (or "###" "---")))
	   section ssl al temp)
      (cl-loop while (re-search-forward rx nil t) ; Searching for (sub)section comments
	       do
	       (goto-char (match-beginning 0))
	       (cond
		((looking-at user-ext-elisp-section-comment-regexp)
		 ;; Section
		 (cl-ext-when section
		   ;; When there is a previous section
		   (setf (alist-get section al) (cl-ext-when ssl (nreverse ssl))))
		 (setq temp (match-string-no-properties 1))
		 (setq section (propertize temp
					   'length (length temp)
					   'position (match-beginning 0))
		       ssl nil))
		((looking-at user-ext-elisp-subsection-comment-regexp)
		 ;; Subsection
		 (cl-ext-unless section
		   (user-error "Subsection '%s' is not preceded by a section"
			       (match-string 1)))
		 (setq temp (match-string-no-properties 1))
		 (push (propertize temp
				   'length (length temp)
				   'position (cl-ext-unless (elisp-ext--occur-separator-string
							     temp)
						 (match-beginning 0)))
		       ssl)))
	       (forward-line)
	       finally do
	       (cl-ext-when section
		 (setf (alist-get section al) (and ssl (nreverse ssl)))))
      (nreverse al))))

(defun elisp-ext--occur-goto-section (_string-or-symbol)
  (let (;; (string (if (cl-typep string-or-symbol 'symbol)
	;; 	    (symbol-name string-or-symbol)
	;; 	  string-or-symbol))
	(position (get-text-property (point) 'position)))
    (cl-assert (cl-typep position 'integer) t)
    (cl-assert elisp-occur-original-buffer)
    ;; (with-current-buffer elisp-occur-original-buffer
    ;;   (goto-char position))
    (pop-to-buffer elisp-occur-original-buffer)
    (goto-char position)
    (message "Jump to position %d" position)))

(defsubst elisp-ext--occur-separator-string (string)
  (string-match "^-\\{10,\\}" string))

(defun elisp-ext--occur-buttonize-entries (type)
  (cl-check-type type symbol)
  (cl-assert (memq type user-ext-elisp-valid-occur-types) t)
  (goto-char (point-min))
  (save-excursion
    (let ((inhibit-read-only t)
	  (type (cl-case type
		  (functions #'describe-function)
		  (variables #'describe-variable)
		  (types #'describe-symbol)
		  (sections #'elisp-ext--occur-goto-section))))
      (with-silent-modifications
	(cl-loop with start
		 with end
		 while (not (eobp))
		 do
		 (buffer-substring (point) (+ (point) 1))
		 (setq start (point)
		       end (let ((sz (get-text-property (point) 'length)))
			     (cl-check-type sz integer)
			     (+ start sz)))
		 (cl-ext-unless (elisp-ext--occur-separator-string
				 (buffer-substring-no-properties start end))
		     (make-button start end 'type 'occur-cross-reference
				  'xref-function type))
		 (forward-line 1))))))

(defun elisp-ext--occur-setup-buffer (type-symbol)
  (cl-ext-when (eq type-symbol 'sections)
    (setq tabulated-list-format [("Name" 50 nil) ("Type" 0 nil)]
	  tabulated-list-sort-key nil)))

(defun elisp-ext--occur (buffer-base-name type-symbol)
  "Display a list of items according to TYPE-SYMBOL.
BUFFER-BASE-NAME is the name of the buffer in which the list
is displayed.  TYPE-SYMBOL indicates what kind of list to
construct.  See `user-ext-elisp-valid-occur-types' for a
list of valid symbols."
  (cl-assert (memq type-symbol user-ext-elisp-valid-occur-types) t)
  (let* ((current-buffer (current-buffer))
	 (buffer-name (s-lex-format "*Occur: ${buffer-base-name}*"))
	 (buffer (get-buffer buffer-name)))
    (cl-ext-when buffer
      (kill-buffer buffer))
    (setq buffer (get-buffer-create buffer-name))
    (with-current-buffer buffer
      (elisp-occur-mode)
      (elisp-ext--occur-setup-buffer type-symbol)
      (setq elisp-occur-original-buffer current-buffer
	    elisp-occur-type type-symbol)
      (setq tabulated-list-entries
	    (lambda ()
	      (with-current-buffer current-buffer
		(elisp-ext--list-x type-symbol))))
      (tabulated-list-print)
      (elisp-ext--occur-buttonize-entries type-symbol))
    (display-buffer buffer)))

(defun elisp-ext-occur-sections ()
  "Display a list of this buffer's sections and subsection comments.
Occurrences of such comments will be shown in a temporary
buffer."
  (interactive)
  (elisp-ext--occur "Elisp Sections" 'sections))

(defun elisp-ext-occur-variables ()
  "Display a list of this buffer's variables.
Display a temp buffer that lists the current buffer's
functions, distinguishing their types as functions and
commands (i.e., interactive functions)."
  (interactive)
  (elisp-ext--occur "Elisp Variables" 'variables))

(defun elisp-ext-occur-functions ()
  "Display a list of this buffer's functions.
Display a temp buffer that lists the current buffer's
variables, distinguishing their types as constants, user
options, and so on."
  (interactive)
  (elisp-ext--occur "Elisp Functions" 'functions))

(defun elisp-ext-occur-types ()
  "Display a list of this buffer's types.
Display a temp buffer that lists the current buffer's types
(i.e., faces, structures, etc.)."
  (interactive)
  (elisp-ext--occur "Elisp Types" 'types))

;; --- Elisp Ext Occur Major Mode

(defun elisp-occur--revert-hook ()
  (run-with-idle-timer 0.01 nil
		       (lambda ()
			 (elisp-ext--occur-buttonize-entries elisp-occur-type))))

(fext-defadvice tabulated-list-sort (after tabulated-list-sort)
  "Sorted entries in `elisp-occur-mode'."
  (cl-assert elisp-occur-type)
  (cl-ext-when (eq major-mode 'elisp-occur-mode)
    (run-with-idle-timer 0.01 nil
			 (lambda ()
			   (elisp-ext--occur-buttonize-entries elisp-occur-type)))))

(define-derived-mode elisp-occur-mode tabulated-list-mode "Elisp Occur"
  "Major mode for elisp-ext-occur* functions."
  (setq tabulated-list-format [("Name" 50 t) ("Type" 0 t)]
	tabulated-list-sort-key '("Name" . nil))
  (add-hook 'tabulated-list-revert-hook #'elisp-occur--revert-hook nil t)
  (tabulated-list-init-header))

(defvar-local elisp-occur-original-buffer nil
  "The buffer from which this one was launched.")

(defvar-local elisp-occur-type nil
  "The type of items being listed.")

(define-key elisp-occur-mode-map (kbd "k") #'kill-and-quit)

;; --- Copy/Kill Sexp

(defun elisp-ext-copy-kill-sexp (&optional fun n)
  "Copy, kill, or delete the Lisp expression containing point.
FUN is a function that accepts two arguments, the beginning
and end of a region.  N, if non-nil, is the prefix argument,
as gotten from `current-prefix-arg' or one of its
counterparts (see below).

N is used to control the level this function will traverse:
a value of 1 means the immediate Lisp expression containing
point, 2 the expression containing said expression, and so
on.  A value of nil means 1.  If N is negative, the absolute
value is used to control the level, and the copied/killed
text is minified via `minify'.

As a side note, FUN is meant to be one of the \"kill-ish\"
functions that operate on regions.  This function is called
by one these commands:

- `elisp-ext-copy-sexp' --- passes `kill-ring-save' as FUN
- `elisp-ext-kill-sexp' --- passes `kill-region' as FUN
- `elisp-ext-delete-sexp' --- passes `delete-region' as FUN"
  (let* ((pps (make-ppss-easy (syntax-ppss)))
	 (n (and n (prefix-numeric-value n)))
	 (beg (cl-ext-progn
		(cl-assert pps t)
		(if n
		    (ppss-open-paren-depth pps (abs n))
		  (ppss-innermost-start pps))))
	 end)
    (cl-ext-unless beg
	(user-error "Not inside a Lisp expression"))
    (cl-ext-save-point
      (goto-char beg)
      (forward-sexp)
      (setq end (point)))
    (cl-ext-when (and beg end)
	(funcall fun beg end)
      (cl-ext-when (and n (< n 0))
	  (with-temp-buffer
	    (yank)
	    (goto-char (point-min))
	    (minify 0 1 t)
	    (kill-region (point-min) (point-max)))))))

(defun elisp-ext-jump-sexp-level (&optional n)
  (interactive "p")
  (let* ((pps (make-ppss-easy (syntax-ppss)))
	 (pos (cl-ext-progn
		(cl-assert pps t)
		(if n
		    (ppss-open-paren-depth pps n)
		  (ppss-innermost-start pps)))))
    (cl-ext-unless pos
	(user-error "Not inside a Lisp expression"))
    (goto-char pos)))

(defun elisp-ext-delete-sexp (&optional n interactive-p)
  "Delete the Lisp expression containing point without saving it.
N is the prefix argument: if non-nil, it specifies the level
to traverse from point (see below). A value of 1 denotes the
Lisp expression containing point, 2 the containing Lisp
expression of that, and so on.  A value of nil effectively
means 1.

See also `universal-argument'."
  (interactive (list current-prefix-arg t))
  (barf-if-buffer-read-only)
  (cl-ext-unless interactive-p
    (error "Must be called interactively"))
  (elisp-ext-copy-kill-sexp #'delete-region n))
(put 'elisp-ext-delete-sexp 'interactive-only "use `elisp-ext-copy-kill-sexp'.")

(defun elisp-ext-kill-sexp (&optional n interactive-p)
  "Kill (\"cut\") the Lisp expression containing point.
N is the prefix argument: if non-nil, it specifies the level
to traverse from point (see below). A value of 1 denotes the
Lisp expression containing point, 2 the containing Lisp
expression of that, and so on.  A value of nil effectively
means 1.

See also `universal-argument'."
  (interactive (list current-prefix-arg t))
  (barf-if-buffer-read-only)
  (cl-ext-unless interactive-p
    (error "Must be called interactively"))
  (elisp-ext-copy-kill-sexp #'kill-region n))
(put 'elisp-ext-kill-sexp 'interactive-only "use `elisp-ext-copy-kill-sexp'.")

(defun elisp-ext-copy-sexp (&optional n interactive-p)
  "Save the containing Lisp expression as if killed, but don't kill it.
N is the prefix argument: if non-nil, it specifies the level
to traverse from point (see below). A value of 1 denotes the
Lisp expression containing point, 2 the containing Lisp
expression of that, and so on.  A value of nil effectively
means 1.

See also `universal-argument'."
  (interactive (list current-prefix-arg t))
  (--print-expr var n)
  (cl-ext-unless interactive-p
    (error "Must be called interactively"))
  (elisp-ext-copy-kill-sexp #'kill-ring-save n))
(put 'elisp-ext-copy-sexp 'interactive-only "use `elisp-ext-copy-kill-sexp'.")

;; --- Other Functions

(defsubst elisp-ext-in-comment-p ()
  "Return non-nil if inside a comment."
  (ppss-comment-depth (make-ppss-easy (syntax-ppss))))

;;;###autoload
(defun elisp-ext-minify (start end &optional force)
  "Minify the code between START and END in current buffer.
START and END are the two points in a region.  If the region
is not active, minify the whole buffer, asking the user
beforehand; unless FORCE is non-nil, in which, do it without
asking.

If called interactively, START and END are the region,
provided the region is active, otherwise they are ignored.
FORCE is the prefix argument."
  (interactive "r\nP")
  (cl-block quit
    (let ((msg "The region is not active, so the entire buffer will be minified. Continue?")
	  (reg (region-active-p))
	  answer
	  bstr)
      (setq answer (or reg force (y-or-n-p msg)))
      (cond
       ((and (not reg) (not answer))
	;; Quit; user said no and the region is not active
	(message "Quit")
	(cl-return-from quit))
       ((and (not reg) answer)
	;; User said yes but the region is not active
	(setq start (point-min) end (point-max))))
      (setq bstr (buffer-substring-no-properties start end))
      (delete-region start end)
      (setq bstr (replace-regexp-in-string "[ \t\n\r]+" " " bstr))
      (insert bstr))))

;;;###autoload
(defun elisp-ext-forward-or-backward-sexp (&optional arg)
  "Go to the matching parenthesis to the one is adjacent at point.
With ARG, do it that many times.  A negative arg -N reverses
the direction of the motion."
  (interactive "^p")
  (cond ((looking-at "\\s(")
	 (forward-sexp arg))
	((looking-at "\\s)")
	 (forward-char)
	 (backward-sexp arg))
        ((looking-back "\\s)" 1)
	 (backward-sexp arg))
        ;; Now, try to succeed from inside of a bracket
        ((looking-back "\\s(" 1)
	 (backward-char)
	 (forward-sexp arg)
	 (backward-char))))

;;;###autoload
(defun elisp-ext-update-loadefs (&optional interactive-p)
  "Update autoload definitions of Lisp extensions.

Updates the autoload definitions in the Lisp files in
`user-ext-extension-directory'.  Binds
`generated-autoload-file' to the concatenation of
`user-ext-extension-directory' and \"loaddefs-ext.el\"."
  (interactive "p")
  (unless interactive-p
    (error "This must be called interactively"))
  (let ((generated-autoload-file "~/.emacs.d/extensions/loaddefs-ext.el"))
    (cl-check-type generated-autoload-file string)
    (update-directory-autoloads "~/.emacs.d/extensions/"
				"~/.emacs.d/extensions/packages/"))
  (let ((buffer (get-buffer "loaddefs-ext.el")))
    (cl-assert buffer)
    (if (y-or-n-p "Open buffer \"loaddefs-ext.el\"? ")
	(cl-ext-progn
	  (switch-to-buffer "loaddefs-ext.el")
	  (auto-revert-mode 1))
      (with-current-buffer buffer
	(auto-revert-mode 1)))))
(put 'elisp-ext-update-loadefs 'interactive-only t)

(defun elisp-ext--scratch-buffer-ctrl-c-ctrl-c ()
  "Kill the text inside buffer and quit."
  (interactive)
  (let ((inhibit-read-only t))
    (with-demoted-errors
	"Error `elisp-ext--scratch-buffer-ctrl-c-ctrl-c': %S"
      (goto-char (point-min))
      (cl-ext-when (re-search-forward "[ \t\n\r]+\\'" nil t) ; Delete trailing whitespace
	(replace-match ""))				     ; from the end of the buffer
      (cl-ext-when user-ext-elisp--scratch-minify	     ; Minify the buffer
	(elisp-ext-minify (point-min) (point-max) t))	     ; with prefix argument
      (kill-region (point-min) (point-max))))
  (setq user-ext-elisp--scratch-minify nil)
  (kill-and-quit))

;;;###autoload (autoload 'elisp-ext-scratch-buffer "elisp-ext" "Create a scratch buffer for Emacs lisp code." t)
(define-scratch-buffer-function elisp-ext-scratch-buffer "elisp" (&optional minify)
  "Create a scratch buffer for Emacs lisp code.
The user can type Emacs Lisp code and type C-c C-c to kill
the buffer and its contents.  If MINIFY is non-nil, minify
the buffer contents prior to killing it.

When called interactively, MINIFY is the prefix argument."
  "P"
  (emacs-lisp-mode)
  (setq user-ext-elisp--scratch-minify minify)
  (unless (and (boundp 'electric-pair-mode) electric-pair-mode) ; Enable electric pair mode
    (electric-pair-local-mode t))				;
  (unless (and (boundp 'company-mode) company-mode) ; Enable company mode
    (company-mode t))				    ;
  (setq header-line-format "Type C-c C-c when finished, C-x k to cancel editing.")
  (local-set-key (kbd "C-c C-c") #'elisp-ext--scratch-buffer-ctrl-c-ctrl-c))

(defun elisp-ext--find-quote (&optional reverse)
  "Find the next quote.
If REVERSE is non-nil, search backwards."
  (save-excursion
    (if reverse
	(progn
	  (goto-char (point-max))
	  (search-backward "\"")
	  (point))
      (goto-char (point-min))
      (search-forward "\"")
      (point))))

;; --- Hs minor mode

(defsubst elisp-ext-enable-hs-minor-mode ()
  "Enable `hs-minor-mode' if it isn't already."
  (unless (and (boundp 'hs-minor-mode) hs-minor-mode)
    (hs-minor-mode 1)))

(defun elisp-ext-hide-all ()
  "Hide all toplevel forms in buffer.
Automatically activates `hs-minor-mode' when called."
  (interactive)
  (elisp-ext-enable-hs-minor-mode)
  (hs-hide-all))

(defun elisp-ext-show ()
  "Show the form at or before point."
  (interactive)
  (when (= (char-before) ?\))
    (left-char))
  (elisp-ext-enable-hs-minor-mode)
  (hs-show-block))

(defun elisp-ext-show-only ()
  "Hide every block except for the one containing point."
  (interactive)
  (when (= (char-before) ?\))
    (left-char))
  (cl-ext-save-point
    (elisp-ext-hide-all)
    (elisp-ext-show)))

(defun elisp-ext-hide-toplevel-form ()
  "Hide the function containing point.
Automatically activates `hs-minor-mode' when called."
  (interactive)
  (when (= (char-before) ?\))
    (left-char))
  (let* ((ppss (make-ppss-easy (syntax-ppss)))
	 (tl (syntax-ppss-toplevel-pos ppss))
	 p)
    (when tl
      (cl-ext-save-point
	(goto-char tl)
	(setq p tl)))
    (when p
      (goto-char p)
      (elisp-ext-enable-hs-minor-mode)
      (hs-hide-block))))

(defun elisp-ext-hide-block ()
  "Hide the innermost block containing point.
Automatically activates `hs-minor-mode' when called."
  (interactive)
  (elisp-ext-enable-hs-minor-mode)
  (hs-hide-block))

;; --- Elisp doc minor mode

(defvar elisp-ext-doc--data nil "Internal.")

(defvar-local elisp-ext-doc-type nil
  "Type of scratch buffer.
0 for docstrings, 1 for commentary.")

(defvar-local elisp-ext-doc-buffer nil
  "The original buffer from which the command was launched.")

(defvar-local elisp-ext-doc-origin nil
  "The original point in buffer.")

(defun elisp-ext-doc-scratch-buffer--setup-vars ()
  (cl-destructuring-bind (type source-buffer origin) elisp-ext-doc--data
    (cl-assert (and type source-buffer origin) t)
    (setq elisp-ext-doc-type type
	  elisp-ext-doc-buffer source-buffer
	  elisp-ext-doc-origin origin
	  elisp-ext-doc--data nil)))

(defun elisp-ext-doc-scratch-buffer--shift-return ()
  "Insert a newline and change fill column."
  (interactive)
  (newline)
  (set-fill-column 60))

(defun elisp-ext-doc-scratch-buffer--ctrl-c-ctrl-c ()
  "Kill the text inside this buffer and restore window."
  (interactive)
  (let (fail)
    (unwind-protect
	(cl-ext-progn
	  (cl-case elisp-ext-doc-type
	    (0 (let* ((start (1+ (elisp-ext--find-quote)))
		      (end (1- (elisp-ext--find-quote t))))
		 (run-with-idle-timer 0.1 nil
				      #'message "Killed text from \"%s\""
				      (buffer-substring start end))
		 (kill-region start end)))
	    (1 (let ((start (point-min))
		     (end (point-max))
		     pos
		     string)
		 (setq string
		       (cl-loop initially do
				(goto-char (point-min))
				(delete-trailing-whitespace start end)
				while (not (eobp))
				do
				(insert ";; ")
				(forward-line 1)
				finally do
				(delete-trailing-whitespace start end)
				(cl-return (buffer-string))))
		 (cl-assert elisp-ext-doc-origin)
		 (setq pos elisp-ext-doc-origin)
		 (with-current-buffer elisp-ext-doc-buffer
		   (goto-char pos)
		   (insert string))))
	    (t
	     (run-with-idle-timer
	      0.1 nil #'display-warning 'user-extensions
	      (s-lex-format "Invalid type ${elisp-ext-doc-type}") :error)
	     (setq fail t))))
      (cl-ext-unless fail (kill-buffer))
      (jump-to-register user-ext-elisp--register))))

(defun elisp-ext-doc-scratch-buffer--doc-buffer-init ()
  (emacs-lisp-mode)
  (elisp-ext-doc-scratch-buffer--setup-vars)
  (set-fill-column 67)
  (elisp-ext--enable-minor-mode auto-fill-mode)
  (elisp-ext--enable-minor-mode elisp-ext-doc-minor-mode)
  (elisp-ext--enable-minor-mode electric-pair-mode electric-pair-local-mode)
  (elisp-ext--enable-minor-mode company-mode)
  (elisp-ext--enable-minor-mode display-fill-column-indicator-mode)
  (skeleton-insert '(nil ";; Fill column is set to 67. "
			 "Type S-<return> to set it to 60.\n"
			 ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n"
			 "\"\n" _ "\n\""))
  (goto-char (1+ (elisp-ext--find-quote))))

(defun elisp-ext-doc-scratch-buffer--comment-buffer-init ()
  "Called after opening the scratch buffer for commentary."
  (text-mode)
  (elisp-ext-doc-scratch-buffer--setup-vars)
  (let (string)
    (with-current-buffer elisp-ext-doc-buffer
      (let* ((props (elisp-ext--get-commentary-block))
	     (start (plist-get props :start))
	     (end (plist-get props :end)))
	(cl-assert start)
	(cl-assert end)
	(goto-char start)
	(delete-region start end)
	(setq string (plist-get props :string))))
    (save-excursion (insert string))
    (auto-fill-mode t)
    (elisp-ext-doc-minor-mode t)
    (set-fill-column 67)
    (elisp-ext--enable-minor-mode electric-pair-mode electric-pair-local-mode)
    (elisp-ext--enable-minor-mode company-mode)
    (elisp-ext--enable-minor-mode display-fill-column-indicator-mode)
    (add-hook 'completion-at-point-functions #'elisp-completion-at-point nil t)))

(define-scratch-buffer-function elisp-ext--doc-scratch-buffer "elisp docstring"
				(type source-buffer origin)
  "Create a scratch buffer for Lisp docstrings."
  noninteractive
  (cl-check-type type integer)
  (cl-check-type source-buffer buffer)
  (setq elisp-ext-doc--data (list type source-buffer origin))
  (cl-ecase type
    ;; Documentation string buffer
    (0 (elisp-ext-doc-scratch-buffer--doc-buffer-init))
    ;; Commentary buffer
    (1 (elisp-ext-doc-scratch-buffer--comment-buffer-init)))
  (setq header-line-format "Type C-c C-c when finished."))

;;;###autoload
(defun elisp-ext-doc-scratch-buffer ()
  "Create a scratch buffer for Emacs Lisp docstrings."
  (interactive "*")
  (window-configuration-to-register user-ext-elisp--register)
  (elisp-ext--doc-scratch-buffer 0 (current-buffer) (point-marker)))

(define-minor-mode elisp-ext-doc-minor-mode
  "Minor mode for `elisp-ext--doc-scratch-buffer'.

\\{elisp-ext-doc-minor-mode-map}"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "<S-return>") #'elisp-ext-doc-scratch-buffer--shift-return)
	    (define-key map (kbd "C-c C-c") #'elisp-ext-doc-scratch-buffer--ctrl-c-ctrl-c)
	    map))

;; --- Elisp commentary scratch

(defun elisp-ext--get-comment-indent (&optional noerror)
  "Get the indent of the comment after point.
Search for a comment between point and the end of the visual
line.  Return the column of text after the comment starter.

The return value is an integer unless the search fails; in
that case return nil if NOERROR is non-nil, else raise an
error."
  (cl-ext-progn
    (comment-normalize-vars)
    (save-excursion
      (cl-ext-when (comment-search-forward (line-end-position) noerror)
	  (current-column)))))

(defun elisp-ext--get-commentary-block ()
  "Parse a commentary block.
Return a property list with the elements :string, :start,
and :end.  Parsing starts at the comment at or around
point.  Parse sucessive comment lines until a non-comment
line is reached, and join into a string.

Properties:
:start   Start of the first comment line.
:end     Position at the end of the final comment line.
:string  The body of the comment block"
  (let (pps col content pos line)
    (setq pps (make-ppss-easy (syntax-ppss))
	  pos (ppss-comment-or-string-start pps))
    (if pos
	(let (start end)
	  (setq pos (and (goto-char pos)	    ; Move to start of comment
			 (point-marker))	    ;
		col (elisp-ext--get-comment-indent) ; Get column of comment body
		start pos
		end pos
		content (cl-loop
			 with in-comment = t
			 initially do (goto-char pos)
			 while in-comment
			 collect (cl-ext-progn
				   (move-to-column col)
				   (setq line (buffer-substring (point) (line-end-position)))
				   (forward-line)
				   (setq in-comment (elisp-ext--get-comment-indent t)
					 end (1- (point)))
				   line)
			 finally do
			 (goto-char pos)))
	  (list :string (string-trim (string-join content "\n"))
		:start start :end end))
      (error "Not inside a comment"))))

;;;###autoload
(defun elisp-ext-edit-commentary ()
  "Edit the comment at point."
  (interactive "*")
  (window-configuration-to-register user-ext-elisp--register)
  (elisp-ext--doc-scratch-buffer 1 (current-buffer) (point-marker)))

;; ### Keymap

(eval-and-compile
  (define-prefix-command 'user-ext-elisp-fold-map)
  (define-key emacs-lisp-mode-map (kbd "C-c f") #'user-ext-elisp-fold-map)
  (define-key user-ext-elisp-fold-map (kbd "t") #'elisp-ext-hide-toplevel-form)
  (define-key user-ext-elisp-fold-map (kbd "s") #'elisp-ext-show)
  (define-key user-ext-elisp-fold-map (kbd "C-o") #'elisp-ext-show-only)
  (define-key user-ext-elisp-fold-map (kbd "C-a") #'elisp-ext-hide-all)
  (define-key user-ext-elisp-fold-map (kbd "b") #'elisp-ext-hide-block)

  (define-prefix-command 'user-ext-elisp-skeleton-map)
  (define-key emacs-lisp-mode-map (kbd "C-c C-s") #'user-ext-elisp-skeleton-map)
  (define-key user-ext-elisp-skeleton-map (kbd "f") #'elisp-ext-skeleton-defun)
  (define-key user-ext-elisp-skeleton-map (kbd "m") #'elisp-ext-skeleton-defmacro)

  (define-prefix-command 'user-ext-elisp-occur-map)
  (define-key emacs-lisp-mode-map (kbd "C-c C-o") #'user-ext-elisp-occur-map)
  (define-key user-ext-elisp-occur-map (kbd "f") #'elisp-ext-occur-functions)
  (define-key user-ext-elisp-occur-map (kbd "v") #'elisp-ext-occur-variables)
  (define-key user-ext-elisp-occur-map (kbd "t") #'elisp-ext-occur-types)
  (define-key user-ext-elisp-occur-map (kbd "#") #'elisp-ext-occur-sections)

  (define-key emacs-lisp-mode-map (kbd "C-c C-j") #'imenu)
  (define-key emacs-lisp-mode-map (kbd "C-c c M-s") #'elisp-ext-scratch-buffer)
  (define-key emacs-lisp-mode-map (kbd "C-c c M-d") #'elisp-ext-doc-scratch-buffer)
  (define-key emacs-lisp-mode-map (kbd "C-c c M-c") #'elisp-ext-edit-commentary)
  (define-key emacs-lisp-mode-map (kbd "C-c c b") #'emacs-lisp-byte-compile)
  (define-key emacs-lisp-mode-map (kbd "C-c c M-b") #'emacs-lisp-byte-compile-and-load)
  (define-key emacs-lisp-mode-map (kbd "C-c M-f") #'elisp-ext-minify)
  (define-key emacs-lisp-mode-map (kbd "C-c C-w") #'elisp-ext-kill-sexp)
  (define-key emacs-lisp-mode-map (kbd "C-c M-w") #'elisp-ext-copy-sexp)
  (define-key emacs-lisp-mode-map (kbd "C-c <delete>") #'elisp-ext-delete-sexp)
  (define-key emacs-lisp-mode-map (kbd "C-c ^") #'elisp-ext-jump-sexp-level)

  (define-key lisp-interaction-mode-map (kbd "C-c M-f") #'elisp-ext-minify)
  (define-key lisp-interaction-mode-map (kbd "C-c C-w") #'elisp-ext-kill-sexp)
  (define-key lisp-interaction-mode-map (kbd "C-c M-w") #'elisp-ext-copy-sexp)
  (define-key lisp-interaction-mode-map (kbd "C-c <delete>") #'elisp-ext-delete-sexp)
  (define-key lisp-interaction-mode-map (kbd "C-c ^") #'elisp-ext-jump-sexp-level)

  (define-key lisp-interaction-mode-map [remap kill-and-quit] #'quit-window))

;; ### Hook

;;;###autoload
(defun elisp-ext--extra-hook ()
  "Hook for the `emacs-lisp-mode' extension."
  (local-set-key (kbd "<") #'self-insert-command))

;;;###autoload
(add-hook 'emacs-lisp-mode-hook #'elisp-ext--extra-hook)

(provide 'elisp-ext)
;;; elisp-ext ends here

;; Local Variables:
;; eval: (local-lambda-define-self-insert-command variable-prefix "user-ext-elisp-")
;; eval: (local-lambda-define-self-insert-command function-prefix "elisp-ext-")
;; eval: (local-set-key (kbd "C-c C-c") nil)
;; End:

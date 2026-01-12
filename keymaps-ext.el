;; -*- lexical-binding: t; -*-

(check-emacs-minimum-version "27.1")

(require 'ido)
(require 's)
(require 'dash)

(eval-when-compile
  (require 'subr-x))


;; ### Customization

(defgroup keymaps-ext nil
  "Global user keymaps."
  :group 'user-extensions)


;; ### Variables

(defvar user-ext-keymaps--key-translation-table nil
  "Translation table used for `--contextual-key-func-warning'.")


;; ### Functions

(eval-after-require move-text
  (move-text-default-bindings))

(define-prefix-command 'command-map)

(define-prefix-command 'quick-command-map)
(global-set-key (kbd "C-c m") #'quick-command-map)

(cl-define-compiler-macro kbd (&whole form keys)
  (cl-ext-cond
    ((and (stringp keys)
	  (string-match-p "^[A-za-z0-9]$" keys))
     ;; Assumed to be a letter or digit
     keys)
    (t form)))


;; --- Define Contextual Keys

(defun --contextual-key-func-warning (function value)
  (cl-typecase value
    (string
     (display-warning 'user-extensions
		      (format "In %S, the argument %s is invalid"
			      function value)))
    (list (display-warning 'user-extensions
			   (format "In %S, the following arguments are invalid: %s"
				   function (string-join value ", "))))))

(defun --contextual-key-string (key)
  "Convert KEY into a string which can be interned.
KEY is a string in the format \"[MOD]K\", where MOD is a
modifier prefix and K is one of the letters a-z.

MOD, if provided, is the portion of KEY consisting of one to
three modifiers separated by hyphens. The valid modifiers
are C, M, and S. All three are optional but must appear in
that order; that is, the string \"M-C-a\" is invalid, but
\"C-M-a\" is valid.

The return value is a string describing what KEY is, and
that string is valid for `intern'.

Modifier  Translates to
--------  -------------
C         control
M         meta
S         shift"
  (unless user-ext-keymaps--key-translation-table
    (setq user-ext-keymaps--key-translation-table
	  (let ((tbl (make-hash-table)))
	    (puthash ?C "control" tbl)
	    (puthash ?M "meta" tbl)
	    (puthash ?S "shift" tbl)
	    (puthash ?\  "-" tbl)
	    tbl)))
  (let ((regex (rx string-start
		   (opt (group (opt "C-") (opt "M-") (opt "S-")))
		   (group (any (?a . ?z)))
		   string-end)))
    (or (string-match regex key)
	(signal-invalid-argument key (eval-when-compile
				       (concat "check docstring of `--contextual-key-string'"
					       " for allowed values"))))
    (let ((mods (match-string 1 key))
	  (key (match-string 2 key)))
      (when mods
	(setq mods
	      (thread-last
		  (cl-loop with repl
			   with tbl = user-ext-keymaps--key-translation-table
			   for c across mods
			   collect (cl-ext-progn
				     (setq repl (gethash c tbl))
				     (if repl repl (char-to-string c))))
		(apply #'concat))))
      (concat mods key))))

(defun --contextual-key-completion (cname)
  (let* ((choices (eval cname))
	 (arg current-prefix-arg))
    (pcase choices
      ((pred null)
       (error "%S does not have any elements" cname))
      (`(,choice)
       (list choice arg))
      (`(,_choice1 . ,_choices)
       (->
	(ido-completing-read
	 "Function: "
	 (mapcar (lambda (s) (symbol-name s))
		 (-filter #'fboundp choices))
	 nil t)
	(intern)
	(list arg)))
      (_ (error "Unreachable code reached")))))

(defmacro define-contextual-key-func (key &rest symbols)
  "Define a so-called contextual key function for key KEY.
More precisely, do two things: 1. declare customizable
variable which holds the function symbols the user can
choose from when calling the function. 2. Define an
interactive function that acts on the variable.

As a result of this macro, two things are created, a
customizable variable named user-ext-keymaps-ctxmodes-X and
a function called contextual-key-X.  In both cases, X is the
result of converting KEY into a suffix that would be a valid
symbol on its own.

In the case of the customizable variable, SYMBOLS is used to
its default value.  The variable is declared via `defcustom'
and added to the customization group `keymaps-ext'."
  (declare (debug (stringp &rest symbolp)) (indent 1))
  (cl-check-type key string)
  (dolist (symbol symbols)
    (or (or (symbolp symbol) (functionp symbol))
	(error "%S is invalid; must be a symbol" symbol)))
  (let* ((keysym (make-symbol (--contextual-key-string key)))
	 (cname (intern (format "user-ext-keymaps-ctxmodes-%S" keysym)))
	 (fname (intern (format "contextual-key-%S" keysym)))
	 (cdoc (format "List of functions allowed for the key \"%s\".
These functions are added what the user can choose from when
running `%S'." key fname))
	 (cust-fname (intern (format "customize-%S" fname)))
	 (fdoc (s-lex-format "Call CHOICE interactively with the optional argument ARG.
ARG is used as the prefix argument for CHOICE.

This should be bound to \\[quick-command-map] ${key}.

When called interactively, prompt the user to choose from
`${cname}' if it has more than one
element; if it has 1 element, then call that.  Raise an
error if `${cname}' has zero elements."))
	 (value (and symbols
		     (cons 'quote (list symbols)))))
    `(progn
       (defcustom ,cname ,value
	 ,cdoc
	 :type '(repeat symbol)
	 :group 'keymaps-ext)
       (defun ,fname (choice &optional _arg)
	 ,fdoc
	 (interactive (--contextual-key-completion ',cname))
	 (cl-check-type choice symbol)
	 (call-interactively choice)
	 (add-to-history 'extended-command-history (symbol-name choice)))
       (defun ,cust-fname ()
	 ,(format "Open the customization buffer of `%S'." cname)
	 (interactive)
	 (customize-option ',cname))
       (define-key quick-command-map ,(kbd key) (function ,fname))
       (define-key quick-command-map ,(kbd (concat "M-" key))
	 (function ,cust-fname)))))

(define-contextual-key-func "a")

(define-contextual-key-func "b")

(define-contextual-key-func "c")

(define-contextual-key-func "d")

(define-contextual-key-func "e")

(define-contextual-key-func "f")

(define-contextual-key-func "g")

(define-contextual-key-func "h")

(define-contextual-key-func "j")

(define-contextual-key-func "k")

(define-contextual-key-func "l")

(define-contextual-key-func "o")

(define-contextual-key-func "p")

(define-contextual-key-func "s")

(define-contextual-key-func "t")

(define-contextual-key-func "v")

(define-contextual-key-func "w")

(define-contextual-key-func "y")

(defun lookup-function (fn &optional keymap firstonly)
  "Lookup the key binding for FN.
If KEYMAP is non-nil, only search through that keymap.

Returns a list of keybinding descriptions unless FIRSTONLY
is non-nil."
  (let ((all-bindings
	 (where-is-internal (if (symbolp fn)
				fn
			      (cl-first fn))
			    keymap))
	keys key-bindings)
    (dolist (binding all-bindings)
      (when (and (vectorp binding)
		 (integerp (aref binding 0)))
	(push binding key-bindings)))
    (push (mapconcat #'key-description key-bindings " or ") keys)
    (if (null firstonly)
	keys
      (car keys))))


;; ### Key bindings


;; --- Global commands

(bind-keys ("C-c d" . bookmark-delete)
	   ("C-<tab>" . tab-next)		; tab commands
	   ("C-S-<iso-lefttab>" . tab-previous) ;
	   ("C-M-k" . copy-line)
	   ("C-M-n" . nonincremental-repeat-search-backward)
	   ("M-=" . count-words-region2)
	   ("M-R" . revert-buffer)
	   ("C-x C-b" . ibuffer)
	   ("C-x M-v" . view-file)	    ; find file
	   ("C-x M-f" . find-file-at-point) ;
	   ("C-x M-=" . describe-char)
	   ("C-x Q" . macro-ext-query)
	   ("C-c C-y" . yank-and-indent)
	   ;; killing buffers
	   ("C-x Z" . save-and-kill)
	   ("C-c M-q" . kill-and-quit))


;; --- Prefix command for misc commands

(bind-keys :prefix-map command-map
	   :prefix "C-c c"
	   ("%" . nonincremental-re-search-forward)
	   ("M-%" . nonincremental-re-search-backward)
	   ("M-s" . nonincremental-search-backward)
	   ("d" . copy-text-down)
	   ("c" . commit-helper)
	   ("q" . quit-window)
	   ("p" . elisp-ext-forward-or-backward-sexp)
	   ("s" . nonincremental-search-forward)
	   ("'" . save-current-position)
	   ("&" . pop-saved-position)
	   ("^" . print-saved-positions)
	   ("P" . add-file-local-variable-prop-line)
	   ("M-c" . org-capture)
	   ("M-P" . add-file-local-variable))


;; --- Space

(bind-keys ("M-\\" . delete-horizontal-space)
	   ("M-^" . delete-indentation)
	   ("M-SPC" . just-one-space)
	   ("C-x C-o" . delete-blank-lines)
	   ("C-c M-u" . upcase-insert))


;; --- Extension-related commands

(bind-keys :prefix-map extension-map
	   :prefix "C-c e"
	   ("l" . load-extension)
	   ("F" . find-extension-at-point)
	   ("h" . get-extension-documentation)
	   ("f" . find-extension))

(extension-provide 'keymaps)
;;; keymaps ends here

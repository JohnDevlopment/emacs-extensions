;; -*- lexical-binding: t; -*-

(require 'ido)
(require 's)

(eval-when-compile
  (require 'subr-x))

;; ### Customization

(defgroup keymaps-ext nil
  "Global user keymaps."
  :group 'user-extensions)

(defvar user-ext-keymaps--key-translation-table nil
  "Translation table used for `--contextual-key-func-warning'.")

;; ### Code

(eval-after-require move-text
  (move-text-default-bindings))

;; ### Prefix Commands

(define-prefix-command 'command-map)

(define-prefix-command 'quick-mode-map)
(global-set-key (kbd "C-c m") #'quick-mode-map)

;; ### Functions

(cl-define-compiler-macro kbd (&whole form keys)
  (cl-ext-cond
    ((and (stringp keys)
	  (string-match-p "^[A-za-z0-9]$" keys))
     ;; Assumed to be a letter or digit
     keys)
    (t form)))

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
  (cl-ext-unless user-ext-keymaps--key-translation-table
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
    (cl-ext-unless (string-match regex key)
      (signal-invalid-argument key (eval-when-compile
				     (concat "check docstring of `--contextual-key-string'"
					     " for allowed values"))))
    (let ((mods (match-string 1 key))
	  (key (match-string 2 key)))
      (cl-ext-when mods
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
  (let ((choices (eval cname)))
    (cl-case (length choices)
      (0 (error "`%S' is empty" cname))
      (1 choices)
      (t
       (thread-last
	   (ido-completing-read
	    "Function: " (mapcar (lambda (s) (symbol-name s)) choices)
	    nil t)
	 (intern)
	 (list))))))

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
  (declare (debug t) (indent 1))
  (cl-check-type key string)
  (dolist (symbol symbols)
    (cl-ext-unless (or (symbolp symbol) (functionp symbol))
      (error "%S is invalid; must be a symbol" symbol)))
  (let* ((keysym (make-symbol (--contextual-key-string key)))
	 (cname (intern (format "user-ext-keymaps-ctxmodes-%S" keysym)))
	 (fname (intern (format "contextual-key-%S" keysym)))
	 (cdoc (format "List of functions allowed for the key \"%s\".
These functions are added what the user can choose from when
running `%S'." key fname))
	 (fdoc (s-lex-format "Call CHOICE as an interactive function.
When called interactively, prompt the user to choose from
`${cname}' if it has more than one element; if it has 1
element, then call that.  Raise an error if `${cname}' has
zero elements."))
	 (value (cl-ext-when symbols
		    (cons 'quote (list symbols)))))
    `(progn
       (defcustom ,cname ,value
	 ,cdoc
	 :type '(repeat symbol)
	 :group 'keymaps-ext)
       (defun ,fname (choice)
	 ,fdoc
	 (interactive (--contextual-key-completion ',cname))
	 (cl-check-type choice symbol)
	 (call-interactively choice))
       (define-key quick-mode-map ,(kbd key) (function ,fname)))))

(define-contextual-key-func "a"
  abbrev-mode
  auto-fill-mode
  auto-revert-mode)

(define-contextual-key-func "b"
  basic-generic-mode
  basic-libreoffice-mode
  bind-fill-region
  bind-imenu
  bind-imenu-lsp)

(define-contextual-key-func "c"
  comment-tags-mode
  company-mode
  conf-mode)

(define-contextual-key-func "d" display-fill-column-indicator-mode)

(define-contextual-key-func "e")

(define-contextual-key-func "f" fundamental-mode)

(define-contextual-key-func "g"
  electric-pair-mode
  global-comment-tags-mode
  global-company-mode
  global-visual-line-mode
  yas-global-mode)

(define-contextual-key-func "h" hs-minor-mode)

(define-contextual-key-func "l"
  local-lambda-mode
  lsp-mode)

(define-contextual-key-func "o"
  org-ext-tbl-minor-mode
  outline-minor-mode
  outline-mode)

(define-contextual-key-func "p")

(define-contextual-key-func "s" shell-script-mode)

(define-contextual-key-func "j"
  jinja2-mode
  jit-lock-debug-mode)

(define-contextual-key-func "m" map-revert-buffer)

(define-contextual-key-func "v" activate-view-mode)

(define-contextual-key-func "w" enable-wrap)

(define-contextual-key-func "y"
  yaml-mode
  yas-minor-mode)

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

(defun map-revert-buffer ()
  "Map M-R to `revert-buffer'."
  (interactive)
  (local-set-key (kbd "M-R") #'revert-buffer)
  (message "Mapped %s `revert-buffer'"
	   (lookup-function 'revert-buffer nil t)))

;; ### Key bindings

;; --- Global commands
(bind-keys ("C-c d" . bookmark-delete)
	   ("C-<tab>" . tab-next)		; tab commands
	   ("C-S-<iso-lefttab>" . tab-previous) ;
	   ("C-M-k" . copy-line)
	   ("C-M-n" . nonincremental-repeat-search-backward)
	   ("M-=" . count-words-region2)
	   ("C-x C-b" . ibuffer)
	   ("C-x M-v" . view-file)	    ; find file
	   ("C-x M-f" . find-file-at-point) ;
	   ("C-x M-=" . describe-char)
	   ("C-x Q" . macro-ext-query)
	   ("C-x Z" . save-and-kill)	; killing buffers
	   ("C-c M-q" . kill-and-quit)	;
	   )

;; --- Prefix command for misc commands
(bind-keys :prefix-map command-map
	   :prefix "C-c c"
	   ("%" . nonincremental-re-search-forward)
	   ("M-%" . nonincremental-re-search-backward)
	   ("M-s" . nonincremental-search-backward)
	   ("c" . commit-helper)
	   ("d" . copy-text-down)
	   ;; ("g" . git-commit)
	   ("p" . elisp-ext-forward-or-backward-sexp)
	   ("s" . nonincremental-search-forward)
	   ("'" . save-current-position)
	   ("&" . pop-saved-position)
	   ("^" . print-saved-positions)
	   ("P" . add-file-local-variable-prop-line)
	   ("M-P" . add-file-local-variable))

;; --- Space
(bind-keys ("M-\\" . delete-horizontal-space)
	   ("M-^" . delete-indentation)
	   ("M-SPC" . just-one-space)
	   ("C-x C-o" . delete-blank-lines))

;; --- Extension-related commands
(bind-keys :prefix-map extension-map
	   :prefix "C-c e"
	   ("l" . load-extension)
	   ("F" . find-extension-at-point)
	   ("h" . get-extension-documentation)
	   ("f" . find-extension))

(provide 'keymaps-ext)

;;; keymaps-ext ends here

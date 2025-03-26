;; -*- lexical-binding: t; -*-

;;;### (autoloads nil "basic-ext" "basic-ext.el" (0 0 0 0))
;;; Generated autoloads from basic-ext.el

(autoload 'basic-generic-mode--extra-hook "basic-ext" nil nil nil)

(add-hook 'basic-generic-mode-mode-hook #'basic-generic-mode--extra-hook)

(autoload 'basic-libreoffice-mode "basic-ext" "\
Programming mode for Libreoffice Basic.
Derived from `basic-mode'.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "basic-ext" '("basic-ext-add-to-list-local")))

;;;***

;;;### (autoloads nil "basic-ext" "basic-ext.el" (0 0 0 0))
;;; Generated autoloads from basic-ext.el

(autoload 'basic-generic-mode--extra-hook "basic-ext" nil nil nil)

(add-hook 'basic-generic-mode-mode-hook #'basic-generic-mode--extra-hook)

(autoload 'basic-libreoffice-mode "basic-ext" "\
Programming mode for Libreoffice Basic.
Derived from `basic-mode'.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "basic-ext" '("basic-ext-add-to-list-local")))

;;;***

;;;### (autoloads nil "bbcode-ext" "bbcode-ext.el" (0 0 0 0))
;;; Generated autoloads from bbcode-ext.el

(autoload 'bbcode-ext-insert-tag "bbcode-ext" "\


\(fn STRING &optional BEG END)" t nil)

(autoload 'bbcode-extra-hook "bbcode-ext" "\
Extra hook for `bbcode-mode'." nil nil)

(add-hook 'bbcode-mode-hook #'bbcode-extra-hook)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "bbcode-ext" '("bbcode-ext-")))

;;;***

;;;### (autoloads nil "bnf-ext" "bnf-ext.el.gz" (0 0 0 0))
;;; Generated autoloads from bnf-ext.el.gz

(autoload 'bnf--extra-hook "bnf-ext" nil nil nil)

(add-hook 'bnf-mode-hook #'bnf--extra-hook)

;;;***

;;;### (autoloads nil "buffers-ext" "buffers-ext.el" (0 0 0 0))
;;; Generated autoloads from buffers-ext.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "buffers-ext" '("activate-view-mode" "clone-" "define-scratch-buffer-function" "faces-buffer" "git-commit-scratch" "kill-" "narrow-to-region2" "revert-all-buffers" "text-scratch--complete-mode" "tmpbuf" "user-ext-temp-buffers-to-kill")))

;;;***

;;;### (autoloads nil "c-ext" "c-ext.el" (0 0 0 0))
;;; Generated autoloads from c-ext.el

(autoload 'c-ext--extra-hook "c-ext" nil nil nil)

(add-hook 'c-mode-common-hook 'c-ext--extra-hook)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "c-ext" '("c-skeleton-define")))

;;;***

;;;### (autoloads nil "codeium-ext" "codeium-ext.el" (0 0 0 0))
;;; Generated autoloads from codeium-ext.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "codeium-ext" '("lookup-key" "user-ext-")))

;;;***

;;;### (autoloads nil "comment-tags-ext" "comment-tags-ext.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from comment-tags-ext.el

(autoload 'comment-tags-mode-turn-on "comment-tags-ext" "\
Turn on `comment-tags-mode' if the buffer has TODO-ish comments." t nil)

(put 'global-comment-tags-mode 'globalized-minor-mode t)

(defvar global-comment-tags-mode nil "\
Non-nil if Global Comment-Tags mode is enabled.
See the `global-comment-tags-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-comment-tags-mode'.")

(custom-autoload 'global-comment-tags-mode "comment-tags-ext" nil)

(autoload 'global-comment-tags-mode "comment-tags-ext" "\
Toggle Comment-Tags mode in all buffers.
With prefix ARG, enable Global Comment-Tags mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Comment-Tags mode is enabled in all buffers where
`comment-tags-mode-turn-on' would do it.
See `comment-tags-mode' for more information on Comment-Tags mode.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "comment-tags-ext" '("global-comment-tags-regexp" "user-ext-global-comment-tags-exclude")))

;;;***

;;;### (autoloads nil "custom-ext" "custom-ext.el" (0 0 0 0))
;;; Generated autoloads from custom-ext.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "custom-ext" '("define-face-alias" "log-view-")))

;;;***

;;;### (autoloads nil "desktop-ext" "desktop-ext.el" (0 0 0 0))
;;; Generated autoloads from desktop-ext.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "desktop-ext" '("user-ext-desktop-prefix")))

;;;***

;;;### (autoloads nil "dired-ext" "dired-ext.el" (0 0 0 0))
;;; Generated autoloads from dired-ext.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dired-ext" '("dired-")))

;;;***

;;;### (autoloads nil "dtext-ext" "dtext-ext.el" (0 0 0 0))
;;; Generated autoloads from dtext-ext.el

(autoload 'dtext-ext-shift-return "dtext-ext" "\
Handle S-<return>.
If point is inside a list, then start a new item." t nil)

(autoload 'dtext-ext-insert-ext-link "dtext-ext" "\
Insert a link to URL at point with TEXT.

If the arguments SITENAME and SEP are provided, the link
will say something like \"TEXT SEP SITENAME\".  Without, the
link will just have text.  If SEP is not provided, it
defaults to \"-\".

Interactively, prompts the user for URL, TEXT, and SITENAME.

\(fn URL TEXT &optional SITENAME SEP)" t nil)

(autoload 'dtext-ext-insert-ext-danbooru-link "dtext-ext" "\
Insert a link pointing to URL with TEXT.
This simply calls `dtext-ext-insert-ext-link' with
\"Danbooru\" as SITENAME and \"on\" as SEP.

\(fn URL TEXT)" t nil)

(autoload 'dtext--ext-hook "dtext-ext" "\
Extra hook for `dtext-mode'." nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dtext-ext" '("dtext-ext-list-regexp")))

;;;***

;;;### (autoloads nil "elisp-ext" "elisp-ext.el" (0 0 0 0))
;;; Generated autoloads from elisp-ext.el

(autoload 'elisp-ext-forward-or-backward-sexp "elisp-ext" "\
Go to the matching parenthesis to the one is adjacent at point.
With ARG, do it that many times.  A negative arg -N reverses
the direction of the motion.

\(fn &optional ARG)" t nil)

(autoload 'elisp-ext-update-loadefs "elisp-ext" "\
Update autoload definitions of Lisp extensions.

Updates the autoload definitions in the Lisp files in
`user-ext-extension-directory'.  Binds
`generated-autoload-file' to the concatenation of
`user-ext-extension-directory' and \"loaddefs-ext.el\".

\(fn &optional INTERACTIVE-P)" t nil)
 (autoload 'elisp-ext-scratch-buffer "elisp-ext" "Create a scratch buffer for Emacs lisp code." t)

(autoload 'elisp-ext-doc-scratch-buffer "elisp-ext" "\
Create a scratch buffer for Emacs Lisp docstrings." t nil)

(autoload 'elisp-ext--extra-hook "elisp-ext" "\
Hook for the `emacs-lisp-mode' extension." nil nil)

(add-hook 'emacs-lisp-mode-hook #'elisp-ext--extra-hook)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "elisp-ext" '("elisp-" "emacs-lisp-mode-abbrev-table" "occur-cross-reference" "user-ext-elisp-")))

;;;***

;;;### (autoloads nil "extensions" "extensions.el" (0 0 0 0))
;;; Generated autoloads from extensions.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "extensions" '("--extension-c" "eval-after-require" "find-extension" "get-extension-documentation" "load-extension" "user-ext-extension-directory")))

;;;***

;;;### (autoloads nil "general" "general.el" (0 0 0 0))
;;; Generated autoloads from general.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "general" '("bind-fill-region" "capf-enable-ispell-locally" "date-format-version" "describe-region" "enable-wrap" "invalid-argument" "kill-and-quit" "minify" "pop-saved-position" "print-saved-positions" "save-" "signal-wrong-argument" "sort-words" "user-ext-local-position-ring" "wrong-argument")))

;;;***

;;;### (autoloads nil "highlight-ext" "highlight-ext.el" (0 0 0 0))
;;; Generated autoloads from highlight-ext.el

(autoload 'highlight-ext-region "highlight-ext" "\


\(fn BEG END FACE)" t nil)

(autoload 'highlight-ext-remove-all-highlights "highlight-ext" "\
docstring" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "highlight-ext" '("highlight-ext--init" "user-ext-highlight-")))

;;;***

;;;### (autoloads nil "html-ext" "html-ext.el" (0 0 0 0))
;;; Generated autoloads from html-ext.el

(autoload 'mhtml-ext-insert-entity "html-ext" "\
Insert an entity symbol called NAME.

\(fn NAME)" t nil)

(autoload 'mhtml--extra-hook "html-ext" nil nil nil)

(add-hook 'mhtml-mode-hook #'mhtml--extra-hook)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "html-ext" '("mhtml-ext--entities" "user-ext-mhtml-entities")))

;;;***

;;;### (autoloads nil "ibuffer-ext" "ibuffer-ext.el" (0 0 0 0))
;;; Generated autoloads from ibuffer-ext.el

(autoload 'ibuffer-ext-toggle-current-filter-group "ibuffer-ext" "\
Expand/collapse the filter group of point.
If point is on a buffer, toggle the group it is in." t nil)

(autoload 'ibuffer--extra-hook "ibuffer-ext" nil nil nil)

(add-hook 'ibuffer-mode-hook #'ibuffer--extra-hook)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ibuffer-ext" '("ibuffer-ext--after-operation")))

;;;***

;;;### (autoloads nil "imenu-ext" "imenu-ext.el" (0 0 0 0))
;;; Generated autoloads from imenu-ext.el

(autoload 'bind-imenu "imenu-ext" "\
Binds `imenu' to the right-mouse button locally.
The bindings are local to the active keymap, which means
buffers sharing the same major mode will be affected." t nil)

(autoload 'bind-imenu-lsp "imenu-ext" "\
Binds `imenu' to the double left-click mouse button locally.
The bindings are local to the active keymap, which means
buffers sharing the same major mode will be affected." t nil)

(autoload 'imenu--python-hook "imenu-ext" "\
Imenu Hook for Python mode." nil nil)

(add-hook 'python-mode-hook #'imenu--python-hook)

;;;***

;;;### (autoloads nil "jinja2" "jinja2.el" (0 0 0 0))
;;; Generated autoloads from jinja2.el

(autoload 'jinja2-mode "jinja2" "\
Minor mode for editing Jinja2 templates within major modes.

If called interactively, enable Jinja2 mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

\\{jinja2-mode-map}

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jinja2" '("jinja2-" "tag-match-")))

;;;***

;;;### (autoloads nil "js-ext" "js-ext.el" (0 0 0 0))
;;; Generated autoloads from js-ext.el

(autoload 'js2--extra-hook "js-ext" nil nil nil)

(add-hook 'js2-mode-hook #'js2--extra-hook)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "js-ext" '("user-ext-js-indent-")))

;;;***

;;;### (autoloads nil "keymaps-ext" "keymaps-ext.el" (0 0 0 0))
;;; Generated autoloads from keymaps-ext.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "keymaps-ext" '("--contextual-key-func-warning" "define-contextual-key-func" "lookup-function" "map-revert-buffer")))

;;;***

;;;### (autoloads nil "liquidsoap-ext" "liquidsoap-ext.el.gz" (0
;;;;;;  0 0 0))
;;; Generated autoloads from liquidsoap-ext.el.gz

(autoload 'liquidsoap-ext-get-doc "liquidsoap-ext" "\
Display a help window for the Liquidsoap COMMAND.

Interactively, the user is prompted for a Liquidsoap
command, with completion.

\(fn COMMAND)" t nil)

(autoload 'liquidsoap-ext--hook "liquidsoap-ext" "\
Extra hook for `liquidsoap-mode'." nil nil)

(add-hook 'liquidsoap-mode-hook #'liquidsoap-ext--hook)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "liquidsoap-ext" '("liquid" "user-ext-liquidsoap-ext-")))

;;;***

;;;### (autoloads nil "local-lambda" "local-lambda.el" (0 0 0 0))
;;; Generated autoloads from local-lambda.el

(autoload 'local-lambda-get "local-lambda" "\
Return the lambda with the name NAME, or nil if it doesn't exist.
NAME must be a symbol.

\(fn NAME)" nil nil)

(autoload 'local-lambda-add-local-lambda "local-lambda" "\
Add a local function FUNCTION under key KEY.
FUNCTION is saved to `local-lambda-lambdas' under
KEY, which is a symbol.  FUNCTION is a `lambda' expression.

\(fn KEY FUNCTION &optional OVERWRITE)" nil nil)

(autoload 'local-lambda-run-local-lambda "local-lambda" "\
Run the local function under key KEY.
KEY must be have previously been added via one of the other
functions..

Interactively, KEY is prompted from the user with completion.

\(fn KEY)" t nil)

(autoload 'local-lambda-define-local-defun "local-lambda" "\
Define NAME as a buffer-local function with ARGLIST.
ARGLIST is an argument list for a `defun', and BODY is a list
of forms to add to it.

As a result of this macro, NAME can be run with
`local-lambda-run-local-lambda'.

\(fn NAME ARGS [DOCSTRING] [INTERACTIVE] BODY)" nil t)

(function-put 'local-lambda-define-local-defun 'doc-string-elt '3)

(function-put 'local-lambda-define-local-defun 'lisp-indent-function '2)

(autoload 'local-lambda-define-skeleton "local-lambda" "\
Define NAME as a buffer-local skeleton command.
SKELETON works the same way as the SKELETON argument in
`define-skeleton', which see.

\(fn NAME DOCSTRING &rest SKELETON)" nil t)

(function-put 'local-lambda-define-skeleton 'lisp-indent-function '1)

(function-put 'local-lambda-define-skeleton 'doc-string-elt '2)

(autoload 'local-lambda-define-self-insert-command "local-lambda" "\
Define a buffer-local command NAME to insert STRING into buffer.
By default, `company-complete' is called as a command after
inserting STRING, unless NO-COMPLETE is non-nil, in which
case it is not.

\(fn NAME STRING &optional NO-COMPLETE)" nil t)

(function-put 'local-lambda-define-self-insert-command 'lisp-indent-function '1)

(autoload 'local-lambda-mode "local-lambda" "\
A minor mode for running buffer-local functions.

If called interactively, enable Local-Lambda mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\\{local-lambda-mode-map}

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "local-lambda" '("local-lambda-")))

;;;***

;;;### (autoloads nil "log4j-ext" "log4j-ext.el" (0 0 0 0))
;;; Generated autoloads from log4j-ext.el

(autoload 'log4j-ext-start-filter "log4j-ext" "\
Turn filtering on in the current log file buffer.
When used interactively, the user enters INCLUDE-STRING and
EXCLUDE-STRING, which should be strings of filter keywords,
separated by spaces.

This calls `log4j-start-filter' with the given args.

\(fn INCLUDE-STRING EXCLUDE-STRING)" t nil)

(autoload 'log4j-ext-stop-filter "log4j-ext" "\
Turn filtering off in the current log file buffer." t nil)

(autoload 'log4j--extra-hook "log4j-ext" "\
Extra hook for `log4j-mode'." nil nil)

(add-hook 'log4j-mode-hook #'log4j--extra-hook)

(add-to-list 'auto-mode-alist '("\\.log\\.[1-9]+\\'" . log4j-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "log4j-ext" '("user-ext-log4j-filtering")))

;;;***

;;;### (autoloads nil "lsp-ext" "lsp-ext.el" (0 0 0 0))
;;; Generated autoloads from lsp-ext.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-ext" '("kill-lsp-buffers" "lsp-" "user-ext-lsp-")))

;;;***

;;;### (autoloads nil "lua-ext" "lua-ext.el" (0 0 0 0))
;;; Generated autoloads from lua-ext.el

(autoload 'lua-add-require "lua-ext" "\
Inserts a require statement at point. MODNAME is the name of a Lua module.

\(fn MODNAME)" t nil)

(autoload 'lua--extra-mode-hook "lua-ext" nil nil nil)

(add-hook 'lua-mode-hook #'lua--extra-mode-hook)

;;;***

;;;### (autoloads nil "macro-ext" "macro-ext.el" (0 0 0 0))
;;; Generated autoloads from macro-ext.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "macro-ext" '("macro-ext-query")))

;;;***

;;;### (autoloads nil "markdown-ext" "markdown-ext.el" (0 0 0 0))
;;; Generated autoloads from markdown-ext.el
 (autoload 'markdown-skeleton-id "markdown-ext" "Insert a link with an id field." t)

(autoload 'markdown-ext-insert-footnote "markdown-ext" "\
Insert a footnote with a new number and move point to footnote definition." t nil)

(autoload 'markdown-ext-set-footnote-counter "markdown-ext" "\
Set the footnote counter to NUM.

\(fn NUM)" t nil)

(autoload 'markdown-ext-insert-image "markdown-ext" "\


\(fn FILE ALT &optional TITLE PREFIX)" t nil)
 (autoload 'markdown-ext-scratch "markdown-ext" "Open a scratch buffer to edit markdown." t)

(autoload 'markdown--extra-hook "markdown-ext" "\
Hook for `markdown-mode' extension." nil nil)

(add-hook 'markdown-mode-hook #'markdown--extra-hook)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "markdown-ext" '("markdown-" "orgtbl-to-markdown")))

;;;***

;;;### (autoloads nil "nroff-ext" "nroff-ext.el" (0 0 0 0))
;;; Generated autoloads from nroff-ext.el

(add-to-list 'auto-mode-alist '("\\.[13]p\\'" . nroff-mode))

;;;***

;;;### (autoloads nil "org-ext" "org-ext.el" (0 0 0 0))
;;; Generated autoloads from org-ext.el

(autoload 'browse-url-brave "org-ext" "\
Browse URL in Brave browser.
_NEW-WINDOW is ignored.

\(fn URL &optional NEW-WINDOW)" t nil)
 (autoload 'org-ext-scratch "org-ext" "Create an `org-mode' scratch buffer." t)

(autoload 'org-ext-tbl-minor-mode "org-ext" "\
Turn on `orgtbl-mode'.
ARG is passed to the function.

\(fn &optional ARG)" t nil)

(autoload 'org--extra-hook "org-ext" "\
Extra hook for `org-mode'." nil nil)

(add-hook 'org-mode-hook #'org--extra-hook)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-ext" '("org-ext-" "user-ext-browse-url-brave-arguments")))

;;;***

;;;### (autoloads nil "origami-ext" "origami-ext.el" (0 0 0 0))
;;; Generated autoloads from origami-ext.el

(autoload 'origami--extra-hook "origami-ext" nil nil nil)

(add-hook 'origami-mode-hook #'origami--extra-hook)

;;;***

;;;### (autoloads nil "outline-ext" "outline-ext.el" (0 0 0 0))
;;; Generated autoloads from outline-ext.el

(autoload 'outline--extra-hook "outline-ext" nil nil nil)

(autoload 'outline-minor-mode--extra-hook "outline-ext" nil nil nil)

(add-hook 'outline-minor-mode-hook #'outline-minor-mode--extra-hook)

(add-hook 'outline-mode-hook #'outline--extra-hook)

;;;***

;;;### (autoloads nil "packages/alist-ext" "packages/alist-ext.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from packages/alist-ext.el

(autoload 'alist-ext-from-list "packages/alist-ext" "\
Turn a regular list LIST into an alist.
LIST must have an even number of elements.

\(fn LIST)" nil nil)

(function-put 'alist-ext-from-list 'pure 't)

(autoload 'alist-ext-define "packages/alist-ext" "\
Construct an alist with PAIRS, where each key precedes its value.

Each key can be any valid lisp object, but symbols have to
be quoted.

\(fn KEY VALUE ...)" nil t)

;;;***

;;;### (autoloads nil "packages/cl-ext" "packages/cl-ext.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from packages/cl-ext.el

(autoload 'cl-ext-when "packages/cl-ext" "\
If COND yields non-nil, do FIRST-FORM and BODY, else return nil.
When COND yields non-nil, eval FIRST-FORM and BODY forms
sequentially and return value of last one.

The main difference between `cl-ext-when' and `when' is that
when BODY is empty, this expands to a `and' form; otherwise,
it behaves exactly the same.

When BODY is nil, if COND is itself a `and' form, ala `(and
SUB-CONDS...)', SUB-CONDS is collapsed into COND.  As a
result, the following form

   (cl-ext-when (and n (> n 0))
     n)

expands to this:

   (and n (> n 0) n)

\(fn COND FIRST-FORM BODY...)" nil t)

(function-put 'cl-ext-when 'lisp-indent-function '1)

(autoload 'cl-ext-unless "packages/cl-ext" "\
If COND yields nil, do FIRST-FORM and BODY, else return nil.
When COND yields nil, eval FIRST-FORM and BODY forms
sequentially and return value of last one.

The main difference between `cl-ext-unless' and `unless' is
that when BODY is empty, this expands to a `or' form;
otherwise, it behaves exactly the same.

\(fn COND FIRST-FORM BODY...)" nil t)

(function-put 'cl-ext-unless 'lisp-indent-function '1)

(autoload 'cl-ext-append "packages/cl-ext" "\
Add X to the list stored in PLACE.

\(fn X PLACE)" nil t)

(autoload 'cl-ext-append-list "packages/cl-ext" "\
Add X to the list stored in PLACE, but wrap X in a list.
X is supposed to be a list, likely representing a Lisp
expression; it's added to PLACE in such a way that it isn't
destructured (see `append').

Let's use an example.  Supposed you're building a list of
Lisp expressions and adding another list to it:
   (let ((body '(\"Docstring.\")))
     (cl-append-list (interactive) body))
   => (\"Docstring\" (interactive))

\(fn X PLACE)" nil t)

(function-put 'cl-ext-append-list 'pure 't)

(autoload 'cl-ext-nconcat "packages/cl-ext" "\
Append the arguments (SEQUENCES) as strings to PLACE.
the string found at PLACE and SEQUENCES are combined via
`concat' and then set as the new value of PLACE.

\(fn PLACE &rest SEQUENCES)" nil t)

(autoload 'cl-ext-save-point "packages/cl-ext" "\
Execute BODY and restore point to its original position.
Any errors are caught and printed as simple messages.

\(fn BODY...)" nil t)

(function-put 'cl-ext-save-point 'lisp-indent-function '0)

(autoload 'cl-ext-progn "packages/cl-ext" "\
Eval BODY forms sequentially and return value of last one.

This expansion changes to different things depending on how
many elements BODY has: if 0, this expands to a single call
to `(ignore)'; if 1, to just that element; if 2 or greater,
this behaves exactly like `progn'.

\(fn BODY...)" nil t)

(function-put 'cl-ext-progn 'lisp-indent-function '0)

;;;***

;;;### (autoloads nil "packages/debug-ext" "packages/debug-ext.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from packages/debug-ext.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "packages/debug-ext" '("--" "assert" "debug-ext-get-function-body")))

;;;***

;;;### (autoloads nil "packages/embed-doc" "packages/embed-doc.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from packages/embed-doc.el

(defsubst embed-doc-get-documentation (symbol) (cl-check-type symbol symbol) (let ((doc (get symbol embed-doc-prop))) (when doc (substitute-command-keys doc))))

(autoload 'embed-doc-document-symbol "packages/embed-doc" "\
Embed documentation in SYMBOL, starting with PREAMBLE.
PREAMBLE is the first part of the documentation.

  (embed-doc-document-symbol symbol
    [:keyword [option]]...)

:commands    Document commands (i.e., interactive functions).
:customs     Document user options (i.e., variables defined
             with `defcustom').
:faces       Document faces (i.e., defined with `defface').
:functions   Document functions.
:variables   Document variables.

It does not replace any existing documentation, so functions,
variables, and faces.

\(fn SYMBOL PREAMBLE ARG...)" nil t)

(function-put 'embed-doc-document-symbol 'lisp-indent-function '1)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "packages/embed-doc" '("embed-doc-")))

;;;***

;;;### (autoloads nil "packages/function-ext" "packages/function-ext.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from packages/function-ext.el

(autoload 'fext-defalias "packages/function-ext" "\
Define SYMBOL's definition to DEFINITION.

\(fn SYMBOL DEFINITION &optional DOCSTRING)" nil t)

(function-put 'fext-defalias 'doc-string-elt '3)

(autoload 'fext-replace-function "packages/function-ext" "\
Replace the function definition of SYMBOL.
The new definition is
\(lambda SYMBOL ARGLIST [DOCSTRING] BODY...).  The arguments
are the same as `defun', which see.

This function has two side effects: on the first invocation
of this macro for SYMBOL, its original definition is saved
to SYMBOL--old.  Second, SYMBOL is declared as a function
via `declare-function'.  This is just to placate the byte-
compiler.

\(fn SYMBOL FILE ARGLIST [DOCSTRING] [DECL] BODY...)" nil t)

(function-put 'fext-replace-function 'lisp-indent-function '3)

(function-put 'fext-replace-function 'doc-string-elt '4)

(autoload 'fext-defadvice "packages/function-ext" "\
Define a piece of advice for FUNCTION (a symbol).
The syntax of `fext-defadvice' is as follows:

  (fext-defadvice FUNCTION (CLASS NAME [ARGLIST])
    [KEYWORD-ARGS]
    [DOCSTRING] [INTERACTIVE-FORM]
    BODY...)

FUNCTION ::= Name of the function being advised.
CLASS ::= `before' | `before-while' `before-until' |
    `around' | `after' | `after-while' |  `after-until' |
    `activation' | `deactivation' | `override' |
    `filter-args' | `filter-return'
ARGLIST ::= Argument list for the advice.  Without it,
    FUNCTION's argument list will consist of
    `(&optional _args)'
DOCSTRING ::= Optional documentation string for defined
    function.
INTERACTIVE-FORM ::= Optional interactive form.
BODY ::= Any s-expression.

For CLASS, see the WHERE argument of `add-function'.

Keyword arguments can be provided at the beginning of BODY.
The following keywords are supported:

:remove FORM    If FORM evaluates to non-nil, the advice is
                removed from FUNCTION.  In this case, the
                rest of BODY is not evaluated.  CLASS and
                NAME are used to specify what advice is
                removed.

\(fn FUNCTION (CLASS NAME [ARGLIST]) BODY...)" nil t)

(function-put 'fext-defadvice 'lisp-indent-function '2)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "packages/function-ext" '("user-ext-fext-valid-advice-classes")))

;;;***

;;;### (autoloads nil "packages/menu-ext" "packages/menu-ext.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from packages/menu-ext.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "packages/menu-ext" '("define-menu")))

;;;***

;;;### (autoloads nil "packages/server-view-ext" "packages/server-view-ext.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from packages/server-view-ext.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "packages/server-view-ext" '("server-ext-")))

;;;***

;;;### (autoloads nil "perl-ext" "perl-ext.el" (0 0 0 0))
;;; Generated autoloads from perl-ext.el

(autoload 'perl-mode--extra-hook "perl-ext" nil nil nil)
 (autoload 'perl-ext-scratch-buffer "perl-ext" "Create a Perl scratch buffer." t)

(add-hook 'perl-mode-hook #'perl-mode--extra-hook)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "perl-ext" '("perl-ext-scratch-buffer")))

;;;***

;;;### (autoloads nil "python-ext" "python-ext.el" (0 0 0 0))
;;; Generated autoloads from python-ext.el
 (autoload 'python-ext-forward-def "python-ext" nil t)
 (autoload 'python-ext-forward-class "python-ext" nil t)
 (autoload 'python-ext-backward-def "python-ext" nil t)
 (autoload 'python-ext-backward-class "python-ext" nil t)
 (autoload 'py-hide-base "python-ext")

(autoload 'python-ext-show "python-ext" "\
Toggle visibility of existing forms at point." t nil)

(autoload 'python-ext-pydoc "python-ext" "\


\(fn WHAT)" t nil)

(autoload 'python-ext-finish-variable-type "python-ext" "\
Finish the type of the variable at point.

In order for this to work, the current buffer must be using
LSP and the underlying server must support inlay hints.  To
see if that is available, call \\[lsp-describe-session] and
look for `inlayHintProvider'." t nil)
 (autoload 'python-ext-scratch "python-ext" "Opens a scratch buffer to let you write Python code." t)

(autoload 'python-ext-kill-pyi-buffers "python-ext" nil t nil)

(autoload 'python-ext-kill-venv-buffers "python-ext" nil t nil)

(autoload 'python-ext-docstring "python-ext" "\
Open a temporary buffer to write a docstring." t nil)

(autoload 'python--extra-hook "python-ext" "\
Hook for `python-mode' for this extension." nil nil)

(autoload 'python--lsp-hook "python-ext" "\
Hook for `python-mode' when lsp is enabled." nil nil)

(add-hook 'python-mode-hook #'python--extra-hook)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "python-ext" '("python-" "user-ext-python-")))

;;;***

;;;### (autoloads nil "python-sphinx-ext" "python-sphinx-ext.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from python-sphinx-ext.el

(autoload 'sphinx-doc-mode--extra-hook "python-sphinx-ext" "\
Hook for `sphinx-doc-mode'." nil nil)

(add-hook 'sphinx-doc-mode-hook #'sphinx-doc-mode--extra-hook)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "python-sphinx-ext" '("sphinx-ext-")))

;;;***

;;;### (autoloads nil "rst-ext" "rst-ext.el" (0 0 0 0))
;;; Generated autoloads from rst-ext.el

(autoload 'rst-mode--extra-hook "rst-ext" "\
Extra hook for `rst-mode'." nil nil)

(add-hook 'rst-mode-hook #'rst-mode--extra-hook)

;;;***

;;;### (autoloads nil "rust-ext" "rust-ext.el" (0 0 0 0))
;;; Generated autoloads from rust-ext.el

(autoload 'rust-ext-cargo-run-with-args "rust-ext" "\
Run this project via 'cargo run'.
This calls `rustic-cargo-run' with a non-nil argument." t nil)

(autoload 'rust-ext-docstring "rust-ext" "\
Open a temporary buffer to write a docstring." t nil)

(autoload 'rust-ext-customize-group "rust-ext" "\
Customize the group for `rusti-mode'." t nil)
 (autoload 'rust-ext-scratch-buffer "rust-ext" "Open a scratch buffer in `rustic-mode'." t)

(autoload 'rust--extra-hook "rust-ext" "\
Hook for `rustic-mode' extension." nil nil)

(add-hook 'rust-mode-hook #'rust--extra-hook)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rust-ext" '("rust-" "user-ext-rust-")))

;;;***

;;;### (autoloads nil "sh-ext" "sh-ext.el" (0 0 0 0))
;;; Generated autoloads from sh-ext.el

(autoload 'sh--extra-hook "sh-ext" nil nil nil)

(add-hook 'sh-mode-hook #'sh--extra-hook)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sh-ext" '("global-abbrev-table" "sh-" "user-ext-sh-")))

;;;***

;;;### (autoloads nil "syntax-ext" "syntax-ext.el.gz" (0 0 0 0))
;;; Generated autoloads from syntax-ext.el.gz

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "syntax-ext" '("make-ppss-easy")))

;;;***

;;;### (autoloads nil "tcl-ext" "tcl-ext.el.gz" (0 0 0 0))
;;; Generated autoloads from tcl-ext.el.gz

(autoload 'tcl--extra-hook "tcl-ext" "\
Extra hook for `tcl mode'." nil nil)

(add-hook 'tcl-mode-hook #'tcl--extra-hook)

;;;***

;;;### (autoloads nil "text-ext" "text-ext.el.gz" (0 0 0 0))
;;; Generated autoloads from text-ext.el.gz

(autoload 'text-ext-highlight-even-lines "text-ext" "\
Highlight every even-numbered line in the current buffer." t nil)

;;;***

;;;### (autoloads nil "toml-ext" "toml-ext.el.gz" (0 0 0 0))
;;; Generated autoloads from toml-ext.el.gz

(autoload 'toml-mode--extra-hook "toml-ext" "\
Extra hook for `toml-mode'." nil nil)

(add-hook 'toml-mode-hook #'toml-mode--extra-hook)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "toml-ext" '("toml-ext-indent")))

;;;***

;;;### (autoloads nil "wrap-mode" "wrap-mode.el" (0 0 0 0))
;;; Generated autoloads from wrap-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "wrap-mode" '("global-visual-line-mode" "user-ext-visual-line-global-modes" "visual-line-mode-turn-on")))

;;;***

;;;### (autoloads nil "yasnippets-ext" "yasnippets-ext.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from yasnippets-ext.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "yasnippets-ext" '("yas-ext-")))

;;;***

;;;### (autoloads nil nil ("code-outline-bootstrap.el" "faces-ext.el"
;;;;;;  "help-ext.el" "jdesktop-bootstrap.el" "liquidsoap-bootstrap.el.gz"
;;;;;;  "packages/documentation-ext.el") (0 0 0 0))

;;;***

;;;### (autoloads nil "abbrev-ext" "abbrev-ext.el" (0 0 0 0))
;;; Generated autoloads from abbrev-ext.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "abbrev-ext" '("abbrev-ext-insert-hook" "global-abbrev-table" "user-ext-abbrev-insert-char-regex")))

;;;***

(provide 'loaddefs-ext)
;;; loaddefs-ext.el ends here

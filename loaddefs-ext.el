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

;;;### (autoloads nil "_buffers-ext" "_buffers-ext.el" (0 0 0 0))
;;; Generated autoloads from _buffers-ext.el

(autoload 'define-scratch-buffer-function "_buffers-ext" "\
Define a function NAME for creating a scratch buffer.
The scratch buffer is named \"BUFFER-NAME\".  NAME has
ARG-LIST and DOCSTRING as its argument list and
documentation string, respectively.  Likewise, INT-SPEC is
used as the argument to `interactive' if non-nil.  The rest
of the arguments is the BODY of function NAME.

BODY is evaluated with the scratch buffer as the current one.
The last form of BODY is returned by the defined function.

\(fn NAME BUFFER-NAME ARG-LIST DOCSTRING INT-SPEC &rest BODY)" nil t)

(function-put 'define-scratch-buffer-function 'lisp-indent-function '3)

(function-put 'define-scratch-buffer-function 'doc-string-elt '4)
 (autoload 'faces-buffer "_buffers-ext" "Open a buffer listing all the faces.\n\n\(fn)" t)
 (autoload 'general-scratch "_buffers-ext" "Open a general-purpose scratch buffer.\n\n\(fn MODE)" t)

(autoload 'tmpbuf "_buffers-ext" "\
Open a temporary buffer with the name BUFFER-NAME.
Create the buffer if it does not already exist.  If SWITCH is
non-nil, switch to the newly buffer.

If this is called interactively, switch to the buffer.

\(fn BUFFER-NAME &optional SWITCH)" t nil)

(autoload 'kill-certain-temp-buffers "_buffers-ext" "\
Convenience function to kill certain buffers you do not need.

This kills buffers belonging to `user-ext-temp-buffers-to-kill' and
`user-ext-temp-buffers-to-kill-regex'." t nil)

(autoload 'kill-customization-buffers "_buffers-ext" "\
Close all customization buffers.
Internally, calls `kill-buffers' with \"^*Customize.*\" as the pattern." t nil)

(autoload 'kill-buffers "_buffers-ext" "\
Close all buffers matching PATTERN.
If PREDICATE is specified, it is a function that accepts a
buffer object and returns a non-nil value if said buffer
should be killed.

Called interactively, PREDICATE cannot be specified.

\(fn PATTERN &optional PREDICATE)" t nil)

(autoload 'clone-indirect-buffer-this-window "_buffers-ext" "\
Create an indirect buffer that is a twin copy of the current buffer." t nil)

(autoload 'revert-all-buffers "_buffers-ext" "\
Reverts all buffers, including special buffers.
This reverts all buffers in like manner to `revert-buffer'.  The user is NOT
asked to confirm, so be careful when using this function.  IGNORE-AUTO and
PREVERSE-MODES are the same as for `revert-buffer', and they are specified
as prefix args." t nil)

(autoload 'activate-view-mode "_buffers-ext" "\
Activate view mode in current buffer and maybe setup exit actions.
If ARG is non-nil, setup buffer to be killed when View mode
is exited, similar to the behavior of `view-buffer' or
`view-file'.

Interactively, ARG is the raw prefix argument.

\(fn &optional ARG)" t nil)

(autoload 'clone-and-view-buffer "_buffers-ext" "\
Create an indirect buffer and show it in another window.

Creates an indirect buffer of BASE-BUFFER and shows it in
another window.  BASE-BUFFER should be a live buffer or the
name of an existing buffer.  If the optional arg CLONE is
non-nil, BASE-BUFFER's state is preserved in the indirect
buffer, meaning things like minor and major modes.  Otherwise,
the indirect buffer's state is reset to default values.

Interactively, CLONE is a prefix argument.

\(fn BASE-BUFFER &optional CLONE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "_buffers-ext" '("faces-buffer" "general-scratch" "text-scratch--complete-mode")))

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

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "bnf-ext" '("user-ext-bnf--init")))

;;;***

;;;### (autoloads nil "bookmark-menu-ext" "bookmark-menu-ext.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from bookmark-menu-ext.el

(autoload 'bookmark-bmenu--extra-hook "bookmark-menu-ext" nil nil nil)

(add-hook 'bookmark-bmenu-mode-hook #'bookmark-bmenu--extra-hook)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "bookmark-menu-ext" '("bookmark-bmenu-ext-this-window-alternate")))

;;;***

;;;### (autoloads nil "buffers-ext" "buffers-ext.el" (0 0 0 0))
;;; Generated autoloads from buffers-ext.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "buffers-ext" '("set-current-window-dedicated" "user-ext-temp-buffers-to-kill" "with-tmpbuf")))

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

(autoload 'comment-tags-ext-remove-comment "comment-tags-ext" nil t nil)

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

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "comment-tags-ext" '("user-ext-global-comment-tags-")))

;;;***

;;;### (autoloads nil "commit-helper" "commit-helper.el" (0 0 0 0))
;;; Generated autoloads from commit-helper.el
 (autoload 'commit-helper "commit-helper" nil t)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "commit-helper" '("commit-helper-")))

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

;;;### (autoloads nil "ebnf-ext" "ebnf-ext.el" (0 0 0 0))
;;; Generated autoloads from ebnf-ext.el

(autoload 'ebnf--extra-hook "ebnf-ext" nil nil nil)

(add-hook 'ebnf-mode-hook #'ebnf--extra-hook)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ebnf-ext" '("user-ext-ebnf--init")))

;;;***

;;;### (autoloads nil "elisp-ext" "elisp-ext.el" (0 0 0 0))
;;; Generated autoloads from elisp-ext.el

(autoload 'elisp-ext-minify "elisp-ext" "\
Minify the code between START and END in current buffer.
START and END are the two points in a region.  If the region
is not active, minify the whole buffer, asking the user
beforehand; unless FORCE is non-nil, in which, do it without
asking.

If called interactively, START and END are the region,
provided the region is active, otherwise they are ignored.
FORCE is the prefix argument.

\(fn START END &optional FORCE)" t nil)

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
Create and switch to a scratch buffer for Emacs Lisp docstrings.
When the buffer is opened, the cursor is placed between two
double quotation characters, each on its own line.  The
lines between the two quotations is used for the docstring.

Auto fill mode is enabled when the buffer is first opened.
Initially the fill column is set to 67, but when the user
types \\[elisp-ext-doc-scratch-buffer--shift-return], a
newline is added and the fill column is changed to 60.  This
reflects the recommended line length as stated in Emacs'
official style guide.

When you are finished writing the docstring and want to exit,
type \\[elisp-ext-doc-scratch-buffer--ctrl-c-ctrl-c].  Doing
so does the following, in order:
1. Kills (cuts) the text inbetween the quotation characters.
2. Kills the buffer.
3. Restores the previous window configuration (see below).

The window configuration is saved when this command is run.
As such, you exit this buffer in the way prescribed above,
it is restored." t nil)

(autoload 'elisp-ext-edit-commentary "elisp-ext" "\
Edit the comment at point." t nil)

(autoload 'elisp-ext--extra-hook "elisp-ext" "\
Hook for the `emacs-lisp-mode' extension." nil nil)

(add-hook 'emacs-lisp-mode-hook #'elisp-ext--extra-hook)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "elisp-ext" '("elisp-" "emacs-lisp-mode-abbrev-table" "occur-cross-reference" "user-ext-elisp-")))

;;;***

;;;### (autoloads nil "errors" "errors.el" (0 0 0 0))
;;; Generated autoloads from errors.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "errors" '("invalid-argument" "signal-" "type-error" "wrong-argument")))

;;;***

;;;### (autoloads nil "extensions" "extensions.el" (0 0 0 0))
;;; Generated autoloads from extensions.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "extensions" '("--" "eval-after-require" "find-extension" "get-extension-documentation" "load-extension" "user-ext-extension-directory")))

;;;***

;;;### (autoloads nil "general" "general.el" (0 0 0 0))
;;; Generated autoloads from general.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "general" '("--make-sure-user-actually-wants-to-quit" "bind-fill-region" "capf-enable-ispell-locally" "describe-region" "enable-wrap" "kill-and-quit" "minify" "pop-saved-position" "print-saved-positions" "save-" "sort-words" "temp" "user-ext-local-position-ring")))

;;;***

;;;### (autoloads nil "help-ext" "help-ext.el" (0 0 0 0))
;;; Generated autoloads from help-ext.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "help-ext" '("help-")))

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

;;;### (autoloads nil "image-ext" "image-ext.el" (0 0 0 0))
;;; Generated autoloads from image-ext.el

(autoload 'image--extra-hook "image-ext" nil nil nil)

(add-hook 'image-mode-hook #'image--extra-hook)

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

;;;***

;;;### (autoloads nil "info" "info.el" (0 0 0 0))
;;; Generated autoloads from info.el

(autoload 'info-node "info" "\
Enter Info, the documentation browser, for NODE.
NODE is an Info node of the form \"(FILENAME)NODENAME\".  Optional
argument BUFFER specifies the Info buffer name; the default buffer
name is \"*info*\".  If BUFFER exists, just switch to it; otherwise,
create a new buffer with the top-level Info directory.

In interactive use, a numeric prefix argument of N selects an Info
buffer named \"*info*<N>\".

Aside from these differences, this works exactly the same as
`info'.

\(fn NODE &optional BUFFER)" t nil)

(autoload 'info-node-other-window "info" "\
Like `info-node' but show buffer in another window.

\(fn NODE &optional BUFFER)" t nil)

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

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "keymaps-ext" '("--contextual-key-" "define-contextual-key-func" "lookup-function" "map-revert-buffer" "user-ext-keymaps--key-translation-table")))

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

\(fn NAME &optional DOCSTRING &rest SKELETON)" nil t)

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

(autoload 'kill-lsp-buffers "lsp-ext" "\
Kill all buffers that have to do with function `lsp-mode'." t nil)

(autoload 'lsp-ext--delete-temp-workspace-folders "lsp-ext" "\
Remove temporary folders from the LSP workspace.

Remove the contents of `user-ext-lsp-temporary-workspace-folders'
from the workspace list.  Effectively, this removes
temporary folders from the workspace." nil nil)

(autoload 'lsp-ext-workspace-folders-remove-list "lsp-ext" "\
Call `lsp-workspace-folders-remove' one or more times in a loop." t nil)

(autoload 'lsp-ext-workspace-blocklist-add "lsp-ext" "\
Add PROJECT-ROOT to the workspace blocklist.

\(fn PROJECT-ROOT)" t nil)

(autoload 'lsp-ext-workspace-blocklist-remove-all "lsp-ext" "\
Remove all folders in the workspace blocklist." t nil)

(autoload 'lsp-ext-workspace-folders-add-temp "lsp-ext" "\
Temporarily add PROJECT-ROOT to the list of workspace folders.

\(fn PROJECT-ROOT)" t nil)

(autoload 'lsp-ext-open-project-config "lsp-ext" "\
Open the project config file.

\(fn PROJECT-ROOT)" t nil)

(autoload 'lsp-ext-python-rename-buffer "lsp-ext" "\
Rename the current buffer." t nil)

(autoload 'lsp-ext--start-hook "lsp-ext" nil nil nil)

(add-hook 'kill-emacs-hook #'lsp-ext--delete-temp-workspace-folders)

(add-hook 'lsp-configure-hook #'lsp-ext--start-hook)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-ext" '("lsp-" "user-ext-lsp-")))

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

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-ext" '("org-ext-" "user-ext-org-")))

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

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "outline-ext" '("outline-ext--after-premote-demote")))

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

(autoload 'alist-ext-dolist "packages/alist-ext" "\
Loop over an alist.
Evaluate BODY with KVAR and VVAR bound the key and value of
each association from ALIST.  Then evaluate RESULT to get
the return value, defaulting to nil.

\(fn (KEY-VAR VAL-VAR ALIST [RESULT]) BODY...)" nil t)

(function-put 'alist-ext-dolist 'lisp-indent-function '1)

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
it behaves exactly the same as the latter.

If BODY is nil and COND is an `and' form ala
`(or SUB-CONDS...)', SUB-CONDS is collapsed into COND.  As
a result, the form

   (cl-ext-when (and n (> n 0))
       n)

expands to

   (and n (> n 0) n)

If BODY is nil, and FIRST-FORM returns non-nil on the
predicate `cl-ext--pcase-special-form', this expands
directly to an `if' form.

\(fn COND FIRST-FORM BODY...)" nil t)

(function-put 'cl-ext-when 'lisp-indent-function '2)

(autoload 'cl-ext-unless "packages/cl-ext" "\
If COND yields nil, do FIRST-FORM and BODY, else return nil.
When COND yields nil, eval FIRST-FORM and BODY forms
sequentially and return value of last one.

The main difference between `cl-ext-unless' and `unless' is
that when BODY is empty, this expands to an `or' form;
otherwise, it behaves exactly the same as the latter.

If BODY is nil and COND is an `or' form ala
`(or SUB-CONDS...)', SUB-CONDS is collapsed into COND.  As
a result, the form

   (cl-ext-unless (or n (<= n 0))
       n)

expands to

   (or n (<= n 0) n)

If BODY is nil, and FIRST-FORM returns non-nil on the
predicate `cl-ext--pcase-special-form', this expands
directly to an ...

\(fn COND FIRST-FORM BODY...)" nil t)

(function-put 'cl-ext-unless 'lisp-indent-function '2)

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

(autoload 'cl-ext-until "packages/cl-ext" "\
If TEST yields nil, eval BODY...and repeat.
This is the opposite of `while'--BODY is evaluated every
iteration of the loop until TEST returns non-nil.

\(fn TEST BODY...)" nil t)

(function-put 'cl-ext-until 'lisp-indent-function '1)

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

(autoload 'cl-ext-check-type "packages/cl-ext" "\
Verify FORM is of type TYPE; signal an error if not.
To wit, signal `type-error' if VALUE does not match TYPE
according to `cl-typep', which see.  Include STRING in the
error message if provided.

\(fn FORM TYPE &optional STRING)" nil t)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "packages/cl-ext" '("cl-ext-" "user-ext-cl-special-forms")))

;;;***

;;;### (autoloads nil "packages/debug-ext" "packages/debug-ext.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from packages/debug-ext.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "packages/debug-ext" '("--" "debug-ext-get-function-body")))

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

If the first element of BODY is the keyword :remove, SYMBOL
is reverted to its original state, with its original function
definition.

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

;;;### (autoloads nil "packages/variable-ext" "packages/variable-ext.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from packages/variable-ext.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "packages/variable-ext" '("defconst2")))

;;;***

;;;### (autoloads nil "perl-ext" "perl-ext.el" (0 0 0 0))
;;; Generated autoloads from perl-ext.el

(autoload 'perl-mode--extra-hook "perl-ext" nil nil nil)
 (autoload 'perl-ext-scratch-buffer "perl-ext" "Create a Perl scratch buffer." t)

(add-hook 'perl-mode-hook #'perl-mode--extra-hook)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "perl-ext" '("perl-ext-scratch-buffer")))

;;;***

;;;### (autoloads nil "plantuml-ext" "plantuml-ext.el" (0 0 0 0))
;;; Generated autoloads from plantuml-ext.el

(autoload 'plantuml--extra-hook "plantuml-ext" nil nil nil)

(add-hook 'plantuml-mode-hook #'plantuml--extra-hook)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "plantuml-ext" '("plantuml-ext-" "user-ext-plantuml-")))

;;;***

;;;### (autoloads nil "polymode-ext" "polymode-ext.el" (0 0 0 0))
;;; Generated autoloads from polymode-ext.el
 (autoload 'poly-emacs-lisp-file-variables-mode "polymode-ext" "A variation of `emacs-lisp-mode' for Polymode." t)
 (autoload 'poly-html-mode "polymode-ext" "A variation of `html-mode' for Poly mode." t)
 (autoload 'poly-markdown-mode "polymode-ext" "A variation of `markdown-mode' for Poly mode." t)
 (autoload 'poly-org-mode "polymode-ext" "A variation of `org-mode' for Poly mode." t)
 (autoload 'nil "polymode-ext" "A variation of `plantuml-mode' for Poly mode." t)
 (autoload 'poly-shell-script-mode "polymode-ext" "A variation of `shell-script-mode' for Poly mode." t)

(advice-add 'hack-local-variables :around #'polymode-inhibit-in-indirect-buffers)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "polymode-ext" '("poly-")))

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
Open a temporary buffer to write a docstring.

The major mode of the buffer is controlled by the user option
`user-ext-python-docstring-major-mode'.  A list of minor
modes are enabled according to the user option
`user-ext-python-docstring-minor-modes'.

The initial fill column is controlled by the user option
`user-ext-python-docstring-fill-column'." t nil)

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

;;;### (autoloads nil "search-ext" "search-ext.el" (0 0 0 0))
;;; Generated autoloads from search-ext.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "search-ext" '("python-regexp-to-emacs" "search-ext-")))

;;;***

;;;### (autoloads nil "sh-ext" "sh-ext.el" (0 0 0 0))
;;; Generated autoloads from sh-ext.el

(autoload 'sh--extra-hook "sh-ext" nil nil nil)

(add-hook 'sh-mode-hook #'sh--extra-hook)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sh-ext" '("global-abbrev-table" "sh-" "user-ext-sh-")))

;;;***

;;;### (autoloads nil "syntax-ext" "syntax-ext.el.gz" (0 0 0 0))
;;; Generated autoloads from syntax-ext.el.gz

(autoload 'ppss-open-paren-depth "syntax-ext" "\
Get the N'th innermost depth of CL-X's open parentheses.
CL-X is a `ppss' type, and N is the optional depth.  If N is
omitted or nil, then get the innermost depth.

See the info node `(elisp)Parser State', and look at element
9 for more information.

\(fn CL-X &optional N)" nil nil)

(function-put 'ppss-open-paren-depth 'side-effect-free 't)

(function-put 'ppss-open-paren-depth 'pure 't)

(defsubst make-ppss-easy (ppss) "\
Constructor of type `ppss' from PPSS." (make-ppss :depth (nth 0 ppss) :innermost-start (nth 1 ppss) :last-complete-sexp-start (nth 2 ppss) :string-terminator (nth 3 ppss) :comment-depth (nth 4 ppss) :quoted-p (nth 5 ppss) :min-depth (nth 6 ppss) :comment-style (nth 7 ppss) :comment-or-string-start (nth 8 ppss) :open-parens (nth 9 ppss) :two-character-syntax (nth 10 ppss)))

;;;***

;;;### (autoloads nil "table-ext" "table-ext.el" (0 0 0 0))
;;; Generated autoloads from table-ext.el

(autoload 'table-mode "table-ext" "\
Minor mode for tables created via `table-insert'.

If called interactively, enable Table mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "tcl-ext" "tcl-ext.el.gz" (0 0 0 0))
;;; Generated autoloads from tcl-ext.el.gz

(autoload 'tcl--extra-hook "tcl-ext" "\
Extra hook for `tcl mode'." nil nil)

(add-hook 'tcl-mode-hook #'tcl--extra-hook)

;;;***

;;;### (autoloads nil "temp" "temp.el" (0 0 0 0))
;;; Generated autoloads from temp.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "temp" '("plantuml-ext-" "user-ext-plantuml-submode")))

;;;***

;;;### (autoloads nil "text-ext" "text-ext.el.gz" (0 0 0 0))
;;; Generated autoloads from text-ext.el.gz

(autoload 'text-ext-highlight-even-lines "text-ext" "\
Highlight every even-numbered line in the current buffer." t nil)

;;;***

;;;### (autoloads nil "thingatpt-ext" "thingatpt-ext.el" (0 0 0 0))
;;; Generated autoloads from thingatpt-ext.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "thingatpt-ext" '("copy-thing-at-point" "define-" "kill-thing-at-point" "thing-at-point-ext-" "user-ext-thingatpt-valid-things" "valid-thing")))

;;;***

;;;### (autoloads nil "toml-ext" "toml-ext.el.gz" (0 0 0 0))
;;; Generated autoloads from toml-ext.el.gz

(autoload 'toml-mode--extra-hook "toml-ext" "\
Extra hook for `toml-mode'." nil nil)

(add-hook 'toml-mode-hook #'toml-mode--extra-hook)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "toml-ext" '("toml-ext-indent")))

;;;***

;;;### (autoloads nil "types-ext" "types-ext.el" (0 0 0 0))
;;; Generated autoloads from types-ext.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "types-ext" '("alist-")))

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

;;;### (autoloads nil nil ("code-outline-bootstrap.el" "commit.el"
;;;;;;  "compat-29-ext.el" "faces-ext.el" "git-ext.el" "jdesktop-bootstrap.el"
;;;;;;  "jinja2-bootstrap.el" "liquidsoap-bootstrap.el.gz" "packages/documentation-ext.el"
;;;;;;  "polymode-bootstrap.el") (0 0 0 0))

;;;***

;;;### (autoloads nil "abbrev-ext" "abbrev-ext.el" (0 0 0 0))
;;; Generated autoloads from abbrev-ext.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "abbrev-ext" '("abbrev-ext-" "global-abbrev-table" "user-ext-abbrev-")))

;;;***

(provide 'loaddefs-ext)
;;; loaddefs-ext.el ends here

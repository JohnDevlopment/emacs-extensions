;;; loaddefs-ext.el --- Autoloads                    -*- lexical-binding: t; -*-

;; Copyright (C) 2024  John

;; Author: John <john@john-System-Product-Name>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;

;;; Code:


;;;### (autoloads nil "bbcode-ext" "bbcode-ext.el" (0 0 0 0))
;;; Generated autoloads from bbcode-ext.el

(autoload 'bbcode-emphasis "bbcode-ext" "\
Insert BBCode tags at point or around region according to CHAR.

If called interactively, or if BEG and END are non-nil, the
tags are wrapped around the region indicated by BEG and END.

\(fn CHAR &optional BEG END)" t nil)

(autoload 'bbcode-ext-enable "bbcode-ext" "\
Call this function to load `bbcode-ext'.
Otherwise, this function does nothing." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "bbcode-ext" '("bbcode-")))

;;;***

;;;### (autoloads nil "buffers-ext" "buffers-ext.el" (0 0 0 0))
;;; Generated autoloads from buffers-ext.el

(autoload 'kill-certain-temp-buffers "buffers-ext" "\
Convenience function to kill certain buffers you do not need.

This kills buffers belonging to `user-ext-temp-buffers-to-kill' and
`user-ext-temp-buffers-to-kill-regex'." t nil)

(autoload 'kill-lsp-buffers "buffers-ext" "\
Kill all buffers that have to do with function `lsp-mode'." t nil)

(autoload 'kill-customization-buffers "buffers-ext" "\
Close all customization buffers.
Internally, calls `kill-buffers' with \"^*Customize.*\" as the pattern." t nil)

(autoload 'kill-flymake-diagnostics "buffers-ext" "\
Close all buffers for flymake diagnostics." t nil)

(autoload 'kill-buffers "buffers-ext" "\
Close all buffers matching PATTERN.

\(fn PATTERN)" t nil)

(autoload 'get-buffer-file-name "buffers-ext" "\
Print the file belonging to the current buffer." t nil)
 (autoload 'faces-buffer "buffers-ext" "Open a buffer listing all the faces." t)
 (autoload 'docstring-scratch "Open a scratch buffer for documentation strings.\n..." t)
 (autoload 'git-commit-scratch "buffers-ext" "Open a scratch buffer to let you format a git commit." t)

(autoload 'revert-all-buffers "buffers-ext" "\
Reverts all buffers, including special buffers.
This reverts all buffers in like manner to `revert-buffer'.  The user is NOT
asked to confirm, so be careful when using this function.  IGNORE-AUTO and
PREVERSE-MODES are the same as for `revert-buffer', and they are specified
as prefix args." t nil)

(autoload 'tmpbuf "buffers-ext" "\
Open a temporary buffer.

If it doesn't exist, open a new one.  BUF is the name of the
buffer.

If this function is called interactively, or if SWITCH is
non-nil, this switches to the newly created buffer.
Otherwise, this just returns the newly created buffer.

\(fn BUF &optional SWITCH)" t nil)

(autoload 'narrow-to-region2 "buffers-ext" "\
Call `narrow-to-region' with START and END.

\(fn START END)" t nil)

(autoload 'view-into-buffer "buffers-ext" "\
Create an indirect buffer and show it in another window.

Creates an indirect buffer of BASE-BUFFER and shows it in
another window.  BASE-BUFFER should be a live buffer or the
name of an existing buffer.  If the optional arg CLONE is
non-nil, BASE-BUFFER's state is preserved in the indirect
buffer, meaning things like minor and major modes.  Otherwise,
the indirect buffer's state is reset to default values.

Interactively, CLONE is a prefix argument.

\(fn BASE-BUFFER &optional CLONE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "buffers-ext" '("define-scratch-buffer-function" "docstring-scratch" "faces-buffer" "git-commit-scratch" "user-ext-temp-buffers-to-kill")))

;;;***

;;;### (autoloads nil "c-ext" "c-ext.el" (0 0 0 0))
;;; Generated autoloads from c-ext.el

(autoload 'c--custom-hook "c-ext" nil nil nil)

(add-hook 'c-mode-common-hook 'c--custom-hook)

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

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "custom-ext" '("log-view-")))

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

(autoload 'elisp-ext-create-dir-locals-file "elisp-ext" "\
Create a .dir-locals.el file at FILE.

\(fn FILE)" t nil)

(autoload 'elisp-ext-forward-or-backward-sexp "elisp-ext" "\
Go to the matching parenthesis to the one is adjacent at point.
With ARG, do it that many times.  A negative arg -N reverses
the direction of the motion.

\(fn &optional ARG)" t nil)
 (autoload 'elisp-ext-scratch-buffer "elisp-ext" "Create a scratch buffer for Emacs lisp code." t)
 (autoload 'elisp-ext-doc-scratch-buffer "elisp-ext" "Create a scratch buffer for Emacs lisp docstrings.")

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "elisp-ext" '("elisp-ext-" "user-ext-elisp-dir-locals-template")))

;;;***

;;;### (autoloads nil "extensions" "extensions.el" (0 0 0 0))
;;; Generated autoloads from extensions.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "extensions" '("--extension-completion" "find-extension" "load-extension")))

;;;***

;;;### (autoloads nil "general" "general.el" (0 0 0 0))
;;; Generated autoloads from general.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "general" '("add-mode-comment" "bind-fill-region" "copy-line" "count-words-region2" "date-format-version" "emacs-lisp-mode-abbrev-table" "enable-wrap" "kill-and-quit" "narrow-to-region2" "pop-saved-position" "save-current-position")))

;;;***

;;;### (autoloads nil "html-ext" "html-ext.el" (0 0 0 0))
;;; Generated autoloads from html-ext.el

(autoload 'modify-html-tag-alist "html-ext" "\
Modify `html-tag-alist' with our own tags." t nil)

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

;;;### (autoloads nil "js-ext" "js-ext.el" (0 0 0 0))
;;; Generated autoloads from js-ext.el

(autoload 'js2--extra-hook "js-ext" nil nil nil)

(add-hook 'js2-mode-hook #'js2--extra-hook)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "js-ext" '("user-ext-js-indent-")))

;;;***

;;;### (autoloads nil "keymaps-ext" "keymaps-ext.el" (0 0 0 0))
;;; Generated autoloads from keymaps-ext.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "keymaps-ext" '("define-contextual-key-func" "lookup-function" "map-revert-buffer" "remap-narrow-to-region")))

;;;***

;;;### (autoloads nil "liquidsoap-ext" "liquidsoap-ext.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from liquidsoap-ext.el

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

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-ext" '("lsp-")))

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

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "markdown-ext" '("markdown-")))

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

;;;### (autoloads nil "perl-ext" "perl-ext.el" (0 0 0 0))
;;; Generated autoloads from perl-ext.el

(autoload 'perl-mode--extra-hook "perl-ext" nil nil nil)
 (autoload 'perl-ext-scratch-buffer "perl-ext" "Create a Perl scratch buffer." t)

(add-hook 'perl-mode-hook #'perl-mode--extra-hook)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "perl-ext" '("perl-ext-scratch-buffer")))

;;;***

;;;### (autoloads nil "python-ext" "python-ext.el" (0 0 0 0))
;;; Generated autoloads from python-ext.el
 (autoload 'python-ext-scratch "python-ext" "Opens a scratch buffer to let you write Python code." t)

(autoload 'python--extra-hook "python-ext" "\
Hook for `python-mode' for this extension." nil nil)

(autoload 'python--lsp-hook "python-ext" "\
Hook for `python-mode' when lsp is enabled." nil nil)

(add-hook 'python-mode-hook #'python--extra-hook)

(add-hook 'lsp-after-open-hook #'python--lsp-hook)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "python-ext" '("python-" "user-ext-python-")))

;;;***

;;;### (autoloads nil "python-sphinx-ext" "python-sphinx-ext.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from python-sphinx-ext.el

(autoload 'sphinx-ext-align "python-sphinx-ext" "\
Align the current line with the first line of this role.

Point should be on the second line of a ':param:' or
':keyword:' role after `auto-fill-mode' kicks in, for best
effect." t nil)

(autoload 'sphinx-doc-mode--extra-hook "python-sphinx-ext" "\
Hook for `sphinx-doc-mode'." nil nil)

(add-hook 'sphinx-doc-mode-hook #'sphinx-doc-mode--extra-hook)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "python-sphinx-ext" '("sphinx-ext-" "user-ext-sphinx-param-regexp")))

;;;***

;;;### (autoloads nil "rust-ext" "rust-ext.el" (0 0 0 0))
;;; Generated autoloads from rust-ext.el

(autoload 'rust--custom-hook "rust-ext" nil nil nil)

(add-hook 'rust-mode-hook #'rust--custom-hook)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rust-ext" '("rust-skeleton-define")))

;;;***

;;;### (autoloads nil "sh-ext" "sh-ext.el" (0 0 0 0))
;;; Generated autoloads from sh-ext.el

(autoload 'sh--extra-hook "sh-ext" nil nil nil)

(add-hook 'sh-mode-hook #'sh--extra-hook)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sh-ext" '("sh-ext-" "user-ext-sh-ext-color-escapes")))

;;;***

;;;### (autoloads nil "syntax-ext" "syntax-ext.el" (0 0 0 0))
;;; Generated autoloads from syntax-ext.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "syntax-ext" '("my/mcpyrate-syntax-highlight-setup")))

;;;***

;;;### (autoloads nil "tcl-ext" "tcl-ext.el" (0 0 0 0))
;;; Generated autoloads from tcl-ext.el

(autoload 'tcl--extra-hook "tcl-ext" "\
Extra hook for `tcl mode'." nil nil)

(add-hook 'tcl-mode-hook #'tcl--extra-hook)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "tcl-ext" '("tcl-eval-region2")))

;;;***

;;;### (autoloads nil "text-ext" "text-ext.el" (0 0 0 0))
;;; Generated autoloads from text-ext.el

(autoload 'text-ext-highlight-even-lines "text-ext" "\
Highlight every even-numbered line in the current buffer." t nil)

;;;***

;;;### (autoloads nil "toml-ext" "toml-ext.el" (0 0 0 0))
;;; Generated autoloads from toml-ext.el

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

;;;### (autoloads nil nil ("faces-ext.el" "liquidsoap-bootstrap.el"
;;;;;;  "modes.el") (0 0 0 0))

;;;***

;;;### (autoloads nil "abbrev-ext" "abbrev-ext.el" (0 0 0 0))
;;; Generated autoloads from abbrev-ext.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "abbrev-ext" '("abbrev-ext-insert-hook")))

;;;***

(provide 'loaddefs-ext)

;;; loaddefs-ext.el ends here

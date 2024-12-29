;;; loaddefs-ext.el ---                              -*- lexical-binding: t; -*-

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

;; (provide 'loaddefs-ext)

;;; loaddefs-ext.el ends here

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

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "buffers-ext" '("clone-indirect-buffer-this-window" "define-scratch-buffer-function" "faces-buffer" "git-commit-scratch" "kill-" "narrow-to-region2" "revert-all-buffers" "text-scratch--complete-mode" "tmpbuf" "user-ext-temp-buffers-to-kill" "view-into-buffer")))

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

(autoload 'elisp-ext-minify "elisp-ext" "\
Minify the code between START and END in current buffer.
If called interactively, START and END are the region,
provided the region is active.  But if the region is not
active, the entire buffer is minified.

\(fn START END)" t nil)

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

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "elisp-ext" '("elisp-ext-" "user-ext-elisp-")))

;;;***

;;;### (autoloads nil "extensions" "extensions.el" (0 0 0 0))
;;; Generated autoloads from extensions.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "extensions" '("--extension-completion" "find-extension" "load-extension" "user-ext-extension-directory")))

;;;***

;;;### (autoloads nil "general" "general.el" (0 0 0 0))
;;; Generated autoloads from general.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "general" '("--add-mode-comment--" "add-mode-comment" "bind-fill-region" "copy-line" "count-words-region2" "date-format-version" "emacs-lisp-mode-abbrev-table" "enable-wrap" "kill-and-quit" "narrow-to-region2" "pop-saved-position" "print-saved-positions" "save-" "sort-words" "user-ext-local-position-ring")))

;;;***

;;;### (autoloads nil "html-ext" "html-ext.el" (0 0 0 0))
;;; Generated autoloads from html-ext.el

(autoload 'modify-html-tag-alist "html-ext" "\
Modify `html-tag-alist' with our own tags." t nil)

;;;***

;;;### (autoloads nil "ibuffer-ext" "ibuffer-ext.el" (0 0 0 0))
;;; Generated autoloads from ibuffer-ext.el

(autoload 'ibuffer-toggle-current-filter-group "ibuffer-ext" "\
Expand/collapse the filter group of point.
If point is on a buffer, toggle the group it is in." t nil)

(autoload 'ibuffer--extra-hook "ibuffer-ext" nil nil nil)

(add-hook 'ibuffer-mode-hook #'ibuffer--extra-hook)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ibuffer-ext" '("ibuffer--after-operation")))

;;;***

;;;### (autoloads nil "imenu-ext" "imenu-ext.el" (0 0 0 0))
;;; Generated autoloads from imenu-ext.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "imenu-ext" '("bind-imenu" "imenu--python-hook")))

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

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jinja2" '("jinja2-")))

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

(advice-add 'lsp--before-save :after #'lsp-ext--before-save)

(autoload 'lsp-ext--before-save "lsp-ext" "\
Disable LSP inlays and maybe do other things before saving.
This is supposed to be called before `lsp--before-save'." nil nil)

(autoload 'kill-lsp-buffers "lsp-ext" "\
Kill all buffers that have to do with function `lsp-mode'." t nil)

(autoload 'lsp-workspace-folders-remove-list "lsp-ext" "\
Call `lsp-workspace-folders-remove' one or more times in a loop." t nil)

(autoload 'lsp-workspace-blocklist-add "lsp-ext" "\
Add PROJECT-ROOT to the workspace blocklist.

\(fn PROJECT-ROOT)" t nil)

(autoload 'lsp-workspace-blocklist-remove-all "lsp-ext" nil t nil)

(autoload 'lsp-workspace-folders-add-temp "lsp-ext" "\
Temporarily add PROJECT-ROOT to the list of workspace folders.

\(fn PROJECT-ROOT)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-ext" '("lsp--delete-temp-workspace-folders" "user-ext-lsp-")))

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

;;;### (autoloads nil "outline-ext" "outline-ext.el" (0 0 0 0))
;;; Generated autoloads from outline-ext.el

(autoload 'outline--extra-hook "outline-ext" nil nil nil)

(add-hook 'outline-mode-hook #'outline--extra-hook)

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
 (autoload 'rust-ext-scratch-buffer "rust-ext" "Open a scratch buffer in `rustic-mode'." t)

(autoload 'rust--custom-hook "rust-ext" "\
Hook for `rustic-mode' extension." nil nil)

(add-hook 'rust-mode-hook #'rust--custom-hook)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rust-ext" '("rust-" "user-ext-rust-")))

;;;***

;;;### (autoloads nil "sh-ext" "sh-ext.el" (0 0 0 0))
;;; Generated autoloads from sh-ext.el

(autoload 'sh--extra-hook "sh-ext" nil nil nil)

(add-hook 'sh-mode-hook #'sh--extra-hook)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sh-ext" '("sh-" "user-ext-sh-")))

;;;***

;;;### (autoloads nil "syntax-ext" "syntax-ext.el" (0 0 0 0))
;;; Generated autoloads from syntax-ext.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "syntax-ext" '("make-ppss-easy" "my/mcpyrate-syntax-highlight-setup")))

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

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "toml-ext" '("toml-ext-")))

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

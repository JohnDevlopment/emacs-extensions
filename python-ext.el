;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'python-mode)
(require 'python)
(require 'lsp)
(require 'function-ext)

(eval-when-compile
  (declare-function python-ext-command-prefix "python-ext")
  (defvar python-ext-command-prefix)
  (require 'dash)
  (require 'skeleton))

(put #'python-rx 'function-documentation
     "Python mode specialized rx macro.
This variant of `rx' supports common Python named REGEXPS.

This macro adds additional constructs, such as the
following:

Constructs that begin and end with symbol boundaries:

block-start     Match a keyword that starts a block (def, class,
                if/elif/else, try/except/finally, for, while, with,
                async def/for/with).
dedenter        Match the keywords that cause dedention (elif, else,
                except, finally).
defun           Match the symbol def, class, or the async equivelents.
block-ender     Match the symbols break, continue, pass, raise,
                return.

decorator       Match decorators.
if-name-main    Match \\`if __name__ == \"__main__\"'
symbol-name     Match any symbol.
open-paren      Match open parenthesis.
close-paren     Match close parenthesis.

simple-operator         Match a simple operator (+, -, /, &, ^, ~, |,
                        ^, *, <, >, ?=, ?%).
not-simple-operator     Match the inverse of simple-operator.
operator                Match an operator (==, >=, is, not, **, //,
                        <<, >>, <=, !=, +, -, /, &, ^, ~, |, *, <, >,
                        =, %).
assignment-operator	Match the assignment operators (+=, -=, *=,
                        /=, //=, %=, **=, >>=, <<=, &=, ^=, |=, =).

string-delimiter	Match both signal and triple string delimters,
                        accounting for escape sequences.
coding-cookie		Coding system declarations, which are
                        recognized by Emacs, Vim and Python. Examples
                        include: \"# coding=utf-8\", \"# coding: utf-8\",
                        \"# -*- coding: utf-8 -*-\",
                        \"# vim: set fileencoding=utf-8 :\".

(fn &rest REGEXPS)")

(rx-define python-ext-kw-def
  (seq symbol-start
       (seq (? "async" (+ space)) "def")
       symbol-end))


;; ### Customization

(defgroup python-ext nil
  "A group for Python extension."
  :group 'user-extensions)

(defcustom user-ext-python-docstring-fill-column 60
  "Fill column for docstring."
  :type 'integer
  :group 'python-ext)

(defface user-ext-python-pydoc-keyword
  '((t . (:inherit font-lock-keyword-face)))
  "Face used for keywords."
  :group 'python-ext)

(defcustom user-ext-python-docstring-major-mode 'markdown-mode
  "Major mode for editing Python docstring."
  :type 'symbol
  :safe #'symbolp
  :group 'python-ext)

(defcustom user-ext-python-docstring-minor-modes
  '(abbrev-mode
    auto-fill-mode
    display-fill-column-indicator-mode
    sphinx-doc-mode)
  "Minor modes enabled when editing docstrings."
  :type '(repeat (symbol :tag "Function"))
  :safe #'listp
  :group 'python-ext)

(defcustom user-ext-python-ruff-format-command
  '("ruff" "format")
  "Commandline to format a file with Ruff.
The first element is the program to run, and the succeeding
elements are arguments to the program."
  :type '(repeat string)
  :group 'python-ext)

(defcustom user-ext-python-ruff-lint-command
  '("ruff" "check")
  "Commandline to lint a file with Ruff.
The first element is the program to run, and the succeeding
elements are arguments to the program."
  :type '(repeat string)
  :group 'python-ext)

(defcustom user-ext-python-ruff-lint-extra-rules
  nil
  "Extra rules to add to the Ruff command."
  :type '(repeat string)
  :group 'python-ext)

(defface user-ext-python-lint-keyword-face
  '((t . (:inherit font-lock-keyword-face)))
  "Face used for keywords, such as rule codes."
  :group 'python-ext)

(defface user-ext-python-lint-constant-face
  '((t . (:inherit font-lock-constant-face)))
  "Face used for so-called constants."
  :group 'python-ext)

(defface user-ext-python-lint-summary-face
  '((t . (:inherit bold)))
  "Face used for the summary line of every lint error."
  :group 'python-ext)


;; ### Variables

(defconst user-ext-python--register ?p
  "The register for `python-ext-docstring'.
This is passed to `window-configuration-to-register'.")

(defconst user-ext-python-identifier-regex
  (rx word-start
      (group
       (any (?A . ?Z) (?a . ?z) ?_)
       (+ (any (?A . ?Z) (?a . ?z) ?_ "0-9"))
       (* ?.
	  (any (?A . ?Z) (?a . ?z) ?_)
	  (+ (any (?A . ?Z) (?a . ?z) ?_ "0-9"))))
      word-end)
  "Regular expression for matching identifiers to be documented.
Group 1 matches the whole identifier.

Examples of what get matched:
   $ a = 'some string'
   $ a.upper()
     ^^^^^^^
   $ str.make_trans({})")

(defconst user-ext-python-def-regexp
  (python-rx (? bol (* space)) (group python-ext-kw-def)
	     (+ space) (group symbol-name)
	     open-paren (*? anything) close-paren (* space) ; arguments
	     (? "->" (* space) (+? nonl) (* space))	    ; return type
	     ?:)
  "A regular expression that matches function definitions.
Group 1 matches the keyword that starts the block.
Group 2 matches the name of the function.")

(defconst user-ext-python-def-start-regexp
  (python-rx defun (* space))
  "Matches the beginning of a function definition.")

(defvar user-ext-python--orig-position nil
  "Position in the original buffer when editing Python docstring.")

(defvar user-ext-python--orig-buffer nil
  "Original buffer when editing Python docstring.")

(defvar user-ext-python--docstring-buffer nil
  "Buffer for Python docstring.")

(defvar user-ext-python--reverted nil
  "t if `python-ext-revert-all-python-buffers' is called.")

(defvar user-ext-python--first-buffer-loaded nil
  "Indicates whether the first Python buffer was loaded.
This is non-nil if the first Python buffer was loaded, nil
otherwise.")

(defconst user-ext-python-ruff-lint-help-buffer
  "*ruff-lint errors*"
  "The help buffer that shows Ruff lint errors.")


;; ### Project

(defgroup python-ext-project nil
  "Python project management."
  :group 'python-ext)

(defcustom user-ext-python-project-ignores nil
  "List of glob patterns to ignore in a Python project.
This has the same format as `project-vc-ignores', which see."
  :group 'python-ext-project
  :type '(repeat string)
  :safe #'list-of-strings-p)


;; --- Functions

(cl-defstruct python-ext-project
  (root default-directory :type string
	:documentation "Root directory of the project.")
  (name "" :type string
	:documentation "Name of the project."))

(cl-defmethod project-root ((project python-ext-project))
  (python-ext-project-root project))

(cl-defmethod project-ignores ((project python-ext-project) dir)
  (let* ((root (python-ext-project-root project))
         (backend 'Git))
    (append
     (when (and backend (file-equal-p dir root))
       (delq
        nil
        (mapcar
         (lambda (entry)
           (cond
            ((eq ?! (aref entry 0))
             ;; No support for whitelisting (yet).
             nil)
            ((string-match "\\(/\\)[^/]" entry)
             ;; FIXME: This seems to be Git-specific.
             ;; And / in the entry (start or even the middle) means
             ;; the pattern is "rooted".  Or actually it is then
             ;; relative to its respective .gitignore (of which there
             ;; could be several), but we only support .gitignore at
             ;; the root.
             (if (= (match-beginning 0) 0)
                 (replace-match "./" t t entry 1)
               (concat "./" entry)))
            (t entry)))
         (condition-case nil
             (vc-call-backend backend 'ignore-completion-table root)
           (vc-not-supported () nil)))))
     (project--value-in-dir 'user-ext-python-project-ignores root)
     (mapcar
      (lambda (dir)
        (concat dir "/"))
      vc-directory-exclusion-list))))

(cl-defmethod project-files ((project python-ext-project) &optional dirs)
  (mapcan
   (lambda (dir)
     (let ((ignores (project--value-in-dir 'user-ext-python-project-ignores dir))
	   (backend 'Git))
       (require 'vc-git)
       (if (and (file-equal-p dir (python-ext-project-root project))
		(cl-ext-cond
                  ((eq backend 'Hg) t)
                  ((and (eq backend 'Git)
			(or
                         (not ignores)
                         (version<= "1.9" (vc-git--program-version))))
		   t)))
	   (project--vc-list-files dir backend ignores)
	 (project--files-in-directory
          dir
          (project--dir-ignores project dir)))))
   (or dirs (list (project-root project)))))

(defun python-ext-try-project (dir)
  "Detect Python project."
  (when-let ((root (locate-dominating-file default-directory "pyproject.toml")))
    (make-python-ext-project :root root
			     :name (f-filename root))))

(defun python-ext-project-current (&optional maybe-prompt no-error)
  "Return the project instance if this is a Python project."
  (let ((project (project-current maybe-prompt)))
    (if (and project (python-ext-project-p project))
	project
      (unless no-error
	(user-error "Not in Python project")))))

(defsubst python-ext-project-get-root ()
  "Return the root of the current project."
  (python-ext-project-root (python-ext-project-current)))

(defun python-ext-project-parent (file)
  "Return the parent of FILE within the Python project."
  (let* ((root (->> (python-ext-project-current t)
		    (project-root))))
    (f-common-parent (list root (abbreviate-file-name file)))))

(defun python-ext-project-rename-buffer ()
  "Rename the current buffer to its relative module path."
  (interactive)
  (when-let ((file (abbreviate-file-name (buffer-file-name)))
	     (parent (abbreviate-file-name (python-ext-project-parent file)))
	     (module (python-ext-project-module-path file parent)))
    (rename-buffer module t)))

(defun python-ext-project-module-path (file root)
  "Return the relative module path of FILE.
ROOT is the root directory of the current project.
Return nil if FILE is not a descendant of ROOT."
  (if (f-descendant-of? file root)
      (let* ((modpath (f-split file))
	     (drop (length (f-split root))))
	(when (string= (nth drop modpath) "src")
	  (cl-incf drop))
	(when (= (- (length modpath) drop) 1)
	  (error "%s is not a part of a package" file))
	(cl-incf drop)			; Next index assumed to be package name
	(setq modpath (--drop-while (< it-index drop) modpath))
	(let* ((i (1- (length modpath)))
	       (file (nth i modpath)))
	  ;; Remove extension from last item in list
	  (setf (nth i modpath) (f-no-ext file)))
	(concat "." (s-join "." modpath)))))
(ert-deftest python-ext-project-test-module-path ()
  "A file inside the package -> .errors"
  (let ((parent "~/github/download-video")
	(file "~/github/download-video/download_video/errors.py"))
    (let ((result (python-ext-project-module-path file parent)))
      (should-not (null result))
      (should (equal result ".errors")))))
(ert-deftest python-ext-project-test-module-path-wrong-root ()
  "Nil result because file does not descend from parent"
  (let ((parent "~/github/emacs")
	(file "~/github/download-video/download_video/errors.py"))
    (let ((result (python-ext-project-module-path file parent)))
      (should (null result)))))
(ert-deftest python-ext-project-test-module-path-nested ()
  "Nested paths -> .site_processors.formats"
  (let ((parent "~/github/download-video")
	(file "~/github/download-video/download_video/site_processors/formats.py"))
    (let ((result (python-ext-project-module-path file parent)))
      (should-not (null result))
      (should (equal result ".site_processors.formats")))))
(ert-deftest python-ext-project-test-module-path-src ()
  "Handles src/ layouts -> .utils"
  (let ((parent "~/github/download-video")
	(file "~/github/download-video/src/download_video/utils.py"))
    (let ((result (python-ext-project-module-path file parent)))
      (should-not (null result))
      (should (equal result ".utils")))))

(add-hook 'project-find-functions #'python-ext-try-project -100)
(--ignore
  (remove-hook 'project-find-functions #'python-ext-try-project)
  t)


;; ### Functions

(fext-defadvice py-electric-backspace-mode
    (around py-electric-backspace-mode (oldfun &rest r))
  (with-demoted-errors "py-electric-backspace-mode: %S"
    (apply oldfun r)))

(advice-add #'py-shift-right :after #'deactivate-mark)
(advice-add #'py-shift-left :after #'deactivate-mark)

(defun python-ext-compute-indentation (oldfun &rest args)
  "Compute Python indentation."
  (if-let ((cl-x (python-ext--string-at-pos))
	   (indent (save-excursion
		     (goto-char (python-ext--docstring-start cl-x))
		     (skip-syntax-backward "\"")
		     (current-column))))
      (cl-ext-progn
	indent)
    (apply oldfun args)))
(advice-add #'py-compute-indentation :around #'python-ext-compute-indentation)

(defun python-ext--before-save ()
  (save-excursion
    (delete-trailing-whitespace)))

(defun python-ext-initialize-package (name &optional main)
  "Initialize package NAME."
  (interactive "sPackage Name: \nP")
  (when (y-or-n-p (format "Creating package %s in %s. Continue? "
			  name default-directory))
    (make-directory name)
    (find-file (f-join name "__init__.py"))
    (save-buffer)
    (kill-buffer)
    (when main
      (find-file (f-join name "__main__.py"))
      (save-buffer)
      (kill-buffer))))

(defun python-ext-convert-rx (regex &optional type)
  (cl-check-type regex string)
  (cl-check-type type (or character null))
  (cl-ecase (or type ?e)
    (?e
     (cl-loop with new-regex = regex
	      while (string-match (rx "\\" (group (any "(){}|"))) new-regex)
	      do
	      (setq new-regex (replace-match "\\1" nil nil new-regex))
	      finally return
	      new-regex))))

(defun python-ext-revert-all-python-buffers ()
  "Revert all Python buffers."
  (interactive)
  (unless user-ext-python--reverted
    (cl-loop
     with bl = (buffer-list)
     with bfn = nil
     for buf in bl
     do
     (setq bfn (buffer-file-name))
     (with-current-buffer buf
       (when (and bfn (file-exists-p bfn)
		  (not (buffer-modified-p))
		  (eq major-mode 'python-mode))
	 (revert-buffer t t)
	 (setq user-ext-python--reverted t))))))

(defun python-ext-shift-return ()
  "Called when the user presses <S-return>."
  (interactive)
  (py-newline-and-indent)
  (py-newline-and-indent)
  (py-electric-backspace)
  (forward-line -1)
  (py-indent-or-complete))

(defsubst python-ext--regexp-match (subexp end)
  (cond
   ((and subexp end)
    (match-end subexp))
   (subexp (match-beginning subexp))
   (end (match-end 0))
   (t (match-beginning 0))))

(defun python-ext-forward-regexp (regexp &optional subexp end)
  "Search forward from point for regular expression REGEXP.
Move point to the beginning of the match next.  If SUBEXP is
non-nil, match that subexpression (e.g., 1 for group 1).  If
END is non-nil, move point to the end of the match."
  (cl-check-type regexp string)
  (cl-check-type subexp (or integer null))
  (when (cl-ext-save-point (re-search-forward regexp nil t))
    (let ((pos (python-ext--regexp-match subexp end)))
      (prog1 pos
	(and pos (goto-char pos))))))

(defun python-ext-backward-regexp (regexp &optional subexp limit end)
  "Search backward from point for regular expression REGEXP.
Move point to the beginning of the next match.  SUBEXP and
END are the same as for `python-ext-forward-regexp', which
see.  If LIMIT is non-nil, bound the search so that the
match has to be after it; it is a buffer position."
  (cl-check-type regexp string)
  (cl-check-type subexp integer-or-null)
  (cl-check-type limit (or integer-or-marker null))
  (when (and (not (bobp))
	     (cl-ext-save-point (re-search-backward regexp limit t)))
    (let ((pos (python-ext--regexp-match subexp end)))
      (prog1 pos
	(and pos (goto-char pos))))))

(defun python-ext--pydoc (what)
  (cl-assert (stringp what) t "what = %s" what)
  (let (code)
    (with-temp-buffer
      (setq code (call-process "/bin/bash" nil t nil "-c" (concat "pydoc3 " what)))
      (cl-assert (cl-typep what 'integer))
      (unless (= code 0)
	(error "Non-zero error returned from Pydoc."))
      (buffer-string))))

(defun python-ext--get-symbol-at-point ()
  (let ((bol (cl-ext-save-point
	       (beginning-of-line)
	       (point)))
	bs)
    (save-excursion
      (when (looking-back user-ext-python-identifier-regex bol)
	(setq bs (match-string-no-properties 1))
	(cl-assert (cl-typep bs 'string))
	bs))))

;;;###autoload
(defun python-ext-pydoc (what)
  (interactive (list (read-string "What: " (thing-at-point 'word))))
  (let ((bufname "*pydoc*")
	(doc (python-ext--pydoc what))
	beg end)
    (with-help-window bufname
      (with-current-buffer bufname
	(insert doc)
	(goto-char (point-min))
	(save-excursion
	  (when (re-search-forward "^\\(class\\) [A-Za-z0-9_]+" nil t)
	    (setq beg (match-beginning 1)
		  end (match-end 1))
	    (cl-assert (not (null beg)))
	    (cl-assert (not (null end)))
	    (add-text-properties beg end '(face user-ext-python-pydoc-keyword))))))))

;;;###autoload
(defun python-ext-finish-variable-type ()
  "Finish the type of the variable at point.

In order for this to work, the current buffer must be using
LSP and the underlying server must support inlay hints.  To
see if that is available, call \\[lsp-describe-session] and
look for \`inlayHintProvider'."
  (interactive)
  (let (start end res)
    (save-excursion
      (setq end (point) start (progn
				(beginning-of-line)
				(point))))
    (setq res (lsp-request
	       "textDocument/inlayHint"
	       (lsp-make-inlay-hints-params
		:text-document (lsp--text-document-identifier)
		:range (lsp-make-range :start (lsp-point-to-position start)
				       :end (lsp-point-to-position end)))))
    (cl-block found-inlay
      (dolist (hint res)
	(-let* (((&InlayHint :label :position) hint)
		(label (lsp--label-from-inlay-hints-response label))
		(pos (lsp--position-to-point position)))
	  (if (= (point) pos)
	      (progn
		(cond
		 ((string-match-p "-> .+" label)
		  ;; complete return
		  (insert ?\  label)
		  (cl-return-from found-inlay))
		 ((string-match-p ": .+" label)
		  ;; complete variable type
		  (insert label)
		  (cl-return-from found-inlay))
		 ((string-match-p "[A-Za-z_][A-Za-z_0-9]*=" label)
		  ;; complete parameter name
		  (insert label)
		  (cl-return-from found-inlay))))
	    (message "label: %s, position: %s" label pos)))))))

;;;###autoload (autoload 'python-ext-scratch "python-ext" "Opens a scratch buffer to let you write Python code." t)
(define-scratch-buffer-function python-ext-scratch "python" nil
  "Opens a scratch buffer to let you write Python code."
  nil
  (python-mode))

;;;###autoload
(defun python-ext-kill-pyi-buffers ()
  (interactive)
  (kill-buffers "\\.pyi$"))

;;;###autoload
(defun python-ext-kill-venv-buffers ()
  (interactive)
  (kill-buffers "\\.pyi$" (lambda (buf)
			    (string-match-p "/\\.venv/.+$" (buffer-file-name buf)))))


;; --- Lint and format buffers

(eval-after-require lsp-ruff
  (defun python-ext-hack-ruff-args ()
    "Modify `lsp-ruff-ruff-args' to include a project-specific config file.
The following modifications are made:
- Add `--config $PROJECTDIR/$CONFIG` if not already present.
- Ensures Ruff uses the correct configuration file for the current project.

This function is intended to be called interactively or via hook to dynamically
adjust Ruff's behavior based on project layout."
    (interactive)
    (let* ((args lsp-ruff-ruff-args)
	   (root (project-root (python-ext-project-current)))
           (config-path
	    (cl-ext-progn
	      (cl-assert root)
	      (cl-loop for file in '("pyproject.toml" "ruff.toml")
		       when (f-exists-p (f-join root file))
		       do
		       (cl-return (f-join root file))
		       finally do
		       (error "No config file found")))))
      (unless (-some (lambda (arg) (equal arg "--config")) args)
	(setopt lsp-ruff-ruff-args (append args
					   (list "--config"
						 config-path))))
      (message "Ruff args updated: %s" lsp-ruff-ruff-args))))
(--ignore
  (prog1 nil
    (with-current-buffer (get-buffer-create "*output*")
      (emacs-lisp-mode)
      (cl-prettyprint (symbol-function #'python-ext--lint-format-buffer))
      (run-with-idle-timer 0.2 nil #'activate-view-mode 1)
      (set-buffer-modified-p nil))
    (pop-to-buffer "*output*" t)
    (call-interactively #'menu-bar--toggle-truncate-long-lines))
  t)

(cl-defun python-ext--lint-format-buffer
    (buffer-or-name command process-name final-message &optional after)
  ;; TODO: documentation string
  (cl-check-type buffer-or-name (or buffer string))
  (cl-check-type process-name string)
  (cl-check-type final-message string)
  (cl-check-type command list)
  (let ((buffer (get-buffer buffer-or-name))
	(stdout-buffer (get-buffer-create (format "*%s::stdout*" process-name)))
	(stderr-buffer (get-buffer-create (format "*%s::stderr*" process-name))))
    (save-current-buffer
      (set-buffer stdout-buffer)
      (erase-buffer)
      (set-buffer stderr-buffer)
      (erase-buffer))
    (unless (and buffer (buffer-live-p buffer))
      (error "Buffer %S is dead or nonexistent" buffer))
    (save-current-buffer
      (set-buffer buffer)
      (and (buffer-modified-p buffer)		   ; raise an error either
	   (error "Buffer %S is modified" buffer)) ; because buffer is modified
      (or (derived-mode-p 'python-mode)		   ; or because it is not in Python mode
	  (error "Not in a Python buffer."))	   ;
      (let* ((infile
	      (let ((it (buffer-file-name buffer)))
		;; Input buffer must be associated with a file
		(prog1 it
		  (or (file-exists-p it)
		      (error "Buffer %s is not associated with a file" buffer)))))
	     (-auto-revert-mode auto-revert-mode)
	     (cmd `(,@command
		    "--output-format"
		    "json"
		    "--exit-zero"
		    ,@(let ((config (python-ext-lint-config)))
			(and config (list "--config" config)))
		    ,infile))
	     (proc
	      (make-process
	       :name process-name
	       :command cmd
	       :noquery t
	       :buffer stdout-buffer
	       :stderr stderr-buffer
	       :sentinel
	       (lambda (process event)
		 (when (string-match-p "^\\(exited abnormally\\|failed\\|finished\\)" event)
		   ;; Reenable auto revert mode if it was previously active,
		   ;; display the buffer in the event of an error, then revert
		   ;; the current buffer
		   (with-current-buffer buffer
		     (revert-buffer 'ignore-auto t t))
		   (run-with-timer 1 nil #'auto-revert-mode -auto-revert-mode)
		   (if (= (process-exit-status process) 0)
		       (cl-ext-progn
			 (message final-message buffer)
			 (and after (funcall after)))
		     (message "Process %s reported an error, see %s for details"
			      process-name
			      stderr-buffer)))))))
	(auto-revert-mode 0)))))

(defun python-ext--lint-format-show-help-buffer (json-string buffer help-buffer)
  ;; TODO: documentation string
  (cl-check-type buffer buffer)
  (cl-check-type help-buffer string)
  (let ((root (python-ext-project-get-root))
	(data (json-parse-string json-string))
	lines)
    (cl-flet* ((gethashr
		(hash &rest keys)
		(cl-loop with value = hash
			 for key in keys
			 do
			 (if (hash-table-p value)
			     (setq value (gethash key value))
			   (cl-return value))
			 finally return value))
	       (format-line
		(hash)
		(format "%s %s
  %s %s:%d:%d"
			(propertize (gethash "code" hash)
				    'face 'user-ext-python-lint-keyword-face)
			(propertize (gethash "message" hash)
				    'face 'user-ext-python-lint-summary-face)
			(propertize "-->" 'face 'user-ext-python-lint-constant-face)
			(f-relative (gethash "filename" hash) root)
			(gethashr hash "location" "row")
			(gethashr hash "location" "column"))))
      (with-current-buffer buffer
	(setq lines
	      (cl-loop with str
		       for h across data
		       collect
		       (format-line h))))
      (with-help-window help-buffer
	(with-current-buffer help-buffer
	  (insert (format "Lint results for %s
Root directory: %s\n\n"
			  (abbreviate-file-name (buffer-file-name buffer))
			  root))
	  (insert (s-join "\n\n" lines)))))))
(--ignore
  (prog1 nil
    (with-current-buffer (get-buffer-create "*output*")
      (emacs-lisp-mode)
      (cl-prettyexpand '(with-help-window help-buffer
			  (erase-buffer)
			  (insert (s-join "\n\n" lines))))
      (run-with-idle-timer 0.2 nil #'activate-view-mode 1)
      (set-buffer-modified-p nil))
    (pop-to-buffer "*output*" t))

  (let ((str "[
  {
    \"cell\": null,
    \"code\": \"PLC0415\",
    \"end_location\": {
      \"column\": 18,
      \"row\": 86
    },
    \"filename\": \"/home/john/github/tmux-macros/utils.py\",
    \"fix\": null,
    \"location\": {
      \"column\": 5,
      \"row\": 86
    },
    \"message\": \"`import` should be at the top-level of a file\",
    \"noqa_row\": 86,
    \"url\": \"https://docs.astral.sh/ruff/rules/import-outside-top-level\"
  }
]")
	(default-directory "~/github/tmux-macros"))
    (python-ext--lint-format-show-help-buffer
     str
     (get-buffer "utils.py")
     user-ext-python-ruff-lint-help-buffer))
  t)

(defun python-ext-lint-config ()
  "Return the path of a Ruff config file."
  (cl-loop with file
	   with root = (python-ext-project-get-root)
	   with default-directory = root
	   for base in '("ruff.toml" ".ruff.toml" "pyproject.toml")
	   do
	   (setq file (f-join root base))
	   (when (f-exists-p file)
	     (cl-return file))
	   finally return nil))

(defun python-ext-lint-buffer (&optional buffer-or-name)
  "Lint a buffer BUFFER-OR-NAME using Ruff.
BUFFER-OR-NAME may be a buffer, a string (buffer name), or
nil for the current buffer.  If BUFFER-OR-NAME refers to a
dead buffer, or one whose major mode does not derived from
`python-mode', then an error is raised.

When called interactively, the user is prompted for a buffer
name from a list of the currently live (i.e., active) buffers."
  (interactive "bLint Buffer: ")
  (cl-check-type buffer-or-name (or buffer string null))
  (let ((buffer (cl-etypecase buffer-or-name
		  (string (get-buffer buffer-or-name))
		  (buffer buffer-or-name)
		  (null (current-buffer))))
	(default-directory (project-root (python-ext-project-current))))
    (python-ext--lint-format-buffer
     buffer
     user-ext-python-ruff-lint-command
     "ruff-lint"
     "Linted buffer %S"
     (lambda (stdout-buffer)
       (python-ext--lint-format-show-help-buffer
	(with-current-buffer stdout-buffer
	  (buffer-string-no-properties))
	buffer
	user-ext-python-ruff-lint-help-buffer)))))

(defun python-ext-lint-current-buffer ()
  "Lint the current buffer.
This calls `python-ext-lint-buffer' with a nil argument."
  (interactive)
  (python-ext-lint-buffer))

(defun python-ext-format-buffer (&optional buffer-or-name)
  "Format a buffer BUFFER-OR-NAME using Ruff.
BUFFER-OR-NAME may be a buffer, a string (buffer name), or
nil for the current buffer.  If BUFFER-OR-NAME refers to a
dead buffer, or one whose major mode does not derive from
`python-mode', then an error is raised.

When called interactively, the user is prompted for a buffer
name from a list of the currently live (i.e., active) buffers."
  (interactive "*bFormat Buffer: ")
  (defvar auto-revert-mode)
  (cl-check-type buffer-or-name (or buffer string null))
  (let ((buffer (cl-etypecase buffer-or-name
		  (string (get-buffer buffer-or-name))
		  (buffer buffer-or-name)
		  (null (current-buffer))))
	(default-directory (project-root (python-ext-project-current))))
    ;; TODO: call `python-ext--lint-format-buffer'
    (error "Not implemented")))

(defun python-ext-format-current-buffer ()
  "Format the current buffer.
This calls `python-ext-format-buffer' with a nil argument."
  (interactive)
  (python-ext-format-buffer))


;; --- Docstrings

(defun python-ext--extract-docstring ()
  "Extract the docstring at point if inside one.
Return a list of the form (CONTENT START END), which
indicates the docstring and where it was found in relation
to the current buffer.

START is the beginning of the docstring (after the triple
quotes), END is the end of the docstring (before the triple
quotes), and CONTENT is the text between START and END."
  (when-let ((cl-x (python-ext--string-at-pos))
	     (beg (python-ext--docstring-content-start cl-x))
	     (end (python-ext--docstring-content-end cl-x))
	     (content (buffer-substring-no-properties beg end)))
    (list content beg end)))

(defsubst python-ext-docstring--clear-vars ()
  (setq user-ext-python--orig-position nil
	user-ext-python--orig-position nil))

(defun python-ext--write-docstring ()
  "Exit the Python docstring buffer and apply the docstring."
  (interactive)
  (let (num-lines prefix buffer-string)
    (deactivate-mark)
    (untabify (point-min) (point-max))
    (setq num-lines (count-lines (point-min) (point-max)) ; count number of lines
	  buffer-string (let ((s (buffer-string)))
			  (set-text-properties 0 (length s) nil s)
			  s))
    (when (string-blank-p buffer-string)
      ;; string is empty; exit
      (kill-buffer)
      (jump-to-register user-ext-python--register)
      (setq user-ext-python--orig-position nil)
      (error "Docstring is empty"))
    (kill-buffer)
    (jump-to-register user-ext-python--register)
    (insert buffer-string)
    (cl-ext-check-type user-ext-python--orig-position integer-or-marker)
    (goto-char user-ext-python--orig-position) ; go back to original position
    (save-excursion
      ;; get leading spaces
      (beginning-of-line)
      (setq prefix
	    (if (looking-at "^\\([ \t]+\\)")
		(match-string 1)
	      "")))
    ;; go to the beginning of each line and indent it
    ;; starting on the second line
    (move-beginning-of-line 2)
    (dotimes (_i (1- num-lines))
      (if (looking-at-p "[ \t]*\n")	; check if line is blank
	  (move-beginning-of-line 2)	; if blank, go to next line
	(insert prefix)			; otherwise, insert prefix
	(move-beginning-of-line 2)))
    (python-ext-docstring--clear-vars)))

(defun python-ext--cancel-docstring ()
  (interactive))

(defmacro python-ext--push-mode (mode place)
  `(progn
     (when (and (boundp ',mode) ,mode)
       (cl-pushnew ',mode ,place))))

(defun python-ext--dedent-string (string)
  "Dedent STRING."
  (let* ((lines (split-string string "\n"))
	 (common-indent
	  (cl-loop
	   for line in (seq-filter (lambda (line) ; return list of non-empty lines
				     (not (string-empty-p line))) ;
				   lines)			  ;
	   with indent = most-positive-fixnum
	   do
	   (setq indent (min indent
			     (progn
			       (string-match "^[ \t]*" line)
			       (match-end 0))))
	   finally return indent)))
    (mapconcat (lambda (line)
		 (if (<= (length line) common-indent)
		     (string-trim-left line)
		   (substring line common-indent)))
	       lines
	       "\n")))

(defsubst python-ext-docstring--init-vars ()
  (setq user-ext-python--orig-position (point-marker)
	user-ext-python--orig-buffer (current-buffer)))

;;;###autoload
(defun python-ext-docstring ()
  "Open a temporary buffer to write a docstring.

The major mode of the buffer is controlled by the user option
`user-ext-python-docstring-major-mode'.  A list of minor
modes are enabled according to the user option
`user-ext-python-docstring-minor-modes'.

The initial fill column is controlled by the user option
`user-ext-python-docstring-fill-column'."
  (interactive)
  (let (docstring-data
	start end content)
    (python-ext-docstring--init-vars)
    (setq docstring-data (python-ext--extract-docstring))
    (unless docstring-data		     ; Not inside a docstring, so reset
      (python-ext-docstring--clear-vars)     ; vars
      (user-error "Not inside a docstring")) ;
    (setq content (string-trim (python-ext--dedent-string (car docstring-data)))
	  start (nth 1 docstring-data)
	  end (nth 2 docstring-data))
    (window-configuration-to-register user-ext-python--register)
    (python-ext--docstring-buffer content)
    (cl-assert user-ext-python--docstring-buffer)
    (with-current-buffer user-ext-python--orig-buffer
      (unless (string-blank-p content)
	;; docstring was not empty
	(goto-char start)
	(delete-region start end)
	(py-newline-and-indent)
	(py-newline-and-indent)
	(forward-line -1)
	(py-indent-line)
	(window-configuration-to-register user-ext-python--register)
	(setq user-ext-python--orig-position (point-marker))))))

(define-scratch-buffer-function python-ext--docstring-buffer "python-docstring"
				(content)
  "Python docstring buffer."
  nil
  (let ((mode user-ext-python-docstring-major-mode))
    (setq user-ext-python--docstring-buffer (current-buffer))
    (unless (string-blank-p content)
      ;; docstring is not empty
      (insert (string-trim content))
      (goto-char (point-min)))
    (funcall mode)
    (dolist (mode user-ext-python-docstring-minor-modes)
      (message "%S" `(funcall ,mode 1))
      (funcall mode 1))
    (python-ext-docstring-mode 1)
    (set-fill-column user-ext-python-docstring-fill-column)))

(define-minor-mode python-ext-docstring-mode
  "Minor mode for editing docstrings.

\\{python-ext-docstring-mode-map}"
  :lighter " Py-Docstring"
  :keymap (alist-ext-define (kbd "C-c C-c") #'python-ext--write-docstring
			    (kbd "C-c C-k") #'python-ext--cancel-docstring)
  (if python-ext-docstring-mode
      (setq header-line-format
	    (cl-ext-progn
	      (message "Type C-c C-c to save changes.")
	      (substitute-command-keys
	       "Python Docstring: Type \\[python-ext--write-docstring] to apply changes")))
    (setq header-line-format nil)))

;; ### Key bindings

(define-prefix-command 'python-ext-command-prefix)

(define-key python-mode-map (kbd "C-c i") #'python-ext-command-prefix)
(define-key python-ext-command-prefix "D" #'python-ext-docstring)

(define-key python-ext-command-prefix "c" #'python-skeleton-class)
(define-key python-ext-command-prefix "d" #'python-skeleton-def)
(define-key python-ext-command-prefix "f" #'python-skeleton-for)
(define-key python-ext-command-prefix "I" #'python-skeleton-if)
(define-key python-ext-command-prefix "m" #'python-skeleton-import)
(define-key python-ext-command-prefix "T" #'python-skeleton-try)
(define-key python-ext-command-prefix "w" #'python-skeleton-while)

(define-key python-mode-map (kbd "<S-return>") #'python-ext-shift-return)
(define-key python-mode-map (kbd "M-e")        #'yas-expand)
(define-key python-mode-map (kbd "C-c C-j")    #'imenu)

(define-key python-mode-map (kbd "C-x p r") #'python-ext-project-rename-buffer)

;; company-capf
(define-key python-mode-map (kbd "M-S-SPC") #'company-capf)
(define-key python-mode-map (kbd "C-c M-c") #'python-ext-finish-variable-type)

(define-key python-mode-map (kbd "C-c M-r") #'python-ext-revert-all-python-buffers)

(define-key python-mode-map (kbd "<") #'self-insert-command)

(eval-and-compile
  (easy-menu-define user-ext-python-menu python-mode-map
    "Python extension."
    '("Python Extension"
      ("Project"
       ["Find File" project-find-file]
       ["Rename Buffer" python-ext-project-rename-buffer])
      ("Ruff"
       ["Lint Buffer" python-ext-lint-buffer]
       ["Lint This Buffer" python-ext-lint-current-buffer]
       ["Format Buffer" python-ext-format-buffer]
       ["Format This Buffer" python-ext-format-current-buffer]))))


;; ### Abbrevs

(define-abbrev python-mode-abbrev-table "rnone" "-> None" #'abbrev-ext-insert-hook :system t)
(define-abbrev python-mode-abbrev-table "rbool" "-> bool" #'abbrev-ext-insert-hook :system t)
(define-abbrev python-mode-abbrev-table "rself" "-> Self" #'abbrev-ext-insert-hook :system t)
(define-abbrev python-mode-abbrev-table "false" "False"   #'abbrev-ext-insert-hook :system t)
(define-abbrev python-mode-abbrev-table "true" "True"     #'abbrev-ext-insert-hook :system t)


;; ### Python Shell Mode

(define-key py-shell-mode-map (kbd "<tab>") #'completion-at-point)

;;;###autoload
(defun py-shell--extra-hook () t)

;;;###autoload
(add-hook 'py-shell-mode-hook #'py-shell--extra-hook)


;; ### Hooks

(load-extension "_python-ext-syntax")

;;;###autoload
(defun python--extra-hook ()
  "Hook for `python-mode' for this extension."
  (unless user-ext-python--first-buffer-loaded
    (let ((buffer (current-buffer)))
      (run-with-idle-timer
       1
       nil
       (lambda ()
	 (with-current-buffer buffer
	   (and (y-or-n-p "This is the first Python buffer? Revert? ")
		(revert-buffer nil t))))))
    (setq user-ext-python--first-buffer-loaded t))
  (setq-local beginning-of-defun-function #'py-backward-def-or-class)
  (setq-local skeleton-further-elements
	      '((< '(backward-delete-char-untabify (min python-indent-offset
							(current-column))))
		(^ '(- (1+ (current-indentation)))))
	      hs-allow-nesting t)
  (setq comment-start "# ")
  ;; (tree-sitter-indent-mode 1)
  (tree-sitter-hl-mode 1)
  (python-ext-tree-sitter-mode 1)
  ;; (local-set-key (kbd "<") #'self-insert-command)
  ;; (origami-mode 0)
  (outline-minor-mode 0)
  (add-hook 'lsp-after-open-hook #'python--lsp-hook nil t)
  (remove-hook 'completion-at-point-functions #'py-fast-complete)
  (add-hook 'completion-at-point-functions #'py-fast-complete nil t)
  (add-hook 'before-save-hook #'python-ext--before-save nil t))

;;;###autoload
(defun python--lsp-hook ()
  "Hook for `python-mode' when lsp is enabled."
  (when (eq major-mode 'python-mode)
    (remove-hook 'completion-at-point-functions #'py-fast-complete t)))

;;;###autoload
(add-hook 'python-mode-hook #'python--extra-hook)

(provide 'python-ext)

;;; python-ext ends here

;; Local Variables:
;; eval: (local-lambda-define-self-insert-command vprefix "user-ext-python-")
;; eval: (local-lambda-define-self-insert-command fprefix "python-ext-")
;; End:

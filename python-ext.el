;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'python-mode)
(require 'python)
(require 'lsp)
(require 'function-ext)

(eval-when-compile
  (declare-function python-ext-hide-show-command "python-ext")
  (defvar python-ext-hide-show-command)
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

;; ### Functions

(defun python-ext-initialize-package (name &optional main)
  "Initialize package NAME."
  (interactive "sPackage Name: \nP")
  (cl-ext-when (y-or-n-p (format "Creating package %s in %s. Continue? "
				 name default-directory))
      (make-directory name)
    (find-file (f-join name "__init__.py"))
    (save-buffer)
    (kill-buffer)
    (cl-ext-when main
	(find-file (f-join name "__main__.py"))
      (save-buffer)
      (kill-buffer))))

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
	(cl-ext-when pos
	  (goto-char pos))))))

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
	(cl-ext-when pos
	  (goto-char pos))))))

;; ---Motion and syntax functions

(defun python-ext--beginning-of-def-p (&optional ppss)
  "If point is at the beginning of a function, return point, else nil."
  (let ((ppss (or ppss (make-ppss-easy (syntax-ppss)))))
    (when (and (not (or (ppss-comment-or-string-start ppss) ; not inside comment or string
			(ppss-innermost-start ppss)))	    ; not inside parenthesis
	       (looking-at-p user-ext-python-def-start-regexp)
	       (looking-back "[^ \t]*" (line-beginning-position))
	       (eq (current-column) (current-indentation)))
      (point))))

;; (defun python-ext--inside-def-p ()
;;   (cl-ext-save-point
;;     (py-backward-def)
;;     ))

(cl-defmacro python-ext-define-forward-form (form &key subexp alias)
  (cl-check-type form symbol)
  (cl-check-type subexp integer-or-null)
  (declare (debug (&define name
			   [&optional ":subexp" integerp]
			   [&optional ":alias" "t"])))
  (let* ((fname (intern (format "python-ext-forward-%S" form)))
	 (bof-f (intern-soft (format "python-ext--beginning-of-%S-p" form)))
	 (rx-form (intern-soft (format "user-ext-python-%S-regexp" form))))
    (if alias
	(let ((ofname (intern-soft (format "py-forward-%S" form))))
	  `(progn
	     (defalias (quote ,fname) (function ,ofname))))
      (cl-assert bof-f)
      `(progn
	 (defun ,fname ()
	   ,(format "Go to the beginning of the next `%S'.
Return point if successful, nil otherwise.

Match regular expression `%S'.
Match %s" form rx-form (if (or (not subexp) (= subexp 0))
			   "the whole expression."
			 (s-lex-format "group ${subexp}.")))
	   (if (,bof-f)
	       (point)
	     (python-ext-forward-regexp ,rx-form ,subexp)))))))

;;;###autoload (autoload 'python-ext-forward-def "python-ext" nil t)
(python-ext-define-forward-form def :alias t)

;;;###autoload (autoload 'python-ext-forward-class "python-ext" nil t)
(python-ext-define-forward-form class :alias t)

(cl-defmacro python-ext-define-backward-form (form &key subexp alias)
  (cl-check-type form symbol)
  (cl-check-type subexp integer-or-null)
  (declare (debug (&define name
			   [&optional ":subexp" integerp]
			   [&optional ":alias" "t"])))
  (let* ((fname (intern (format "python-ext-backward-%S" form)))
	 (bof-f (intern-soft (format "python-ext--beginning-of-%S-p" form)))
	 (rx-form (intern-soft (format "user-ext-python-%S-regexp" form))))
    (if alias
	(let ((ofname (intern-soft (format "py-backward-%S" form))))
	  `(progn
	     (defalias (quote ,fname) (function ,ofname))))
      (cl-assert bof-f)
      `(progn
	 (defun ,fname ()
	   ,(format "Go to the beginning of a `%S'.
Return point if successful, nil otherwise.

Match regular expression `%S'.
Match %s" form rx-form (if (or (not subexp) (= subexp 0))
			   "the whole expression."
			 (s-lex-format "group ${subexp}.")))
	   (interactive)
	   (if (,bof-f)
	       (point)
	     (python-ext-backward-regexp ,rx-form ,subexp)))))))

;;;###autoload (autoload 'python-ext-backward-def "python-ext" nil t)
(python-ext-define-backward-form def :subexp 1)

;;;###autoload (autoload 'python-ext-backward-class "python-ext" nil t)
(python-ext-define-backward-form class :alias t)

;; ---Python hs integration

;;;###autoload (autoload 'py-hide-base "python-ext")
(fext-replace-function py-hide-base "python-ext" (form &optional beg end)
  "Hide form at point."
  (hs-minor-mode 1)
  (save-excursion
    (let* ((form (prin1-to-string form))
           (beg (or beg (or (funcall (intern-soft (concat "py--beginning-of-" form "-p")))
                            (funcall (intern-soft (concat "py-backward-" form))))))
           (end (or end (funcall (intern-soft (concat "py-forward-" form)))))
           (modified (buffer-modified-p))
           (inhibit-read-only t))
      (if (and beg end)
          (progn
	    (setq beg (progn
			(goto-char beg)
			(pcase form
			  ("def"
			   (cl-ext-when (looking-at user-ext-python-def-regexp)
			     (goto-char (match-end 0))
			     (point)))
			  (_ (py-forward-statement)))))
            (hs-make-overlay beg end 'code)
            (set-buffer-modified-p modified))
        (error (concat "No " (format "%s" form) " at point"))))))

;;;###autoload
(defun python-ext-show ()
  "Toggle visibility of existing forms at point."
  (interactive)
  (hs-minor-mode 1)
  (save-excursion
    (let* ((ov (hs-overlay-at (point)))
	   (beg (and ov (overlay-start ov)))
	   (end (and ov (overlay-end ov)))
	   (modified (buffer-modified-p))
           (inhibit-read-only t))
      (when (and ov beg end)
	(hs-discard-overlays beg end))
      (set-buffer-modified-p modified))))

(defun python-ext-hide-all-functions ()
  "Hide all `def' forms in buffer, starting from the beginning."
  (interactive)
  (hs-minor-mode 1)
  (let ((inhibit-read-only t)
	(pos (point))
	beg end)
    (save-excursion
      (goto-char (point-min))
      (while (python-ext-forward-regexp user-ext-python-def-regexp 0 t)
	(setq beg (point) end (py-forward-def))
	(goto-char beg)
	(cl-ext-when (and (>= pos beg)
			  (<= pos end))
	  (setq pos beg))
	(hs-make-overlay beg end 'code)))
    (cl-ext-when pos
      (goto-char pos))))

;; ---

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

;; --- Docstrings

(defun python-ext--extract-docstring ()
  "Extract the docstring at point if inside one.
Return a list of the form (CONTENT START END), which
indicates the docstring and where it was found in relation
to the current buffer.

START is the beginning of the docstring (after the triple
quotes), END is the end of the docstring (before the triple
quotes), and CONTENT is the text between START and END."
  (cl-destructuring-bind (_ _ _ string-delim _ _ _ _ string-beg _ _) (syntax-ppss)
    (when (and string-delim
	       (save-excursion
		 (goto-char string-beg)
		 (looking-at-p "\"\\{3\\}")))
      (let* ((start string-beg)
	     (end (save-excursion
		    (goto-char start)
		    (forward-sexp)
		    (point)))
	     (content (buffer-substring-no-properties
		       (+ start 3) (- end 3))))
	(list content (+ start 3) (- end 3))))))

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
    (cl-ext-unless docstring-data	     ; Not inside a docstring, so reset
	(python-ext-docstring--clear-vars)   ; vars
      (user-error "Not inside a docstring")) ;
    (setq content (string-trim (python-ext--dedent-string (car docstring-data)))
	  start (nth 1 docstring-data)
	  end (nth 2 docstring-data))
    (window-configuration-to-register user-ext-python--register)
    (python-ext--docstring-buffer content)
    (cl-assert user-ext-python--docstring-buffer)
    (with-current-buffer user-ext-python--orig-buffer
      (cl-ext-unless (string-blank-p content)
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
    (cl-ext-unless (string-blank-p content)
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

(define-prefix-command 'python-ext-hide-show-command)
(define-key python-mode-map (kbd "C-c f") #'python-ext-hide-show-command)

(define-key python-ext-hide-show-command (kbd "#") #'py-hide-comment)
(define-key python-ext-hide-show-command (kbd "B") #'py-hide-block-or-clause)
(define-key python-ext-hide-show-command (kbd "C") #'py-hide-clause)
(define-key python-ext-hide-show-command (kbd "D") #'py-hide-def-or-class)
(define-key python-ext-hide-show-command (kbd "E") #'py-hide-elif-block)
(define-key python-ext-hide-show-command (kbd "E") #'py-hide-except-block)
(define-key python-ext-hide-show-command (kbd "S") #'py-hide-statement)
(define-key python-ext-hide-show-command (kbd "b") #'py-hide-block)
(define-key python-ext-hide-show-command (kbd "c") #'py-hide-class)
(define-key python-ext-hide-show-command (kbd "d") #'py-hide-def)
(define-key python-ext-hide-show-command (kbd "C-d") #'python-ext-hide-all-functions)
(define-key python-ext-hide-show-command (kbd "e") #'py-hide-else-block)
(define-key python-ext-hide-show-command (kbd "f") #'py-hide-for-block)
(define-key python-ext-hide-show-command (kbd "i") #'py-hide-if-block)
(define-key python-ext-hide-show-command (kbd "s") #'python-ext-show)
(define-key python-ext-hide-show-command (kbd "x") #'py-hide-expression)

;; company-capf
(define-key python-mode-map (kbd "M-S-SPC") #'company-capf)
(define-key python-mode-map (kbd "C-c M-c") #'python-ext-finish-variable-type)

(define-key python-mode-map (kbd "C-c M-r") #'python-ext-revert-all-python-buffers)

;; Abbrevs

(define-abbrev python-mode-abbrev-table "rnone" "-> None" #'abbrev-ext-insert-hook :system t)
(define-abbrev python-mode-abbrev-table "rbool" "-> bool" #'abbrev-ext-insert-hook :system t)
(define-abbrev python-mode-abbrev-table "rself" "-> Self" #'abbrev-ext-insert-hook :system t)
(define-abbrev python-mode-abbrev-table "false" "False"   #'abbrev-ext-insert-hook :system t)
(define-abbrev python-mode-abbrev-table "true" "True"     #'abbrev-ext-insert-hook :system t)

;; Hooks

;;;###autoload
(defun python--extra-hook ()
  "Hook for `python-mode' for this extension."
  (setq-local beginning-of-defun-function #'py-backward-def-or-class)
  (setq-local skeleton-further-elements
	      '((< '(backward-delete-char-untabify (min python-indent-offset
                                                 (current-column))))
		(^ '(- (1+ (current-indentation))))))
  (origami-mode -1)
  (outline-minor-mode -1)
  (add-hook 'lsp-after-open-hook #'python--lsp-hook nil t))

;;;###autoload
(defun python--lsp-hook ()
  "Hook for `python-mode' when lsp is enabled."
  (when (eq major-mode 'python-mode)
    (remove-hook 'completion-at-point-functions #'py-fast-complete t)))

;;;###autoload
(add-hook 'python-mode-hook #'python--extra-hook)

(provide 'python-ext)

;;; python-ext ends here

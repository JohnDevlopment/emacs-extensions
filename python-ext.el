;;; python-ext --- Python mode extension.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'python-mode)
(require 'python)
(require 'lsp)
(require 'function-ext)

(eval-when-compile
  (declare-function python-ext-hide-show-command "python-ext")
  (require 'dash))

;; Variables

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
  :group 'python-ext
  :safe 'symbolp)

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

(defvar user-ext-python--orig-position nil
  "Position in the original buffer when editing Python docstring.")

(defconst user-ext-python--register ?p
  "The register for `python-ext-docstring'.
This is passed to `window-configuration-to-register'.")

(defvar user-ext-python--docstring-buffer nil
  "Buffer for Python docstring.")

(defvar user-ext-python--reverted nil
  "t if `python-ext-revert-all-python-buffers' is called.")

;; Functions

;; ---Python hs integration

;;;###autoload (autoload 'py-hide-base "python-ext" nil t)
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
			(py-forward-statement)))
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
  (let ((bol (cl-save-point
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

(defun python-ext--extract-docstring ()
  "Extract the docstring at point if inside one.
Returns a list of the form (CONTENT START END), which
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

(defun python-ext--write-docstring ()
  "Exit the Python docstring buffer and apply the docstring."
  (interactive)
  (let (num-lines prefix buffer-string)
    (deactivate-mark)
    (untabify (point-min) (point-max))
    (setq num-lines (count-lines (point-min) (point-max))) ; count number of lines
    (setq buffer-string (buffer-string))
    (when (string-blank-p buffer-string)
      ;; string is empty; exit
      (kill-buffer)
      (jump-to-register user-ext-python--register)
      (setq user-ext-python--orig-position nil)
      (error "Docstring is empty"))
    (kill-buffer)
    (jump-to-register user-ext-python--register)
    (setq user-ext-python--orig-position (point))
    (insert buffer-string)
    (cl-check-type user-ext-python--orig-position marker)
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
    (setq user-ext-python--orig-position nil)))

(defmacro python-ext--push-mode (mode place)
  `(progn
     (when (and (boundp ',mode) ,mode)
       (cl-pushnew ',mode ,place))))

(defun python-ext--dedent-string (string)
  (let* ((lines (split-string string "\n"))
	 (common-indent
	  (cl-loop
	   for line in (seq-filter (lambda (line) (not (string-empty-p line))) lines)
	   with indent = most-positive-fixnum
	   do
	   (setq indent
		 (min indent (progn
			       (string-match "^[ \t]*" line)
			       (match-end 0))))
	   finally return indent)))
    (mapconcat (lambda (line)
		 (if (<= (length line) common-indent)
		     (string-trim-left line)
		   (substring line common-indent)))
	       lines
	       "\n")))

;;;###autoload
(defun python-ext-docstring ()
  "Open a temporary buffer to write a docstring."
  (interactive)
  (let ((mode user-ext-python-docstring-major-mode)
	modes
	docstring-data
	start end content)
    (python-ext--push-mode abbrev-mode modes)
    (python-ext--push-mode sphinx-doc-mode modes)
    (setq user-ext-python--orig-position (point-marker))
    ;; get docstring
    (setq docstring-data (python-ext--extract-docstring))
    (unless docstring-data
      (error "Not inside a docstring"))
    (setq content (string-trim (python-ext--dedent-string (car docstring-data)))
	  start (nth 1 docstring-data)
	  end (nth 2 docstring-data))
    (window-configuration-to-register user-ext-python--register)
    (setq user-ext-python--docstring-buffer (tmpbuf "python-docstring")) ; temp buffer
    (cl-assert (not (null user-ext-python--docstring-buffer)))		 ;
    (with-current-buffer user-ext-python--docstring-buffer
      (unless (string-blank-p content)
	;; docstring is not empty
	(insert (string-trim content))
	(goto-char (point-min)))
      (funcall mode)
      (dolist (mode modes)
	(princ `(funcall ,mode 1))
	(funcall mode 1))
      ;; set fill column and enable auto-fill
      (set-fill-column user-ext-python-docstring-fill-column)
      (auto-fill-mode 1)
      ;; set C-c C-c key to exit
      (local-set-key (kbd "C-c C-c") #'python-ext--write-docstring)
      (setq header-line-format "Python Docstring: Type C-c C-c to apply changes")
      (message "Type C-c C-c to save changes."))
    (unless (string-blank-p content)
      ;; docstring was not empty
      (goto-char start)
      (delete-region start end)
      (py-newline-and-indent)
      (py-newline-and-indent)
      (forward-line -1)
      (py-indent-line)
      (window-configuration-to-register user-ext-python--register)
      (setq user-ext-python--orig-position (point-marker)))
    (split-window-sensibly)
    (switch-to-buffer user-ext-python--docstring-buffer)))

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



;; Key bindings

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

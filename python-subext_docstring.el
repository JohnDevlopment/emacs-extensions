;; -*- lexical-binding: t; -*-

(extension-check-requires cl-ext)

;; ### Customization

(defcustom user-ext-python-docstring-major-mode 'markdown-mode
  "Major mode for editing Python docstring."
  :type 'symbol
  :safe #'symbolp
  :group 'python-ext)

(defcustom user-ext-python-docstring-minor-modes
  nil
  "Minor modes enabled when editing docstrings."
  :type '(repeat (symbol :tag "Function"))
  :options '(abbrev-mode
	     auto-fill-mode
	     display-fill-column-indicator-mode)
  :safe #'listp
  :group 'python-ext)


;; ### Variables

(define-obsolete-variable-alias
  'user-ext-python--docstring-buffer
  'user-ext-python/docstring--buffer
  "2026-01-21")
(defvar user-ext-python/docstring--buffer nil "Buffer for Python docstring.")

(define-obsolete-variable-alias
  'user-ext-python--register
  'user-ext-python/docstring--register
  "2026-01-21")
(defconst user-ext-python/docstring--register ?p
  "The register for `python-ext-docstring'.
This is passed to `window-configuration-to-register'.")

(define-obsolete-variable-alias
  'user-ext-python--orig-buffer
  'user-ext-python/docstring--original-buffer
  "2026-01-21")
(defvar user-ext-python/docstring--original-buffer nil
  "Original buffer when editing Python docstring.")

(define-obsolete-variable-alias
  'user-ext-python--orig-position
  'user-ext-python/docstring--original-position
  "2026-01-21")
(defvar user-ext-python/docstring--original-position nil
  "Position in the original buffer when editing Python docstring.")


;; ### Functions

(defun python-ext--extract-docstring ()
  "Extract the docstring at point if inside one.
What this actually does is return a `python-ext--string'
type if point is inside a string."
  (declare (side-effect-free t))
  (unless (extensionp 'python-ext 'syntax)
    (error "This requires \\=`python-ext' \\=`syntax' subextension"))
  (python-ext--string-at-pos))

(defsubst python-ext-docstring--init-vars ()
  (setq user-ext-python/docstring--original-position (point-marker)
	user-ext-python/docstring--original-buffer (current-buffer)
	user-ext-python/docstring--buffer nil))

(defsubst python-ext-docstring--clear-vars ()
  (setq user-ext-python/docstring--original-position nil
	user-ext-python/docstring--original-buffer nil
	user-ext-python/docstring--buffer nil))

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

(defun python-ext--write-docstring ()
  "Exit the Python docstring buffer and apply the docstring."
  (interactive)
  (let (num-lines prefix buffer-string)
    (deactivate-mark)
    (let ((beg (point-min))
	  (end (point-max)))
      (untabify beg end)
      (setq num-lines (count-lines beg end) ; count number of lines
	    buffer-string (let ((s (buffer-string)))
			    (set-text-properties 0 (length s) nil s)
			    s)))
    (when (string-blank-p buffer-string)
      ;; string is empty; exit
      (kill-buffer)
      (jump-to-register user-ext-python/docstring--register)
      (python-ext-docstring--clear-vars)
      (user-error "Docstring is empty"))
    (kill-buffer)
    (jump-to-register user-ext-python/docstring--register)
    (insert buffer-string)
    (cl-ext-check-type user-ext-python/docstring--original-position integer-or-marker)
    (goto-char user-ext-python/docstring--original-position) ; go back to original position
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
  "Cancel editing the docstring."
  (interactive))

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
  (let ((docstring-data (python-ext--extract-docstring))
	start end content)
    (unless docstring-data		     ; Not inside a docstring, so reset
      (python-ext-docstring--clear-vars)     ; vars
      (user-error "Not inside a docstring")) ;
    (python-ext-docstring--init-vars)	     ; Initialize vars
    (setq content (string-trim
		   (python-ext--dedent-string
		    (python-ext--string-content docstring-data)))
	  start (python-ext--string-content-start docstring-data)
	  end (python-ext--string-content-end docstring-data))
    (window-configuration-to-register user-ext-python/docstring--register)
    (python-ext--docstring-buffer content)
    (cl-assert user-ext-python/docstring--buffer)
    (with-current-buffer user-ext-python/docstring--original-buffer
      (unless (string-blank-p content)
	;; docstring was not empty
	(goto-char start)
	(delete-region start end)
	(py-newline-and-indent)
	(py-newline-and-indent)
	(forward-line -1)
	(py-indent-line)
	(window-configuration-to-register user-ext-python/docstring--register)
	(setq user-ext-python/docstring--original-position (point-marker))))))

(define-scratch-buffer-function
    python-ext--docstring-buffer "python-docstring"
    (content)
  "Python docstring buffer."
  nil
  (let ((mode user-ext-python-docstring-major-mode))
    (setq user-ext-python/docstring--buffer (current-buffer))
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
  :keymap (let ((map (make-sparse-keymap)))
	    (keymaps-ext-set-keymap map "C-c C-c" #'python-ext--write-docstring)
	    (keymaps-ext-set-keymap map "C-c C-k" #'python-ext--cancel-docstring)
	    map)
  (if python-ext-docstring-mode
      (setq header-line-format
	    (prog1 (substitute-command-keys
		    "Python Docstring: Type \\[python-ext--write-docstring] to apply changes")
	      (message "Type C-c C-c to save changes.")))
    (setq header-line-format nil)))


(cl-pushnew 'docstring user-ext-python-subextensions)
;;; python-subext_docstring.el ends here

;; Local Variables:
;; eval: (abbrev-ext-install-local-abbrev-functions)
;; eval: (abbrev-ext-define-local-abbrev "pxs" "python-ext--string")
;; eval: (abbrev-ext-define-local-abbrev "pts" "python-ext-tree-sitter")
;; eval: (abbrev-ext-define-local-abbrev "ux" "user-ext-python")
;; eval: (abbrev-ext-define-local-abbrev "uxts" "user-ext-python/tree-sitter")
;; eval: (abbrev-ext-define-local-abbrev "px" "python-ext")
;; eval: (abbrev-ext-define-local-abbrev "tse" "tree-sitter-ext")
;; End:

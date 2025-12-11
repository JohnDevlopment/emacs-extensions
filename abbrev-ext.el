;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'abbrev))

(require 'cl-ext)

;; Documentation
(eval-and-compile
  (embed-doc-document-symbol abbrev-ext
    "Extension to the abbrev system."
    :commands
    abbrev-ext-add-global-abbrev
    abbrev-ext-add-local-abbrev
    abbrev-ext-install-local-abbrev-functions
    abbrev-ext-inverse-add-local-abbrev
    :variables
    user-ext-abbrev-insert-chars
    user-ext-abbrev-insert-char-regex
    user-ext-abbrev-local-abbrevs
    user-ext-abbrev-local-table
    :functions
    abbrev-ext-define-local-abbrev
    abbrev-ext-insert-hook))


;; ### Variables

(defconst user-ext-abbrev-insert-char-regex
  (rx (or "i.e." "e.g." "A.K.A." "interactively"))
  "See `abbrev-ext-insert-hook'.")

(defconst user-ext-abbrev-insert-chars
  (list ?,)
  "See `abbrev-ext-insert-hook'.")

(defvar-local user-ext-abbrev-local-table nil
  "Local abbrev table.")

(defvar-local user-ext-abbrev-local-abbrevs nil
  "Hash table of local abbrevs.")


;; ### Advice

(advice-add 'add-mode-abbrev :after #'deactivate-mark)
(advice-add 'add-global-abbrev :after #'deactivate-mark)


;; ### Functions

(defmacro abbrev-ext--define-abbrev
    (table name expansion &optional insert-hook &rest props)
  "Define abbrev NAME with EXPANSION.

Keywords:
- :save - silently save abbrevs
- :hash - add abbrev to local hash table"
  (declare (debug t) (indent defun))
  (let* ((save (cl-ext-get-keyword-with-arg props :save))
	 (hash (cl-ext-get-keyword-with-arg props :hash)))
    `(progn
       ,(when save
	  `(and save-abbrevs abbrevs-changed
		(progn
		  (if (or arg (eq save-abbrevs 'silently))
		      (write-abbrev-file nil))
		  (setq abbrevs-changed nil))))
       ,(when hash
	  `(puthash ,name ,expansion user-ext-abbrev-local-abbrevs))
       (let ((changed abbrevs-changed))
	 (unwind-protect
	     (define-abbrev ,table ,name ,expansion
	       ,insert-hook ,@props)
	   (setq abbrevs-changed changed))))))

(defsubst abbrev-ext--check-local-table ()
  (unless user-ext-abbrev-local-table
    (user-error "Call `abbrev-ext-install-local-abbrev-functions' first")))

(defsubst abbrev-ext--get-exp (arg)
  (when (>= arg 0)
    (buffer-substring-no-properties
     (point)
     (if (= arg 0)
	 (mark)
       (save-excursion (forward-word (- arg)) (point))))))

(defun abbrev-ext-local-abbrevs ()
  "Return an alist of local abbrevs."
  (cl-loop for k being the hash-keys of user-ext-abbrev-local-abbrevs
	   using (hash-values v)
	   collect
	   (cons k v)))

(defun abbrev-ext-add-local-file-local-variables ()
  "Add local abbrevs to the file-local variables list."
  (interactive)
  (barf-if-buffer-read-only)
  (let ((abbrevs (abbrev-ext-local-abbrevs))
	form)
    (when abbrevs
      (add-file-local-variable
       'eval '(abbrev-ext-install-local-abbrev-functions))
      (alist-ext-dolist (name exp abbrevs)
	(setq form `(abbrev-ext-define-local-abbrev ,name ,exp))
	(add-file-local-variable 'eval form)))))

(defun abbrev-ext-add-global-abbrev (arg)
  "Define global abbrev for last word(s) before point.
Argument is how many words before point form the expansion;
or zero means the region is the expansion.  A negative
argument means to undefine the specified abbrev."
  (interactive "p")
  (let* ((insert-hook #'abbrev-ext-insert-hook)
	 (table global-abbrev-table)
	 (exp (abbrev-ext--get-exp arg))
	 (name (read-string (if exp (format "Global temp abbrev for \"%s\": " exp)
			      "Undefine abbrev: "))))
    (set-text-properties 0 (length name) nil name)
    (when (or (null exp)
	      (not (abbrev-expansion name table))
	      (y-or-n-p (format "%s expands to \"%s\"; redefine? "
				name (abbrev-expansion name table))))
      (abbrev-ext--define-abbrev
	table (downcase name) exp insert-hook :system t))
    (deactivate-mark)))
(function-put #'abbrev-ext-add-global-abbrev 'interactive-only
	      "use `abbrev-ext-define-local-abbrev' instead.")


;; --- Initialization

(defun abbrev-ext-install-local-abbrev-functions ()
  "\"Install\" local abbrev extension, allowing for local abbrevs.
More precisely, enable abbrevs that are only available in the
current buffer."
  (interactive)
  (or
   user-ext-abbrev-local-table
   (let ((ac abbrevs-changed))
     (setq user-ext-abbrev-local-table (copy-abbrev-table local-abbrev-table)
	   user-ext-abbrev-local-abbrevs (make-hash-table)
	   local-abbrev-table user-ext-abbrev-local-table)
     (message "Copied local abbrev table")
     (setq abbrevs-changed ac))))


;; --- Hooks

(defmacro abbrev-ext-hook (&rest body)
  "Return an anonymous function for use as an abbrev hook.
The function defined will always return t. See `lambda' for
the details on making anonymous functions.

It is required that `lexical-binding' be set for this to
work.

\(fn [DOCSTRING] BODY)"
  (declare (indent defun)
	   (debug (&define lambda-list lambda-doc
			   [&optional ("interactive" interactive)]
			   def-body)))
  (let ((first-form (pop body))
	docstring)
    (if (stringp (setq docstring first-form))
      (cl-ext-progn
	`(lambda nil ,docstring
	   (prog1 t
	     ,@body)))
      `(lambda nil nil
	 (prog1 t
	   ,first-form
	   ,@body)))))

(defun abbrev-ext-insert-hook ()
  "Return nil if the expansion trigger character should be inserted.

Return nil under one of the following conditions:
- The trigger character is either a space or a hyphen (-).
- The expansion matches the regular expression at
  `user-ext-abbrev-insert-char-regex' and the trigger
  character is a member of `user-ext-abbrev-insert-chars'."
  (let ((last-char last-command-event)
	(limit (line-beginning-position)))
    (not (or (memql last-char '(?\  ?-))
	     (and (looking-back user-ext-abbrev-insert-char-regex limit)
		  (memql last-char user-ext-abbrev-insert-chars))))))
(function-put #'abbrev-ext-insert-hook 'no-self-insert t)

(defun abbrev-ext-special-hook () t)
(function-put #'abbrev-ext-special-hook 'no-self-insert t)


;; --- Local Abbrevs

(defun abbrev-ext-define-local-abbrev (name expansion)
  "Define a buffer-local abbrev for NAME to expand to EXPANSION.
NAME must be a string and should be lowercase.  EXPANSION
should usually be a string.

The arguments NAME and EXPANSION are actually the same as the
same-name arguments in `define-abbrev', which see."
  (declare (indent 1))
  (abbrev-ext--check-local-table)
  (cl-check-type name string)
  (cl-check-type expansion (or string function))
  (if (stringp expansion)
      (abbrev-ext--define-abbrev
	local-abbrev-table name expansion #'abbrev-ext-insert-hook
	:system t)
    (abbrev-ext--define-abbrev local-abbrev-table name
      "" #'abbrev-ext-special-hook :system t :hash t)))
(--ignore
 (prog1 nil
   (with-current-buffer (get-buffer-create "*output*")
     (emacs-lisp-mode)
     (cl-prettyprint (symbol-function #'abbrev-ext-define-local-abbrev))
     (run-with-idle-timer 0.5 nil #'activate-view-mode 1)
     (set-buffer-modified-p nil))
   (pop-to-buffer "*output*" t))
 t)

(defun abbrev-ext-add-local-abbrev (arg)
  "Define buffer-local abbrev for last word(s) before point.
Argument is how many words before point form the expansion;
or zero means the region is the expansion.  A negative
argument means to undefine the specified abbrev."
  (interactive "p")
  (abbrev-ext--check-local-table)
  (let* ((table local-abbrev-table)
	 (insert-hook #'abbrev-ext-insert-hook)
	 (exp (abbrev-ext--get-exp arg))
	 (name (read-string (if exp (format "Local abbrev for \"%s\": " exp)
			      "Undefine abbrev: "))))
    (set-text-properties 0 (length name) nil name)
    (and (or (null exp)
	     (not (abbrev-expansion name table))
	     (y-or-n-p (format "%s expands to \"%s\"; redefine? "
			       name (abbrev-expansion name table))))
	 (abbrev-ext--define-abbrev
	  table (downcase name) exp insert-hook :system t :hash t)))
  (deactivate-mark))
(function-put #'abbrev-ext-add-local-abbrev 'interactive-only
	      "use `abbrev-ext-define-local-abbrev' instead.")

(defun abbrev-ext-inverse-add-local-abbrev (arg)
  "Define buffer-local abbrev."
  (interactive "p")
  (abbrev-ext--check-local-table)
  (let* ((table local-abbrev-table)
	 (insert-hook #'abbrev-ext-insert-hook)
	 exp name start end)
    (save-excursion
      (forward-word (1+ (- arg)))
      (skip-syntax-backward "^w")
      (setq end (point))
      (backward-word 1)
      (setq start (point)
	    name (buffer-substring-no-properties start end)))
    (setq exp (read-string (format "Local expansion for \"%s\": " name)
			   nil t nil t))
    (when (or (not (abbrev-expansion name))
	      (y-or-n-p (format "%s expands to \"%s\"; redefine? "
				name (abbrev-expansion name))))
      (abbrev-ext--define-abbrev table (downcase name)
	exp insert-hook :system t :hash t)
      (save-excursion
	(goto-char end)
	(expand-abbrev)))
    (deactivate-mark)))


;; ### Keymap

(bind-keys ("C-x a L" . abbrev-ext-add-local-abbrev)
	   ("C-x a I" . abbrev-ext-install-local-abbrev-functions)
	   ("C-x a i L" . abbrev-ext-inverse-add-local-abbrev)
	   ("C-x a G" . abbrev-ext-add-global-abbrev))


;; ### System Abbrevs

(define-abbrev global-abbrev-table "ie" "i.e." #'abbrev-ext-insert-hook :system t)
(define-abbrev global-abbrev-table "eg" "e.g." #'abbrev-ext-insert-hook :system t)
(define-abbrev global-abbrev-table "aka" "A.K.A." #'abbrev-ext-insert-hook :system t)
(define-abbrev global-abbrev-table "regex" "regular expression" #'abbrev-ext-insert-hook :system t)
(define-abbrev global-abbrev-table "regexs" "regular expressions" #'abbrev-ext-insert-hook :system t)

;; Local Variables:
;; eval: (abbrev-ext-install-local-abbrev-functions)
;; eval: (abbrev-ext-define-local-abbrev "ax" "abbrev-ext")
;; eval: (abbrev-ext-define-local-abbrev "ux" "user-ext-abbrev")
;; End:

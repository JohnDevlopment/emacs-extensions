;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'abbrev)
  (require 'cl-ext))

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

;; ### Advice

(advice-add 'add-mode-abbrev :after #'deactivate-mark)
(advice-add 'add-global-abbrev :after #'deactivate-mark)

;; ### Functions

(defsubst abbrev-ext--check-local-table ()
  (cl-ext-unless user-ext-abbrev-local-table
      (user-error "Call `abbrev-ext-install-local-abbrev-functions' first")))

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

(defun abbrev-ext-add-local-abbrev (arg)
  "Define buffer-local abbrev for last word(s) before point.
Argument is how many words before point form the expansion;
or zero means the region is the expansion.  A negative
argument means to undefine the specified abbrev."
  (interactive "p")
  (abbrev-ext--check-local-table)
  (let* ((table local-abbrev-table)
	 (insert-hook #'abbrev-ext-insert-hook)
	 (exp (cl-ext-when (>= arg 0)
		  (buffer-substring-no-properties
		   (point)
		   (if (= arg 0)
		       (mark)
		     (save-excursion (forward-word (- arg)) (point))))))
	 (name (read-string (if exp (format "Local abbrev for \"%s\": " exp)
			      "Undefine abbrev: "))))
    (set-text-properties 0 (length name) nil name)
    (cl-ext-when (or (null exp)
		     (not (abbrev-expansion name table))
		     (y-or-n-p (format "%s expands to \"%s\"; redefine? "
				       name (abbrev-expansion name table))))
	(define-abbrev table (downcase name) exp insert-hook :system t)))
  (deactivate-mark))

(defun abbrev-ext-add-global-abbrev (arg)
  "Define global abbrev for last word(s) before point.
Argument is how many words before point form the expansion;
or zero means the region is the expansion.  A negative
argument means to undefine the specified abbrev."
  (interactive "p")
  (let* ((insert-hook #'abbrev-ext-insert-hook)
	 (table global-abbrev-table)
	 (exp (cl-ext-when (>= arg 0)
		  (buffer-substring-no-properties
		   (point)
		   (if (= arg 0)
		       (mark)
		     (save-excursion (forward-word (- arg)) (point))))))
	 (name (read-string (if exp (format "Local abbrev for \"%s\": " exp)
			      "Undefine abbrev: "))))
    (set-text-properties 0 (length name) nil name)
    (cl-ext-when (or (null exp)
		     (not (abbrev-expansion name table))
		     (y-or-n-p (format "%s expands to \"%s\"; redefine? "
				       name (abbrev-expansion name table))))
	(define-abbrev table (downcase name) exp insert-hook :system t))
    (deactivate-mark)))

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
    (cl-ext-when (or (not (abbrev-expansion name))
		     (y-or-n-p (format "%s expands to \"%s\"; redefine? "
				       name (abbrev-expansion name))))
	(define-abbrev table (downcase name) exp insert-hook :system t)
      (save-excursion
	(goto-char end)
	(expand-abbrev)))
    (deactivate-mark)))

(defun abbrev-ext-define-local-abbrev (name expansion)
  "Define a buffer-local abbrev for NAME to expand to EXPANSION.
NAME must be a string and should be lowercase.  EXPANSION
should usually be a string.

The arguments NAME and EXPANSION are actually the same as the
same-name arguments in `define-abbrev', which see."
  (abbrev-ext--check-local-table)
  (cl-check-type name string)
  (define-abbrev local-abbrev-table name expansion
    #'abbrev-ext-insert-hook :system t))

(defun abbrev-ext-install-local-abbrev-functions ()
  "\"Install\" local abbrev extension, allowing for local abbrevs.
More precisely, enable abbrevs that are only available in the
current buffer."
  (interactive)
  (cl-ext-unless user-ext-abbrev-local-table
      (setq user-ext-abbrev-local-table (copy-abbrev-table local-abbrev-table)
	    local-abbrev-table user-ext-abbrev-local-table)
    (message "Copied local abbrev table")))

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

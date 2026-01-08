;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'dash))


;; ### Variables

(eval-and-compile
  (defconst user-ext-thing-at-point-valid-things
    '(string)
    "A list of valid things in this extension."))


;; ### Functions

(defun copy-thing-at-point (thing)
  "Copy the thing at point."
  (interactive (->>
		(completing-read "What thing? "
				 '("defun" "email" "filename" "line" "list"
				   "number" "page" "sentence" "sexp" "symbol"
				   "url" "uuid" "whitespace" "word"))
		intern
		list))
  (cl-check-type thing symbol)
  (let ((type thing)
	(thing (thing-at-point thing t)))
    (or thing
	(user-error "Failed to get %S at point" type))
    (kill-new thing)
    (message "Copied: %s" (current-kill 0 t))))

(defun kill-thing-at-point (thing)
  "Kill the thing at point."
  (pcase (bounds-of-thing-at-point thing)
    (`(,start . ,end)
     (kill-region start end))
    (form
     (or (not form)
	 (error "Invalid form %S" form)))))

(defun valid-thing (thing)
  "Return non-nil if THING is a valid thing.
THING is valid or not depending on `thing-at-point', which
see."
  (cl-check-type thing symbol)
  (memq thing (eval-when-compile
		(append '(defun email filename line list number page sentence sexp symbol
				url uuid whitespace word)
			user-ext-thing-at-point-valid-things))))

(defmacro define-copy-thing-command (thing &optional docstring)
  "Define a command to copy THING at point to the kill ring.
THING must be a valid thing according to `thing-at-point',
which see.

See also `valid-thing'."
  (declare (debug (&define name [&optional stringp]))
	   (doc-string 2))
  (cl-check-type thing symbol)
  (cl-check-type docstring string-or-null)
  (and (eq thing 'thing)
       (signal-invalid-argument
	thing
	"`thing' is not allowed as it would shadow `copy-thing-at-point'"))
  (or (valid-thing thing)
      (signal-invalid-argument thing "Must be a valid thing (see `thingatpt')"))
  (let ((fname (intern (format "copy-%S-at-point" thing)))
	(docstring (or docstring (format "Copy the thing `%S' at point." thing))))
    `(progn
       (defun ,fname ()
	 ,docstring
	 (interactive)
	 (copy-thing-at-point ',thing)))))

(defmacro define-kill-thing-command (thing &optional docstring)
  "Define a command to copy THING at point to the kill ring.
THING must be a valid thing according to `thing-at-point',
which see.

See also `valid-thing'."
  (declare (debug (&define name [&optional stringp]))
	   (doc-string 2))
  (cl-check-type thing symbol)
  (cl-check-type docstring string-or-null)
  (and (eq thing 'thing)
       (signal-invalid-argument
	thing
	"`thing' is not allowed as it would shadow `kill-thing-at-point'"))
  (or (valid-thing thing)
      (signal-invalid-argument thing "Must be a valid thing (see `thingatpt')"))
  (let ((fname (intern (format "kill-%S-at-point" thing)))
	(docstring (or docstring
		       (format "Kill (\"cut\") the thing `%S' at point." thing))))
    `(progn
       (defun ,fname ()
	 ,docstring
	 (interactive)
	 (kill-thing-at-point ',thing)))))

(define-copy-thing-command symbol)
(define-copy-thing-command sexp)
(define-copy-thing-command filename)
(define-copy-thing-command word)
(define-copy-thing-command url)

(define-kill-thing-command symbol)
(define-kill-thing-command sexp)
(define-kill-thing-command filename)
(define-kill-thing-command word)
(define-kill-thing-command url)


;; --- string

(defun thing-at-point-ext-find-unescaped-string (char &optional limit pos)
  "Move point forward across chars that are not quote syntax.
Stop before an unescaped quote character, or at position
LIMIT.  If POS is non-nil, move position to POS.

This function returns the position found, or nil if no such
char was found."
  (let (qpos)
    (and pos (goto-char pos))
    (setq limit (or limit (point-max)))
    (until (or (>= (point) limit) (eobp) qpos)
      (skip-syntax-forward "^\"" limit)
      (cl-ext-cond
	  ((eql (char-before) ?\\)
	   (forward-char))
	((eql (char-after) char)
	 (setq qpos (point)))
	(t (forward-char))))
    qpos))

(defun thing-at-point-ext-bounds-of-string-at-point ()
  "Return the bounds of string at point."
  (let* ((pps (make-ppss-easy (syntax-ppss)))
	 (char (ppss-string-terminator pps))
	 (start (ppss-comment-or-string-start pps))
	 end)
    (when char
	(setq end (save-excursion
		    (goto-char start)
		    (forward-char 1)
		    (if (eql (char-after) ?\")
			(1+ (point))
		      (skip-syntax-forward "\"")
		      (thing-at-point-ext-find-unescaped-string char)
		      (skip-syntax-forward "\"")
		      (point))))
      (cons start end))))

(defun thing-at-point-ext-string-at-point ()
  "Return the string at point."
  (let ((bounds (thing-at-point-ext-bounds-of-string-at-point)))
    (when bounds
      (-let (((start . end) bounds))
	(buffer-substring start end)))))

(put 'string 'thing-at-point #'thing-at-point-ext-string-at-point)
(put 'string 'bounds-of-thing-at-point #'thing-at-point-ext-bounds-of-string-at-point)

(define-copy-thing-command string)
(define-kill-thing-command string)


;; ### Keymap

(define-prefix-command 'thingatpt-ext-map)
(global-set-key (kbd "C-c t") #'thingatpt-ext-map)
(define-key thingatpt-ext-map (kbd "_") #'copy-symbol-at-point)
(define-key thingatpt-ext-map (kbd "(") #'copy-sexp-at-point)
(define-key thingatpt-ext-map (kbd "f") #'copy-filename-at-point)
(define-key thingatpt-ext-map (kbd "w") #'copy-word-at-point)
(define-key thingatpt-ext-map (kbd "\"") #'copy-string-at-point)
(define-key thingatpt-ext-map (kbd "u") #'copy-url-at-point)

(define-prefix-command 'thingatpt-ext-kill-map)
(define-key thingatpt-ext-map (kbd "<delete>") #'thingatpt-ext-kill-map)
(define-key thingatpt-ext-kill-map (kbd "_") #'kill-symbol-at-point)
(define-key thingatpt-ext-kill-map (kbd "(") #'kill-sexp-at-point)
(define-key thingatpt-ext-kill-map (kbd "f") #'kill-filename-at-point)
(define-key thingatpt-ext-kill-map (kbd "w") #'kill-word-at-point)
(define-key thingatpt-ext-kill-map (kbd "\"") #'kill-string-at-point)
(define-key thingatpt-ext-kill-map (kbd "u") #'kill-url-at-point)


(extension-provide 'thingatpt-ext)
;;; thingatpt-ext.el ends here

;; Local Variables:
;; eval: (local-lambda-define-self-insert-command fprefix "thing-at-point-ext-")
;; eval: (local-lambda-define-local-defun test-string-at-point nil "Test go the next string in buffer." (interactive) (cl-ext-save-point (skip-syntax-forward "^\"") (cl-ext-unless (char-equal (char-after) 34) (error "No strings after point")) (right-char 1) (thing-at-point 'string)))
;; eval: (abbrev-ext-install-local-abbrev-functions)
;; eval: (abbrev-ext-define-local-abbrev "ux" "user-ext-thing-at-point-")
;; eval: (abbrev-ext-define-local-abbrev "tapx" "thing-at-point-ext")
;; End:

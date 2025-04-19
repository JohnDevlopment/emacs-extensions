;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'dash))

;; Variables

(defconst user-ext-thingatpt-valid-things
  '(string))

(define-obsolete-variable-alias
  'user-ext-thingatpt-valid-things
  'user-ext-thing-at-point-valid-things
  "2025-03-31")

;; Functions

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
  (let ((thing (thing-at-point thing t)))
    (kill-new thing)
    (message "Copied: %s" (current-kill 0 t))))

(defun kill-thing-at-point (thing)
  "Kill the thing at point."
  (pcase (bounds-of-thing-at-point thing)
    (`(,start . ,end)
     (kill-region start end))
    (form
     (cl-ext-unless (not form)
       (error "Invalid form %S" form)))))

(defun valid-thing (thing)
  "Return non-nil if THING is a valid thing.
THING is valid or not depending on `thing-at-point', which
see."
  (cl-check-type thing symbol)
  (memq thing (eval-when-compile
		(append '(defun email filename line list number page sentence sexp symbol
				url uuid whitespace word)
			user-ext-thingatpt-valid-things))))

(defmacro define-copy-thing-command (thing &optional docstring)
  "Define a command to copy THING at point to the kill ring.
THING must be a valid thing according to `thing-at-point',
which see.

See also `valid-thing'."
  (declare (debug (&define name [&optional stringp]))
	   (doc-string 2))
  (cl-check-type thing symbol)
  (cl-check-type docstring string-or-null)
  (cl-ext-when (eq thing 'thing)
    (signal-invalid-argument thing "`thing' is not allowed as it would shadow `copy-thing-at-point'"))
  (cl-ext-unless (valid-thing thing)
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
  (cl-ext-when (eq thing 'thing)
    (signal-invalid-argument thing "`thing' is not allowed as it would shadow `kill-thing-at-point'"))
  (cl-ext-unless (valid-thing thing)
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

(define-kill-thing-command symbol)
(define-kill-thing-command sexp)
(define-kill-thing-command filename)
(define-kill-thing-command word)

;; --- `string'

(defun thing-at-point-ext-bounds-of-string-at-point ()
  (let* ((pps (make-ppss-easy (syntax-ppss)))
	 (char (ppss-string-terminator pps))
	 (start (ppss-comment-or-string-start pps))
	 end)
    (cl-ext-when char
      (setq end (cl-ext-save-point
		  (goto-char start)
		  (skip-syntax-forward "\"")
		  (skip-syntax-forward "^\"")
		  (skip-syntax-forward "\"")
		  (point)))
      (cons start end))))

(defun thing-at-point-ext-string-at-point ()
  (let ((bounds (thing-at-point-ext-bounds-of-string-at-point)))
    (cl-ext-when bounds
      (-let (((start . end) bounds))
	(buffer-substring start end)))))

(put 'string 'thing-at-point #'thing-at-point-ext-string-at-point)
(put 'string 'bounds-of-thing-at-point #'thing-at-point-ext-bounds-of-string-at-point)

(define-copy-thing-command string)
(define-kill-thing-command string)

;; ---

;; ### Keymap

(eval-and-compile
  (define-prefix-command 'thingatpt-ext-map)
  (global-set-key (kbd "C-c t") #'thingatpt-ext-map)
  (define-key thingatpt-ext-map (kbd "_") #'copy-symbol-at-point)
  (define-key thingatpt-ext-map (kbd "(") #'copy-sexp-at-point)
  (define-key thingatpt-ext-map (kbd "f") #'copy-filename-at-point)
  (define-key thingatpt-ext-map (kbd "w") #'copy-word-at-point)
  (define-key thingatpt-ext-map (kbd "\"") #'copy-string-at-point)

  (define-prefix-command 'thingatpt-ext-kill-map)
  (define-key thingatpt-ext-map (kbd "<delete>") #'thingatpt-ext-kill-map)
  (define-key thingatpt-ext-kill-map (kbd "_") #'kill-symbol-at-point)
  (define-key thingatpt-ext-kill-map (kbd "(") #'kill-sexp-at-point)
  (define-key thingatpt-ext-kill-map (kbd "f") #'kill-filename-at-point)
  (define-key thingatpt-ext-kill-map (kbd "w") #'kill-word-at-point)
  (define-key thingatpt-ext-kill-map (kbd "\"") #'kill-string-at-point))

(provide 'thingatpt-ext)
;;; thingatpt-ext.el ends here

;; Local Variables:
;; eval: (local-lambda-define-self-insert-command fprefix "thing-at-point-ext-")
;; eval: (local-lambda-define-local-defun test-string-at-point nil "Test go the next string in buffer." (interactive) (cl-ext-save-point (skip-syntax-forward "^\"") (cl-ext-unless (char-equal (char-after) 34) (error "No strings after point")) (right-char 1) (thing-at-point 'string)))
;; End:

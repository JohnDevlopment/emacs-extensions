;;-*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'alist-ext)
(require 's)
(require 'cl-ext)
(require 'debug-ext)

(cl-defstruct tag-match
  "A match for a tag."
  (start nil :type integer-or-marker-p
	 :documentation "The starting position where this tag was found.")
  (end nil :type integer-or-marker-p
       :documentation "The ending position where this tag found.")
  (lstrip_blocks nil :type integerp)
  (trim_blocks nil :type integerp)
  (delimiter nil :type stringp)
  (content nil :type stringp
	   :documentation "The content of the tag."))

(defun tag-match-string (cl-x)
  "Get a string representation of CL-X.
CL-X must be an instance of the `tag-match' type."
  (cl-check-type cl-x tag-match)
  (let* ((delim (tag-match-delimiter cl-x))
	 (other-delim (pcase delim
			("{" "}")
			((or "%" "#")
			 delim))))
    (format "{%s%s %s %s%s}"
	    delim
	    (pcase (tag-match-lstrip_blocks cl-x)
	      ((and x (or ?- ?+))
	       (char-to-string x))
	      (32 "")			; 32 = char " "
	      (x (error "Invalid character \"%c\"" x)))
	    (tag-match-content cl-x)
	    (pcase (tag-match-trim_blocks cl-x)
	      ((and x (or ?- ?+))
	       (char-to-string x))
	      (32 "")			; 32 = char " "
	      (x (error "Invalid character \"%c\"" x)))
	    other-delim)))

(defsubst tag-match-from-match-data (&optional string)
  (let ((delimiter (match-string-no-properties 1 string)))
    (make-tag-match :start (match-beginning 0)
		    :end (match-end 0)
		    :delimiter delimiter
		    :content (match-string-no-properties 3 string))))

;; Customize

(defgroup jinja2 nil
  "Group for `jinja2-mode'."
  :group 'tools)

(defcustom jinja2-lighter " Jinja2"
  "Mode line."
  :group 'jinja2
  :type 'string)

(defcustom jinja2-command-prefix "C-c j"
  "Command prefix for Jinja2."
  :type 'string
  :group 'jinja2)

(defface jinja2-tag
  '((default (:inherit default :background "honeydew"))
    (((background light)) (:background "honeydew"))
    (((background dark)) (:background "lemon chiffon")))
  "Face for tags in Jinja2 mode."
  :group 'jinja2)

;; Variables

(defconst jinja2-tag-regex
  (rx ?{
      (group (any "{%#"))		; tag type delimiter
      (group (? (any ?- ?+)))		; lstrip_blocks flag
      (* (syntax whitespace))
      (group (+? anything))		; content of the tag
      (* (syntax whitespace))
      (group (? (any ?- ?+)))		; trim_blocks flag
      (any "}%#")
      ?})
  "Regular expression for tags.
Group 1 matches the first tag type delimiter.
Group 2 matches the lstrip_blocks flag.
Group 3 matches the content of the tag
Group 4 matches the trim_blocks flag.")

(defconst jinja2-tag-start-regex
  (rx ?{ (any "{%#") (? (any ?- ?+)) (* anything))
  "A regular expression for matching the beginning of a tag.
Group 1 matches the second character, which indicates what
type of tag we are dealing with.

Intended to be used with `looking-back'.")

;; Skeletons

(defun jinja2--around-skeleton ()
  )

(defmacro jinja2-define-auxiliary-skeleton (name &optional doc &rest skel)
  (declare (indent 1) (doc-string 2))
  (cl-assert (symbolp name))
  (let* ((function-name (intern (format "jinja2--skeleton-%S" name)))
	 (msg (format "Insert %s clause? " name)))
    `(progn
       (define-skeleton ,function-name
	 ,(or doc (format "Insert %s clause." name))
	 nil
	 (unless (y-or-n-p ,msg)
	   (signal 'quit t))
	 ,@skel))))

(defmacro jinja2-define-skeleton (name doc &rest skel)
  (declare (debug (sexp form body)) (indent 1) (doc-string 2))
  (cl-assert (symbolp name))
  (let* ((function-name (intern (format "jinja2-skeleton-%S" name))))
    `(progn
       (define-skeleton ,function-name
	 ,doc
	 ,@skel))))

(jinja2-define-auxiliary-skeleton else
  nil
  "{% else %}" \n \n \n -1)

(jinja2-define-skeleton if
  "Insert an if statement."
  "Condition: "
  "{% if " str " %}" \n _ \n
  ("Condition: "
   "{% elif " str " %}" \n \n)
  '(jinja2--skeleton-else)
  "{% endif %}")

(jinja2-define-skeleton for
  "Insert a for loop."
  "Variable: "
  "{% for " str " in " (setq v1 (read-string "Iterable: ")) " %}"
  \n _ \n "{% endfor %}")

(jinja2-define-skeleton macro
  "Insert a macro definition."
  "Name: "
  "{% macro " str ?\( ("Parameter, %s: "
		       (unless (equal ?\( (char-before)) ", ")
		       str)
  ") %}" \n
  _
  \n
  "{% endmacro %}")

(jinja2-define-skeleton comment
  "Insert a comment."
  nil
  "{# " (when arg "\n") _ (if arg "\n" ?\ ) "#}")

(jinja2-define-skeleton tag
  "Insert a tag."
  nil
  "{% " _ " %}")

(jinja2-define-skeleton function
  "Insert a function call."
  "Function: "
  "{% " str
  "(" ("Argument, %s: "
       (unless (= (char-before) ?\() ", ")
       str)
  resume:
  ") %}")

(jinja2-define-skeleton set
  "Insert a variable assignment."
  "Name: "
  '(setq v1 (if arg "{%-" "{%"))
  '(setq v2 (if arg "-%}" "%}"))
  v1 " set " str " = " _ ?\  v2)

(jinja2-define-skeleton block
  "Insert a block."
  "Name: "
  '(setq v1 (if arg "{%-" "{%"))
  '(setq v2 (if arg "-%}" "%}"))
  v1 " block " str ?\  v2 \n
  _ \n v1 " endblock " v2)

(defun jinja2--prompt-v1 (prompt &optional missing)
  (let ((missing (if missing
		     (s-lex-format "<undefine ${missing}>")
		   "<undefined>"))
	v1)
    (setq v1 (read-string prompt))
    (if (string-empty-p v1)
	missing
      v1)))

(defun jinja2--prompt-v2 (what prompt &optional prefix quoted)
  (cl-check-type what string-or-null)
  (cl-check-type prefix string-or-null)
  (let ((prefix (or prefix ""))
	v2)
    (when (y-or-n-p (s-lex-format "Add `${what}'? "))
      (setq v2 (read-string prompt))
      (unless (string-empty-p v2)
	(if quoted
	    (format "%s%s=\"%s\"" prefix what v2)
	  (format "%s%s=%s" prefix what v2))))))

(jinja2-define-skeleton prompt-function
  "Insert a call to the global prompt function."
  nil
  "{{- prompt(\"" (progn
		    (setq v1 (read-string "Key: "))
		    (if (string-empty-p v1)
			"<undefined key>"
		      v1))
  "\", "
  "\"" (progn
	 (setq v1 (read-string "Prompt: "))
	 (if (string-empty-p v1)
	     "<undefined prompt>"
	   v1))
  "\""
  (when (y-or-n-p "Add `type_'? ")
    ;; type_=...
    (setq v1 (read-string "Type: "))
    (unless (string-empty-p v1)
      (format ", type_=\"%s\"" v1)))
  (when (y-or-n-p "Add `default'? ")
    ;; default=...
    (setq v1 (read-string "Default (not quoted): "))
    (unless (string-empty-p v1)
      (format ", default=%s" v1)))
  ") -}}")

(jinja2-define-skeleton prompt-list-function
  "Insert a call to the global prompt-list function."
  "Key: "
  "{{- prompt_list(\"" (progn
			 (setq v1 (read-string "Key: "))
			 (if (string-empty-p v1)
			     "<undefined key>"
			   v1))
  "\", "
  "\"" (progn
	 (setq v1 (read-string "Prompt: "))
	 (if (string-empty-p v1)
	     "<undefined prompt>"
	   v1))
  "\""
  ("Prompt, %s: "
   ", (\"" str "\", \"" (jinja2--prompt-v1 "Type: ") ?\"
   (jinja2--prompt-v2 "default" "Default: " ", " t) ?\))
  resume:
  ") -}}")

(defun jinja2-insert-expression ()
  "Insert a variable expansion."
  (interactive)
  (insert "{{  }}")
  (left-char 3))

;; Functions

(defun jinja2--find-next-tag (limit &optional start)
  (cl-ext-save-point
    (when start
      (cl-check-type start integer-or-marker)
      (goto-char start))
    (let (match)
      (setq match (re-search-forward jinja2-tag-regex limit t))
      (and match (tag-match-from-match-data)))))

(defun jinja2--find-tags (start end)
  "Get a list of every tag within the range START and END."
  (let (match matches)
    (save-excursion
      (goto-char start)
      (setq matches
	    (cl-loop
	     until (>= (point) end)
	     collect (progn
		       (if (setq match (jinja2--find-next-tag end (point)))
			   (goto-char (tag-match-end match))
			 (goto-char end))
		       match))))
    matches))

(defsubst jinja2--get-visible-bounds ()
  (cons (max (window-start) (point-min))
	(min (window-end) (point-max))))

(defun jinja2-inside-tag (&optional pos)
  "If point is inside a tag, return a `tag-match' type for said tag.
If point is not inside a tag, return nil.  If POS is
non-nil, set point to that position before the search."
  (cl-check-type pos (or integer-or-marker null))
  (cl-ext-save-point
    (and pos (goto-char pos))
    (let ((bounds (jinja2--get-visible-bounds)))
      (when (and (looking-back jinja2-tag-start-regex (car bounds))
		 (goto-char (match-beginning 0))
		 (looking-at jinja2-tag-regex))
	(tag-match-from-match-data)))))

(defsubst jinja2-inside-tag-p (&optional pos)
  "If point is inside a tag, return t, else return nil.
If POS is non-nil, it specifies the starting position of the
search instead of point.

This function does not change the match data."
  (save-match-data
    (and (jinja2-inside-tag pos) t)))

(defun jinja2-overlay-at (category &optional pos)
  "Return the overlay at POS with the given CATEGORY.
Checks all overlays at POS and returns the first match with
its category property set to CATEGORY.

CATEGORY is a string which gets passed to
`jinja2--create-or-load-category'.  Its value is used to
compare with the \\=`category' property of each overlay."
  (cl-check-type category string)
  (setq category (jinja2--create-or-load-category category))
  (cl-loop
   named find
   with cat = nil
   for ov in (overlays-at (or pos (point)) t)
   when (progn
	  (setq cat (overlay-get ov 'category))
	  (and cat (eq cat category)))
   return ov))

(defun jinja2--all-overlays (category)
  "Get all overlays in the buffer."
  (setq category (jinja2--create-or-load-category category))
  (cl-destructuring-bind (beg end) (jinja2--get-visible-bounds)
    (cl-loop
     named find
     with cat = nil
     for ov in (overlays-in beg end)
     collect (progn
	       (setq cat (overlay-get ov 'category))
	       (when (and cat (eq cat category))
		 ov)))))

(defun jinja2--handle-change-deletion (pos _length)
  "Handle deletion changes."
  (let ((tag-position (jinja2-inside-tag-p pos))
	(ov (jinja2-overlay-at "tag" pos))
	(cat (jinja2--create-or-load-category "tag"))
	tag-match)
    (cond
     ((and tag-position (not ov))
      ;; Deleting the character(s) resulted in a tag
      ;; Create overlay at TAG-POSITION
      (setq tag-match (jinja2--find-next-tag (point-max) tag-position))
      (cl-assert (tag-match-p tag-match) t)
      (setq ov (make-overlay (tag-match-start tag-match)
			     (tag-match-end tag-match)))
      (overlay-put ov 'category cat))
     ((and (not tag-position) ov)
      ;; Tag is no longer valid; remove the overlay
      (delete-overlay ov))
     ((and tag-position ov)
      ;; Inside a tag but an overlay already exists
      t))))

(defun jinja2--handle-change-insertion (start _end length)
  "Handle insertion changes."
  (if (= length 1)
      (let* ((pos start)
	     (tag-position (jinja2-inside-tag-p pos))
	     (ov (jinja2-overlay-at "tag" pos))
	     (cat (jinja2--create-or-load-category "tag"))
	     tag)
	(cond
	 ((and tag-position (not ov))
	  ;; Created a tag during insertion
	  ;; Create an overlay surrounding tag
	  (setq tag (jinja2--find-next-tag (point-max) tag-position))
	  (cl-assert (tag-match-p tag) t)
	  (setq ov (make-overlay (tag-match-start tag) (tag-match-end tag)))
	  (overlay-put ov 'category cat))
	 ((and tag-position ov)
	  ;; Inside a tag but an overlay already exists
	  t)
	 ((not tag-position)
	  ;; Not inside of a tag
	  (when ov
	    (delete-overlay ov)))
	 (t (message "No conditions met."))))))

(defun jinja2--create-or-load-category (name)
  "Create or load category NAME.
Returns a symbol `category-jinja-NAME'.  NAME can be either
\"tag\" or \"expression\"."
  (let* ((valid-names '("tag" "expression"))
	 (sn (s-lex-format "category-jinja-${name}"))
	 (symbol (intern-soft sn)))
    (unless (cl-member name valid-names :test #'string=)
      (error "Invalid name '%s': can be one of %s" name (string-join valid-names ", ")))
    (if symbol
	symbol
      (setq symbol (intern sn))
      (if (string= name "tag")
	  (progn
	    (cl-assert (null (symbol-plist symbol)))
	    (setplist symbol '(evaporate t face jinja2-tag))
	    symbol)
	(error (s-lex-format "Category ${name} should not be used"))))))

(defun jinja2-clear-tags (beg end)
  "Delete all tag overlays between BEG and END.
If they are not provided, BEG and END default to the
beginning and end of buffer respectively."
  (let ((cat (jinja2--create-or-load-category "tag")))
    (dolist (ov (overlays-in beg end))
      (when (eq (overlay-get ov 'category) cat)
	(delete-overlay ov)))))

(defun jinja2-mark-tags (beg end)
  "Mark all tags within buffer."
  (let* ((cat (jinja2--create-or-load-category "tag"))
	 (tags (jinja2--find-tags beg end))
	 ov ovbeg ovend)
    ;; (print-expr sexp (list beg end))
    (with-silent-modifications
      (jinja2-clear-tags beg end)
      (dolist (tag tags)
	(when (and (tag-match-p tag)
		   (progn
		     (setq ovbeg (tag-match-start tag)
			   ovend (tag-match-end tag))
		     (not (jinja2-overlay-at "tag" ovbeg))))
	  (setq ov (make-overlay ovbeg ovend))
	  (overlay-put ov 'category cat))))))

(defun jinja2-set-tag-trim--both (pcstr choices &optional c)
  (setq c (read-char-from-minibuffer
	   (s-lex-format "lstrip_blocks and trim_blocks flags ${pcstr}: ")))
  (unless (cl-member c choices)
    (user-error "Invalid choice %c: must be one of +, -, or \" \"" c)))

(defun jinja2-set-tag-trim (&optional arg)
  "Set the lstrip_blocks and trim_blocks flags of the tag at point."
  (interactive "P")
  (let ((prompt-char
	 (lambda (prompt &optional c)
	   (setq c (read-char-from-minibuffer
		    (s-lex-format "${prompt}: (choices: +, -, or \" \")")))
	   (unless (memq c (list ?+ ?- ?\ ))
	     (user-error "Invalid character \"%s\", must be one of +, -, or \" \""))
	   c))
	tag)
    (save-excursion
      (cond ((= (char-after) ?{)
	     (when (looking-at jinja2-tag-regex)
	       (tag-match-from-match-data)))
	    ((= (char-before) ?})
	     (left-char 3)
	     (setq tag (jinja2-inside-tag)))
	    (t
	     (setq tag (jinja2-inside-tag)))))
    (when tag
      (let ((pos (point)) c1 c2)
	(if arg
	    (progn
	      (setq c1 (funcall prompt-char "lstrip_blocks and trim_blocks flags"))
	      (setf (tag-match-lstrip_blocks tag) c1
		    (tag-match-trim_blocks tag) c1))
	  (setq c1 (funcall prompt-char "lstrip_blocks flag")
		c2 (funcall prompt-char "trim_blocks flag"))
	  (setf (tag-match-lstrip_blocks tag) c1
		(tag-match-trim_blocks tag) c2))
	(kill-region (tag-match-start tag) (tag-match-end tag))
	(insert (tag-match-string tag))
	(goto-char pos)))))

;; Keybinds

(defvar jinja2-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd (concat jinja2-command-prefix " b")) #'jinja2-skeleton-block)
    (define-key map (kbd (concat jinja2-command-prefix " #")) #'jinja2-skeleton-comment)
    (define-key map (kbd (concat jinja2-command-prefix " f")) #'jinja2-skeleton-for)
    (define-key map (kbd (concat jinja2-command-prefix " F")) #'jinja2-skeleton-function)
    (define-key map (kbd (concat jinja2-command-prefix " i")) #'jinja2-skeleton-if)
    (define-key map (kbd (concat jinja2-command-prefix " m")) #'jinja2-skeleton-macro)
    (define-key map (kbd (concat jinja2-command-prefix " M-p")) #'jinja2-skeleton-prompt-function)
    (define-key map (kbd (concat jinja2-command-prefix " M-l")) #'jinja2-skeleton-prompt-list-function)
    (define-key map (kbd (concat jinja2-command-prefix " t")) #'jinja2-skeleton-tag)
    (define-key map (kbd (concat jinja2-command-prefix " s")) #'jinja2-skeleton-set)
    (define-key map (kbd (concat jinja2-command-prefix " e")) #'jinja2-insert-expression)
    (define-key map (kbd (concat jinja2-command-prefix " C-t")) #'jinja2-set-tag-trim)
    map)
  "Mode map for Jinja2.")

(defun jinja2-deactivate-all ()
  "Deactivate Jinja2 mode in all buffers."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (jinja2-mode 0))))

(defun jinja2-activate ()
  "Activate Jinja2 mode."
  (let ((bounds (jinja2--get-visible-bounds)))
    (jinja2-mark-tags (car bounds) (cdr bounds)))
  ;; (add-hook 'after-change-functions #'jinja2--set-face-on-change nil t)
  (jit-lock-register #'jinja2-mark-tags))

(defun jinja2-deactivate ()
  "Deactivate Jinja2 mode."
  (jinja2-clear-tags (point-min) (point-max))
  ;; (remove-hook 'after-change-functions #'jinja2--set-face-on-change t)
  (jit-lock-unregister #'jinja2-mark-tags))

;;;###autoload
(define-minor-mode jinja2-mode
  "Minor mode for editing Jinja2 templates within major modes.

\\{jinja2-mode-map}"
  :lighter jinja2-lighter
  :require 'jinja2
  :keymap jinja2-mode-map
  (if jinja2-mode
      (jinja2-activate)
    (jinja2-deactivate)))

(provide 'jinja2)

;;; jinja2.el ends here

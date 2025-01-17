;;; jinja2.el --- Jinja2 minor mode                  -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 's))

(require 'cl-ext)
(require 'debug-ext)

(cl-defstruct tag-match
  "A match for a tag."
  (start nil :type integer-or-marker-p)
  (end nil :type integer-or-marker-p)
  (string nil :type stringp))

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
  (rx ?{ (any "{%") (? (any ?- ?+))
      (+? nonl)
      (? (any ?- ?+)) (any "%}") ?})
  "Regular expression for tags.")

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

;; TODO: This function is incomplete
(jinja2-define-skeleton prompt-list-function
  "Insert a call to the global prompt-list function."
  "Key: "
  "{{- prompt_list(\""
  ") -}}")

(defun jinja2-insert-expression ()
  "Insert a variable expansion."
  (interactive)
  (insert "{{  }}")
  (left-char 3))

;; Functions

(when nil
  (cl-defmacro jinja2-define-insert-tag-function
      (name &key (doc (format "Insert a %s tag." name))
	    (arglist (&optional arg))
	    end-tag
	    (int-spec "P")
	    (tag (symbol-name name))
	    (tag-args nil))
    "Define a function to insert a tag.
NAME will be used for both the function name and the tag.
The rest of the arguments are keywords of the form
   [KEYWORD VALUE ...]

The following keywords are recognized:
:arglist    VALUE is the ARGLIST argument for `defun'
:doc        VALUE is the documentation string of the
            created function
:end-tag    If VALUE is provided, it specifies the ending
            tag (e.g., \"endblock\")
:int-spec   VALUE is the argument to `interactive'
:tag        VALUE is the name of the tag.  If it is not
            provided, NAME is used
:tag-args   ...

\(fn NAME &key :arglist :doc :end-tag :int-spec :tag :tag-args)"
    (declare (indent 1))
    (cl-check-type name symbol)
    (cl-check-type doc string)
    (cl-check-type end-tag (or string null))
    (when arglist
      (cl-check-type arglist list))
    (let* ((fname (intern (concat "jinja2-insert-tag-" (symbol-name name))))
	   (body (list (if int-spec
			   `(interactive ,int-spec)
			 '(interactive)))))
      (if tag-args
	  (progn
	    (setq tag (s-lex-format "{% ${tag} %}"))
	    (setq body (append body `((insert ,tag)))))
	nil)
      `(progn
	 (defun ,fname ,arglist
	   ,doc
	   ,@body))))

  (cl-prettyexpand '(jinja2-define-insert-tag-function block
		      :arglist (name &optional arg)
		      :int-spec "sName: \nP"
		      :tag-args "${name}"
		      :end-tag "endblock ${name}")))

(defun jinja2--get-leading-whitespace ()
  (cl-save-point
    (beginning-of-line)
    (when (looking-at "\\([ \t]+\\)")
      (match-string 1))))

(defun jinja2--find-next-tag (limit &optional start)
  (let (match)
    (cl-save-point
      (when start
	(cl-check-type start integer-or-marker)
	(goto-char start))
      (setq match (re-search-forward jinja2-tag-regex limit t))
      (when match
	(make-tag-match :start (match-beginning 0)
			:end (match-end 0)
			:string (match-string-no-properties 0))))))

(defun jinja2--find-tags (start end)
  (let (match matches)
    (save-excursion
      (goto-char start)
      (setq matches
	    (cl-loop
	     named find
	     until (>= (point) end)
	     collect (progn
		       (setq match (jinja2--find-next-tag end (point)))
		       (if match
			   (goto-char (tag-match-end match))
			 (goto-char end))
		       match))))
    matches))

(defsubst jinja2--get-visible-bounds ()
  (list (max (window-start) (point-min))
	(min (window-end) (point-max))))

(defun jinja2-inside-tag-p (&optional pos)
  "If POS is inside a tag, returns tag beginning, else nil.
If POS is not provided, it defaults to point."
  (cl-check-type pos (or integer null))
  (cl-save-point
    (if pos
	(goto-char pos)
      (setq pos (point)))
    (let* ((ppss (make-ppss-easy (syntax-ppss)))
	   (tl (syntax-ppss-toplevel-pos ppss)))
      (when tl
	(goto-char tl)
	(when (looking-at jinja2-tag-regex)
	  (match-beginning 0))))))

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

(when nil
  (cl-prettyprint (symbol-function 'jinja2--handle-change-deletion))
  (cl-prettyprint (symbol-function 'jinja2--handle-change-insertion)))

(defun jinja2--set-face-on-change (start end length)
  "Called when the buffer is changed."
  (save-excursion
    (save-match-data
      (cond
       ((and (= start end) (> length 0))
	;; Deletion; start and end are the same
	;; length = number of characters deleted
	(jinja2--handle-change-deletion start length))
       ((= length 0)
	;; Insertion
	(jinja2--handle-change-insertion start end (- end start)))
       (t
	;; Text is rearranged; no insertions or deletions
	;; Actually, this only when characters are replaced
	;; (e.g., via `overwrite-mode')
	"...")))))

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

(defun jinja2-clear-tags (&optional beg end)
  "Delete all tag overlays between BEG and END.
If they are not provided, BEG and END default to the
beginning and end of buffer respectively."
  (let* ((beg (or beg (point-min)))
	 (end (or end (point-max)))
	 (cat (jinja2--create-or-load-category "tag")))
    (dolist (ov (overlays-in beg end))
      (when (eq (overlay-get ov 'category) cat)
	(delete-overlay ov)))))

(defun jinja2-mark-tags ()
  "Mark all tags within buffer."
  (let* ((beg (point-min))
	 (end (point-max))
	 (cat (jinja2--create-or-load-category "tag"))
	 (tags (jinja2--find-tags beg end))
	 ov ovbeg ovend)
    (with-silent-modifications
      (jinja2-clear-tags beg end)
      (dolist (tag tags)
	(when (tag-match-p tag)
	  (setq ovbeg (tag-match-start tag)
		ovend (tag-match-end tag)
		ov (make-overlay ovbeg ovend))
	  (overlay-put ov 'category cat))))))

;; (jinja2-mark-tags)

(defun jinja2-set-tag-trim (&optional arg)
  (interactive "P")
  (cond
   ((= (char-after) ?{)
    (right-char 3))
   ((= (char-before) ?})
    (left-char 3)))
  (let* ((ppss (make-ppss-easy (syntax-ppss)))
	 (pcstr "(choices: +, -, or \" \")") ; prompt choices string
	 (choices (list ?+ ?- ?\ ))
	 beg
	 end
	 c)
    (cl-save-point
      (setq beg (progn (goto-char (syntax-ppss-toplevel-pos ppss))
		       (point-marker))
	    end (progn (goto-char beg)
		       (forward-sexp)
		       ;; (left-char 3)
		       (point-marker)))
      (goto-char beg)
      (when arg
	(setq c (read-char-from-minibuffer (s-lex-format
					    "lstrip_blocks and trim_blocks flags ${pcstr}: ")))
	(unless (cl-member c choices)
	  (user-error "Invalid choice %c: must be one of +, -, or \" \"" c)))
      (when (looking-at "{%[+-]?\\s-*")
	(unless arg
	  ;; No prefix arg; accept flag for this
	  (setq c (read-char-from-minibuffer
		   (s-lex-format "lstrip_blocks flag ${pcstr}: ")))
	  (unless (cl-member c choices)
	    (user-error "Invalid choice %c: must be one of +, -, or \" \"" c)))
	(cl-case c
	  ((?+ ?-)
	   (replace-match (format "{%%%c " c)))
	  (t
	   (replace-match "{% "))))
      (goto-char end)
      (when (looking-back "\\s-*[+-]?\\([}%]\\)}" beg t)
	(unless arg
	  ;; No prefix arg; accept flag for this
	  (setq c (read-char-from-minibuffer
		   (s-lex-format "trim_blocks flag ${pcstr}: ")))
	  (unless (cl-member c choices)
	    (user-error "Invalid choice %c: must be one of +, -, or \" \"" c)))
	(cl-case c
	  ((?+ ?-)
	   (replace-match (format " %c%%}" c)))
	  (t
	   (replace-match " %}")))))))

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
  (jinja2-mark-tags)
  (add-hook 'after-change-functions #'jinja2--set-face-on-change nil t))

(defun jinja2-deactivate ()
  "Deactivate Jinja2 mode."
  (jinja2-clear-tags)
  (remove-hook 'after-change-functions #'jinja2--set-face-on-change t))

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

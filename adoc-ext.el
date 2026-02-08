;; -*- lexical-binding: t; -*-

(require 'adoc-mode)
(require 'transient)
(require 'llama)

(check-emacs-minimum-version "27.4")

(eval-when-compile
  (require 'debug-ext))


;; ### Templates

(defmacro adoc-ext-tempo-define-template (name documentation elements &optional after)
  "Define a template NAME which inserts ELEMENTS.
The template will be called tempo-template-adoc-ext-NAME.
ELEMENTS is a list of elements recognized by
`tempo-define-template', which see.

See also `tempo-ext-tempo-handler'."
  (declare (debug t) (indent 1) (doc-string 2))
  (cl-check-type name string)
  (cl-check-type documentation string)
  (let* ((name (format "adoc-ext-%s" name))
	 (fname (intern (format "tempo-template-%s" name)))
	 (body (list `(tempo-define-template ,name ,(macroexp-quote elements)
					     nil ,documentation))))
    (cl-ext-when after
	(setq body (append body `((advice-add ',fname :after #'adoc-ext--after-tempo)))))
    `(prog1 ',fname
       ,@body)))

(adoc-ext-tempo-define-template "ifdef-github"
  "Insert a Github-only block."
  ("ifdef::env-github[]" n
   r n
   "endif::[]"))


;; --- Admonitions

;; Tip
(adoc-ext-tempo-define-template "paragraph-tip-ml"
  "Insert a multiline tip admonition."
  ("[TIP]" n "====" n r n "===="))
(adoc-ext-tempo-define-template "paragraph-tip"
  "Insert a single-line tip admonition."
  (bol "TIP: " r %))
(transient-define-suffix adoc-ext-insert-admonition-tip
  (multiline &optional arg)
  "Insert a single-line or multiline tip admonition."
  (interactive (list (adoc-ext--get-transient-arg
		      'adoc-ext-templates "--multiline")
		     (transient-scope 'adoc-ext-templates)))
  (if multiline
      (tempo-template-adoc-ext-paragraph-tip-ml arg)
    (tempo-template-adoc-ext-paragraph-tip arg)))

;; Note
(adoc-ext-tempo-define-template "paragraph-note-ml"
  "Insert a multiline note admonition."
  ("[NOTE]" n "====" n r n "===="))
(adoc-ext-tempo-define-template "paragraph-note"
  "Insert a single-line note admonition."
  (bol "NOTE: " r %))
(transient-define-suffix adoc-ext-insert-admonition-note
  (multiline &optional arg)
  "Insert a single-line or multiline note admonition."
  (interactive (list (adoc-ext--get-transient-arg
		      'adoc-ext-templates "--multiline")
		     (transient-scope 'adoc-ext-templates)))
  (if multiline
      (tempo-template-adoc-ext-paragraph-note-ml arg)
    (tempo-template-adoc-ext-paragraph-note arg)))

;; Important
(adoc-ext-tempo-define-template "paragraph-important-ml"
  "Insert a multiline important admonition."
  ("[IMPORTANT]" n "====" n r n "===="))
(adoc-ext-tempo-define-template "paragraph-important"
  "Insert a single-line important admonition."
  (bol "IMPORTANT: " r %))
(transient-define-suffix adoc-ext-insert-admonition-important
  (multiline &optional arg)
  "Insert a single-line or multiline important admonition."
  (interactive (list (adoc-ext--get-transient-arg
		      'adoc-ext-templates "--multiline")
		     (transient-scope 'adoc-ext-templates)))
  (if multiline
      (tempo-template-adoc-ext-paragraph-important-ml arg)
    (tempo-template-adoc-ext-paragraph-important arg)))

;; Warning
(adoc-ext-tempo-define-template "paragraph-warning-ml"
  "Insert a multiline warning admonition."
  ("[WARNING]" n "====" n r n "===="))
(adoc-ext-tempo-define-template "paragraph-warning"
  "Insert a single-line warning admonition."
  (bol "WARNING: " r %))
(transient-define-suffix adoc-ext-insert-admonition-warning
  (multiline &optional arg)
  "Insert a single-line or multiline warning admonition."
  (interactive (list (adoc-ext--get-transient-arg
		      'adoc-ext-templates "--multiline")
		     (transient-scope 'adoc-ext-templates)))
  (if multiline
      (tempo-template-adoc-ext-paragraph-warning-ml arg)
    (tempo-template-adoc-ext-paragraph-warning arg)))

;; Caution
(adoc-ext-tempo-define-template "paragraph-caution-ml"
  "Insert a multiline caution admonition."
  ("[CAUTION]" n "====" n r n "===="))
(adoc-ext-tempo-define-template "paragraph-caution"
  "Insert a single-line caution admonition."
  (bol "CAUTION: " r %))
(transient-define-suffix adoc-ext-insert-admonition-caution
  (multiline &optional arg)
  "Insert a single-line or multiline caution admonition."
  (interactive (list (adoc-ext--get-transient-arg
		      'adoc-ext-templates "--multiline")
		     (transient-scope 'adoc-ext-templates)))
  (if multiline
      (tempo-template-adoc-ext-paragraph-caution-ml arg)
    (tempo-template-adoc-ext-paragraph-caution arg)))


;; --- Unconstrained Quotes

(adoc-ext-tempo-define-template "monospace-uc"
  "Insert typewriter syntax."
  ("++" (r "Text: " text) "++"))

(adoc-ext-tempo-define-template "emphasis-uc"
  "Insert emphasis (usually rendered italic)."
  ("__" (r "Text: " text) "__"))

(adoc-ext-tempo-define-template "mark-uc"
  "Insert highlight."
  ("##" (r "Text: " text) "##"))

(adoc-ext-tempo-define-template "markcustom-uc"
  "Insert a custom style."
  ("[" (p "Role: " role) "]" "##" (r "Text: " text) "##"))


;; --- Constrained quotes

(adoc-ext-tempo-define-template "monospace-c"
  "Insert typewriter syntax."
  ("`pass:c[" (r "Text: " text) "]`"))

(adoc-ext-tempo-define-template "emphasis-c"
  "Insert emphasis (usually rendered italic)."
  ("_" (r "Text: " text) "_"))

(adoc-ext-tempo-define-template "code-c"
  "Insert monospaced code text."
  ("`" (r "Text: " text) "`"))

(adoc-ext-tempo-define-template "single-quotes-c"
  "Insert styled single quotes."
  ("'`" (r "Text: " text) "`'"))

(adoc-ext-tempo-define-template "double-quotes-c"
  "Insert styled double quotes."
  ("\"`" (r "Text: " text) "`\""))

(adoc-ext-tempo-define-template "markcustom-c"
  "Insert a custom style."
  ("[" (p "Role: ") "]" "#" (r "Text: " text) "#"))


;; --- Delimited blocks

;; Sidebar
(adoc-ext-tempo-define-template "block-sidebar"
  "Insert a sidebar with an optional heading."
  (bol
   (p "Header: " header no-insert)
   (S header :prefix ".") &
   (make-string 10 42)
   n (r "Text: " text) n
   (make-string 10 42)))
(transient-define-suffix adoc-ext-insert-block-sidebar
  (&optional arg)
  "Insert a sidebar with an optional header."
  (interactive (list (transient-scope 'adoc-ext-templates)))
  (tempo-template-adoc-ext-block-sidebar arg))

;; Literal block
(adoc-ext-tempo-define-template "block-literal"
  "Insert a literal block."
  (bol
   (make-string 10 ?.)
   n (r "Text: " text) n
   (make-string 10 ?.)))
(transient-define-suffix adoc-ext-insert-block-literal
  (&optional arg)
  "Insert a literal block."
  (interactive (list (transient-scope 'adoc-ext-templates)))
  (tempo-template-adoc-ext-block-literal arg))

;; Listing block
(adoc-ext-tempo-define-template "block-listing"
  "Insert a listing block."
  (bol
   (make-string 10 ?-)
   n (r "Text: " text) n
   (make-string 10 ?-)))
(transient-define-suffix adoc-ext-insert-block-listing
  (&optional arg)
  "Insert a listing block."
  (interactive (list (transient-scope 'adoc-ext-templates)))
  (tempo-template-adoc-ext-block-listing arg))

;; Table
(adoc-ext-tempo-define-template "table"
  "Insert a table."
  (bol (p "Title: " title no-insert)
       (p "Columns: " columns no-insert)
       (y-or-n-p "Header? " header)
       (S title :prefix ".") &
       (&if (or (named columns) (named header))
	    "[" (&if (flag header) "%header")
	    (&if (named columns)
		 (&if (not (- ?\[)) ",")
		 (S columns :prefix "cols=\"" :suffix "\""))
	    "]" &)
       "|===" n r n "|==="))


;; ### Functions

(defsubst adoc-ext-barf-if-not-adoc-mode ()
  "Emit an error if not in Adoc mode."
  (unless (derived-mode-p 'adoc-mode)
    (user-error "Not in adoc mode")))

(defun adoc-ext-insert-inline-image (file &optional _arg)
  "Insert an inline image a point."
  (interactive
   (cl-ext-progn
     (adoc-ext-barf-if-not-adoc-mode)
     (find-file-read-args "Find file: " t)))
  (adoc-ext-barf-if-not-adoc-mode)
  (skeleton-insert
   `(nil "image:" ,(f-relative file default-directory) "[]")))

(defun adoc-ext-insert-block-image (file &optional _arg)
  "Insert block image a point."
  (interactive
   (cl-ext-progn
     (adoc-ext-barf-if-not-adoc-mode)
     (find-file-read-args "Find file: "
			  (confirm-nonexistent-file-or-buffer))))
  (adoc-ext-barf-if-not-adoc-mode)
  (skeleton-insert
   `(nil "image::" ,(f-relative file default-directory) "[]")))

(defun adoc-ext-insert-link (href &optional description)
  "Insert a link."
  (interactive
   (let ((href (read-string "URL: " nil t))
	 (desc (read-string "Description: " nil t)))
     (list href
	   (if (string-empty-p desc) nil desc))))
  (when (string-match-p "[^a-zA-Z0-9_-]" href)
    (setq href (concat "++" href "++")))
  (skeleton-insert
   `(nil "link:" ,href "[" ,description "]")))

(defun adoc-ext--get-transient-arg (prefix arg)
  (when-let* ((args (transient-args prefix))
	      (arg (cl-member arg args :test #'string=)))
    (car arg)))

(defmacro adoc-ext-define-skeleton (name docstring &rest skel)
  "Define a skeleton command NAME.
DOCUMENTATION is that of of the command.  SKELETON is as
defined under `skeleton-insert'.

\(fn NAME DOCUMENTATION SKELETON...)"
  (declare (indent 1) (debug (&define name stringp skeleton-edebug-spec))
	   (doc-string 2))
  (cl-check-type name symbol)
  (cl-check-type docstring string)
  (let ((fname (intern (format "adoc-ext-skeleton-%S" name))))
    `(progn
       (define-skeleton ,fname
	 ,docstring
	 ,@skel))))

(transient-define-prefix adoc-ext-templates
  (&optional arg)
  ["Arguments"
   ("-l" "multiline" ("-l" "--multiline"))
   ("-s" "switch" "--switch")]
  ["Admonitions"
   ("a g" "Github Ifdef" tempo-template-adoc-ext-ifdef-github)
   ("a t" "Tip" adoc-ext-insert-admonition-tip)
   ("a n" "Note" adoc-ext-insert-admonition-note)
   ("a i" "Important" adoc-ext-insert-admonition-important)
   ("a w" "Warning" adoc-ext-insert-admonition-warning)
   ("a c" "Caution" adoc-ext-insert-admonition-caution)]
  ["Unconstrained Quotes"
   ("u _" "Emphasis (usually rendered italic)" tempo-template-adoc-ext-emphasis-uc)
   ("u +" "Monospaced text (A.K.A., typewriter)" tempo-template-adoc-ext-monospace-uc)
   ("u #" "Custom style" tempo-template-adoc-ext-markcustom-uc)]
  ["Constrained Quotes"
   ("c _" "Emphasis (usually rendered italic)" tempo-template-adoc-ext-emphasis-c)
   ("c +" "Monospaced text (A.K.A., typewriter)" tempo-template-adoc-ext-monospace-c)
   ("c `" "Monospaced code text" tempo-template-adoc-ext-code-c)
   ("c '" "Styled single quotes" tempo-template-adoc-ext-single-quotes-c)
   ("c \"" "Styled double quotes" tempo-template-adoc-ext-double-quotes-c)
   ("c #" "Custom style" tempo-template-adoc-ext-markcustom-c)]
  ["Delimited Blocks"
   ("b ." "Literal block" adoc-ext-insert-block-literal)
   ("b -" "Listing block" adoc-ext-insert-block-listing)
   ("b *" "Sidebar" adoc-ext-insert-block-sidebar)
   ("b t" "Table" tempo-template-adoc-ext-table)]
  (interactive "P")
  (transient-setup 'adoc-ext-templates nil nil :scope arg))

(define-obsolete-function-alias
  'adoc-ext-tempo-handler
  'tempo-ext-tempo-handler
  "2026-01-28")

;; (defun adoc-ext--tempo-handle-if-condition (condition &optional top-level)
;;   (let ((ivfun
;; 	 (lambda ()
;; 	   (signal-invalid-argument
;; 	    condition
;; 	    "See documentation of `adoc-ext-tempo-define-template' for valid elements")))
;; 	(cfun (lambda (cond1 &rest condn)
;; 		(cl-loop
;; 		 for cond in (cons cond1 condn)
;; 		 collect (cl-ext-progn
;; 			   (adoc-ext--tempo-handle-if-condition cond)))))
;; 	result conditions)
;;     (pcase condition
;;       (`(and ,cond1 . ,condn)
;;        (setq conditions (apply cfun cond1 condn)
;; 	     result `(and ,@conditions)))
;;       (`(or ,cond1 . ,condn)
;;        (setq conditions (apply cfun cond1 condn)
;; 	     result `(or ,@conditions)))
;;       (`(not ,cond)
;;        (setq cond (adoc-ext--tempo-handle-if-condition cond)
;; 	     result `(not ,cond)))
;;       (`(flag ,name)
;;        (cl-ext-unless (symbolp name)
;; 	   (signal-type-error name (type-of name) 'symbolp))
;;        (setq result `(tempo-lookup-named ',name)))
;;       (`(named ,name)
;;        (cl-ext-unless (symbolp name)
;; 	   (signal-type-error name (type-of name) 'symbolp))
;;        (setq result `(and (setq value (tempo-lookup-named ',name))
;; 			  (not (string-empty-p value)))))
;;       (`(- . ,args)
;;        ;; (- [N] CHAR)
;;        (cl-ecase (length args)
;; 	 (1 (let ((char (car args)))
;; 	      (setq result `(eql (char-before) ,char))))
;; 	 (2 (setq result `(save-excursion
;; 			    (left-char ,(car args))
;; 			    (eql (char-after) (cadr args)))))))
;;       (_ (funcall ivfun)))
;;     (if top-level
;; 	`(let (value)
;; 	   ,result)
;;       result)))

(defun adoc-ext--after-tempo ()
  (pop tempo-marks))

(defun adoc-ext-goto-cell (column row)
  "Inside a table, go to the cell (COLUMN, ROW).

For this function to work, there needs to be comment that
specifies the number of columns and rows in the table.

   // properties: columns 17 rows 8"
  (interactive "nColumn: \nnRow: ")
  (let ((header-regexp
	 (rx bol ?\[
	     (opt (group "%header,"))
	     "cols="))
	has-header
	properties)
    (unless (looking-at header-regexp)
      (re-search-backward header-regexp))
    (setq has-header (match-string 1))
    (save-excursion
      (forward-line -1)
      (unless (looking-at "^// properties: \\(.+\\)")
	(user-error "No properties line"))
      (setq properties
	    (-map-indexed
	     (lambda (it-index it)
	       (if (zerop (% it-index 2))
		   (intern it)
		 (string-to-number it)))
	     (s-split " " (match-string-no-properties 1)))))
    (forward-line 2)
    (-let (((&plist 'rows rows 'columns columns) properties)
	   (line-blank-forward
	    (lambda ()
	      (and (looking-at-p "^\n")
		   (forward-line 1)))))
      (unless (and (<= 1 row rows)
		   (<= 1 column columns))
	(user-error "Invalid cell %S, bounds are %S"
		    (cons column row)
		    (cons columns rows)))
      (and has-header
	   (forward-line columns)
	   (funcall line-blank-forward))
      (cl-loop for i from 1 below row
	       do
	       (forward-line columns)
	       (funcall line-blank-forward))
      (cl-loop for i from 1 below column
	       do
	       (forward-line 1)))))


;; ### Keymaps

(keymaps-ext-set-keymap adoc-mode-map "C-c C-s" #'adoc-ext-templates)

(define-prefix-command 'user-ext-adoc-sections-map nil
		       (format "%s = Document title; section titles, %s = level 1, %s = level 2, %s = level 3, %s = level 4"
			       (propertize "0" 'face 'font-lock-constant-face)
			       (propertize "1" 'face 'font-lock-constant-face)
			       (propertize "2" 'face 'font-lock-constant-face)
			       (propertize "3" 'face 'font-lock-constant-face)
			       (propertize "4" 'face 'font-lock-constant-face)))
(keymaps-ext-set-keymap adoc-mode-map "C-c C-h" #'user-ext-adoc-sections-map)
(keymaps-ext-set-keymap user-ext-adoc-sections-map "0" #'tempo-template-adoc-title-1)
(keymaps-ext-set-keymap user-ext-adoc-sections-map "1" #'tempo-template-adoc-title-2)
(keymaps-ext-set-keymap user-ext-adoc-sections-map "2" #'tempo-template-adoc-title-3)
(keymaps-ext-set-keymap user-ext-adoc-sections-map "3" #'tempo-template-adoc-title-4)
(keymaps-ext-set-keymap user-ext-adoc-sections-map "4" #'tempo-template-adoc-title-5)

(keymaps-ext-set-keymap adoc-mode-map "C-c i i" #'adoc-ext-insert-inline-image)
(keymaps-ext-set-keymap adoc-mode-map "C-c i I" #'adoc-ext-insert-block-image)
(keymaps-ext-set-keymap adoc-mode-map "C-c i l" #'adoc-ext-insert-link)


;; ### Hook

;;;###autoload
(defun adoc--extra-hook ()
  (setq-local tempo-interactive t
	      tempo-user-elements (cons #'adoc-ext-tempo-handler
					tempo-user-elements)))

;;;###autoload
(add-hook 'adoc-mode-hook #'adoc--extra-hook)

(extension-provide 'adoc-ext)
;;; adoc-ext.el ends here

;; Local Variables:
;; eval: (abbrev-ext-install-local-abbrev-functions)
;; eval: (abbrev-ext-define-local-abbrev "tax" "tempo-template-adoc-ext")
;; eval: (abbrev-ext-define-local-abbrev "tr" "transient")
;; eval: (abbrev-ext-define-local-abbrev "ax" "adoc-ext")
;; eval: (abbrev-ext-define-local-abbrev "ux" "user-ext-adoc")
;; End:

;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'markdown-mode)
(require 'markdown-toc)
(require 'org-table)
(require 'csv-mode)
(require 'tempo)

(eval-when-compile
  (defvar markdown-footnote-counter)
  (defvar markdown-mode-style-map)
  (defvar quick-mode-map))

(declare-function 'markdown-ext-scratch "markdown-ext.el" nil)
(declare-function 'markdown-footnote-counter-inc "markdown-mode.el" nil)


;; ### Variables

(defmacro markdown-ext-rx (&rest regexps)
  "Markdown specialized rx macro.
This variant of `rx' supports common Markdown named REGEXPS.

This adds the following named REGEXPS:
- indent -- Indentation; either 4 spaces or tab
- block-end -- End of a block
- numeral -- Numeric list item (e.g., \"1.\", \"#.\")
- bullet -- List item bullet (*, +, :, -)
- list-marker -- List marker, i.e., numeral or bullet
- heading -- ATX heading; two groups capture the '#'s and the text
- checkbox -- Checkbox"
  `(rx-let ((newline "\n")
            ;; Note: #405 not consider markdown-list-indent-width however this is never used
            (indent (or (= 4 " ") ?\t))
            (block-end (and (or (one-or-more (zero-or-more blank) "\n") line-end)))
            (numeral (and (one-or-more (any "0-9#")) "."))
            (bullet (any "*+:-"))
            (list-marker (or (and (one-or-more (any "0-9#")) ".")
                             (any "*+:-")))
	    (heading (seq bol (* blank) (group (** 1 6 ?#))
			  (* blank) (group (+ nonl))))
            (checkbox (seq "[" (any " xX") "]")))
     (rx ,@regexps)))


;; ### Skeletons

(defmacro markdown-skeleton-define (name doc &rest skel)
  "Define a `markdown-mode' skeleton using NAME, DOC, and SKEL.
The skeleton will be bound to markdown-skeleton-NAME."
  (declare (indent 1) (doc-string 2)
	   (debug (&define name [&or stringp "nil"] skeleton-edebug-spec)))
  (cl-ext-when (eq name 'define)
      (signal-invalid-argument name "Symbol `define' is not allowed"))
  (let* ((name (symbol-name name))
	 (funcname (intern (concat "markdown-skeleton-" name))))
    `(progn
       (define-skeleton ,funcname
	 ,(or doc (format "Insert %s statement." name))
	 ,@skel))))

;;;###autoload (autoload 'markdown-skeleton-id "markdown-ext" "Insert a link with an id field." t)
(markdown-skeleton-define id "Insert a link with an id field."
  "ID: "
  "<a id=\"" str "\"></a>" \n
  _ \n)

;;;###autoload (autoload 'markdown-skeleton-org-table "markdown-ext" "docstring" t)
(markdown-skeleton-define send-receive
  "Insert a Org table that gets converted to a Markdown table."
  "Table name: "
  '(orgtbl-mode 1)
  comment-start \n
  "#ORGTBL: SEND " str " "
  (completing-read "Function (default: orgtbl-to-markdown): "
		   obarray nil t nil nil
		   "orgtbl-to-markdown")
  \n _ \n
  comment-end \n
  "<!-- BEGIN RECEIVE ORGTBL " str " -->" \n
  "<!-- END RECEIVE ORGTBL " str " -->" \n)


;; ### Templates

(defmacro markdown-ext-tempo-define-template (name doc elements)
  "Define a template NAME which inserts ELEMENTS.
The template will be called tempo-template-markdown-ext-NAME.
DOCSTRING is the documentation string for the command.
ELEMENTS is a list of elements recognized by
`tempo-define-template', which see.

The Markdown extension adds `tempo-ext-tempo-handler' to
`tempo-user-elements', so additional elements are available.
\(See the documentation for `tempo-ext-tempo-handler'.)

\(fn NAME DOCSTRING ELEMENTS)"
  (declare (indent defun) (doc-string 2)
	   (debug (&define name stringp sexp)))
  (cl-check-type name string)
  (cl-check-type doc string)
  (cl-check-type elements list)
  (let* ((name (format "markdown-ext-%s" name))
	 (fname (format "tempo-template-%s" name)))
    `(prog1 ',(intern fname)
       (tempo-define-template ,name ',elements nil ,doc))))


;; ### Functions

(defun markdown-ext--cell-to-list (cell)
  (let ((buf (tmpbuf "cell"))
	elts)
    (with-current-buffer buf
      (insert cell)
      (move-beginning-of-line 1)
      (setq elts (mapcar #'csv--unquote-value (csv--collect-fields (line-end-position)))))
    (kill-buffer buf)
    (string-join elts "\n")))

(defun markdown-ext--process-table (table)
  (cl-loop
   for row in table			; is a list
   collect
   (if (listp row)
       (cl-loop
	for cell in row			; is a string
	collect (cond
		 ((string-match "^@list[[:space:]]+\\(.\\)" cell)
		  (markdown-ext--cell-to-list (substring cell
							 (match-beginning 1))))
		 (t cell)))
     row)))

(defun orgtbl-to-markdown (table params)
  "Convert TABLE to a Markdown table."
  (let* ((alignment (mapconcat (lambda (x) (if x "|--:" "|---"))
			       org-table-last-alignment ""))
	 (table (markdown-ext--process-table table))
	 (params2 (list :splice t
			:hline (concat alignment "|")
			:sep " | "
			:lstart "| " :lend " |")))
    (orgtbl-to-generic table (org-combine-plists params2 params))))

;;;###autoload
(defun markdown-ext-insert-footnote ()
  "Insert a footnote with a new number and move point to footnote definition."
  (interactive)
  (let ((fn (markdown-footnote-counter-inc)) pm)
    (insert (format "<sup>[%d](#fnt-%d)</sup>" fn fn))
    (markdown-footnote-text-find-new-location)
    (markdown-ensure-blank-line-before)
    (if (= fn 1)
	(insert "--------------------\n\n"))
    (unless (markdown-cur-line-blank-p)
      (insert "\n"))
    (insert (format "<small id=\"fnt-%d\">%d " fn fn))
    (setq pm (point-marker))
    (insert "</small>")
    (markdown-ensure-blank-line-after)
    (goto-char (marker-position pm))))

;;;###autoload
(defun markdown-ext-set-footnote-counter (num)
  "Set the footnote counter to NUM."
  (interactive "nNew counter: ")
  (setq markdown-footnote-counter num))

;;;###autoload
(defun markdown-ext-insert-image (file alt &optional title prefix)
  ""
  (interactive "fFile: \nsAlt text: \nsTitle (optional): \nP")
  ;; ![alt text](link "optional title")
  (cl-assert (or (stringp title) (null title)) "Title must be a string or nil.")
  (let* ((file (if prefix
		   (file-relative-name file)
		 (expand-file-name file)))
	 (title (if (and (stringp title) (> (length title) 0))
		    (format " \"%s\"" title)
		  ""))
	 (text (format "![%s](%s%s)" alt file title)))
    (insert text)))


;; --- Syntax

(cl-defun markdown-ext-parse-subtree (&optional pos)
  "Parse header subtree starting from the header at or before POS."
  (let (level heading-text)
    (cl-ext-save-point
      (cl-ext-cond
	  ((looking-at (markdown-ext-rx heading))
	   (setq level (length (match-string-no-properties 1))
		 heading-text (match-string-no-properties 2)))
	((re-search-backward (markdown-ext-rx heading) nil t)
	 (setq level (length (match-string-no-properties 1))
	       heading-text (match-string-no-properties 2)))))))


;; --- Hs minor mode


;; ### Menu

(easy-menu-define user-ext-markdown-ext-menu-map markdown-mode-map
  "Markdown extension map."
  '("Markdown Extension"
    ["Insert Footnote" markdown-ext-insert-footnote]
    ["Insert Image" markdown-ext-insert-image]
    ["Insert Org Table Send-Receive" markdown-skeleton-send-receive]))


;; ### Keymaps

(define-key markdown-mode-style-map (kbd "I") #'markdown-skeleton-id)
(define-key markdown-mode-style-map (kbd "f") #'markdown-ext-insert-footnote)
(define-key markdown-mode-map (kbd "C-c m t") #'markdown-toc-mode)

(bind-keys :map markdown-toc-mode-map
	   ("C-c C-t t" . markdown-toc-generate-or-refresh-toc)
	   ("C-c C-t M-t" . markdown-toc-generate-toc)
	   ("C-c C-t d" . markdown-toc-delete-toc)
	   ("C-c C-t x" . markdown-toc-follow-link-at-point)
	   ("C-c C-t r" . markdown-toc-refresh-toc))


;; ### Scratch Minor mode

;;;###autoload (autoload 'markdown-ext-scratch "markdown-ext" "Open a scratch buffer to edit markdown." t)
(define-scratch-buffer-function markdown-ext-scratch
				"markdown scratch" nil
  "Open a scratch buffer to edit markdown."
  nil
  (markdown-mode))


;; ### Hook

;;;###autoload
(defun markdown--extra-hook ()
  "Hook for `markdown-mode' extension."
  (setq-local tempo-interactive t
	      tempo-user-elements (cons #'tempo-ext-tempo-handler
					tempo-user-elements))
  (if (boundp 'electric-pair-text-pairs)
      (setq-local electric-pair-pairs
                  (append '((?\` . ?\`) (?‘ . ?’))
                          electric-pair-pairs))
    (message "TODO: Add electric pair hook for `markdown-mode'.")))

;;;###autoload
(add-hook 'markdown-mode-hook #'markdown--extra-hook)

(extension-provide 'markdown-ext)
;;; markdown-ext.el ends here

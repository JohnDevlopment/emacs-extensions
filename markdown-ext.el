;;; markdown-ext --- Markdown extension  -*- lexical-binding: t;  -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'markdown-mode)
(require 'markdown-toc)
(require 'org-table)
(require 'csv-mode)

(eval-when-compile
  (defvar markdown-footnote-counter)
  (defvar markdown-mode-style-map)
  (defvar quick-mode-map))

(declare-function 'markdown-ext-scratch "markdown-ext.el" nil)
(declare-function 'markdown-footnote-counter-inc "markdown-mode.el" nil)

;; Skeletons

(defmacro markdown-skeleton-define (name doc &rest skel)
  "Define a `markdown-mode' skeleton using NAME, DOC, and SKEL.
The skeleton will be bound to markdown-skeleton-NAME."
  (declare (indent 2))
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

;; Functions

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

(easy-menu-define user-ext-markdown-ext-menu-map markdown-mode-map
  "Markdown extension map."
  '("Markdown Extension"
    ["Insert Footnote" markdown-ext-insert-footnote]
    ["Insert Image" markdown-ext-insert-image]))

;;;###autoload (autoload 'markdown-ext-scratch "markdown-ext" "Open a scratch buffer to edit markdown." t)
(define-scratch-buffer-function markdown-ext-scratch
				"markdown scratch" nil
  "Open a scratch buffer to edit markdown."
  nil
  (markdown-mode))

(define-key markdown-mode-style-map (kbd "i") #'markdown-skeleton-id)
(define-key markdown-mode-style-map (kbd "f") #'markdown-ext-insert-footnote)
(define-key quick-mode-map (kbd "t") #'markdown-toc-mode)

(bind-keys :map markdown-toc-mode-map
	   ("C-c C-t t" . markdown-toc-generate-or-refresh-toc)
	   ("C-c C-t M-t" . markdown-toc-generate-toc)
	   ("C-c C-t d" . markdown-toc-delete-toc)
	   ("C-c C-t x" . markdown-toc-follow-link-at-point)
	   ("C-c C-t r" . markdown-toc-refresh-toc))

;; Hook

;;;###autoload
(defun markdown--extra-hook ()
  "Hook for `markdown-mode' extension.")

;;;###autoload
(add-hook 'markdown-mode-hook #'markdown--extra-hook)

(provide 'markdown-ext)

;;; markdown-ext.el ends here

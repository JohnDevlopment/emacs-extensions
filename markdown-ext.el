;;; markdown-ext --- Markdown extension  -*- lexical-binding: t;  -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'markdown-mode))

(defvar markdown-footnote-counter)
(defvar markdown-mode-style-map)
(defvar quick-mode-map)

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
  (assert (or (stringp title) (null title)) "Title must be a string or nil.")
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

;; Hook

;;;###autoload
(defun markdown--extra-hook ()
  "Hook for `markdown-mode' extension.")

;;;###autoload
(add-hook 'markdown-mode-hook #'markdown--extra-hook)

(provide 'markdown-ext)

;;; markdown-ext.el ends here

(defun markdown-insert-footnote2 ()
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

(defun markdown-set-footnote-counter (num)
  (interactive "nNew counter: ")
  (setq markdown-footnote-counter num))

(defmacro markdown-skeleton-define (name doc &rest skel)
  "Define a `markdown-mode' skeleton using NAME DOC and SKEL. The skeleton will be
bound to markdown-skeleton-NAME."
  (declare (indent 2))
  (let* ((name (symbol-name name))
	 (funcname (intern (concat "markdown-skeleton-" name))))
    `(progn
       (define-skeleton ,funcname
	 ,(or doc (format "Insert %s statement." name))
	 ,@skel))))

(markdown-skeleton-define id "Insert a link with an id field."
  "ID: "
  "<a id=\"" str "\"></a>" \n
  _ \n)

(defun markdown--extra-hook ()
  (define-key markdown-mode-style-map (kbd "i") #'markdown-skeleton-id)
  (define-key markdown-mode-style-map (kbd "f") #'markdown-insert-footnote2)
  (define-key quick-mode-map (kbd "t") #'markdown-toc-mode))

(add-hook 'markdown-mode-hook #'markdown--extra-hook)

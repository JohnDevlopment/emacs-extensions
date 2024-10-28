(eval-when-compile
  (require 'mhtml-mode)
  (require 'sgml-mode))

(defun modify-html-tag-alist ()
  (interactive)
  (setq-local
   html-tag-alist
   (let ((tag-alist html-tag-alist)
	 (div-atts '(("id") ("class"))))
     (assq-delete-all "section" tag-alist)
     (cl-pushnew `("section" \n ,@div-atts) tag-alist)
     tag-alist)))

(eval-and-compile
  (define-key mhtml-mode-map (kbd "C-M-i") #'completion-at-point))

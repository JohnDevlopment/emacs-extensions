;;; html-ext --- HTML mode extension.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'mhtml-mode)
  (require 'sgml-mode))

;;;###autoload
(defun modify-html-tag-alist ()
  "Modify `html-tag-alist' with our own tags."
  (interactive)
  (setq-local
   html-tag-alist
   (let ((tag-alist html-tag-alist)
	 (div-atts '(("id") ("class"))))
     (assq-delete-all "section" tag-alist)
     (cl-pushnew `("section" \n ,@div-atts) tag-alist)
     tag-alist)))

(define-key mhtml-mode-map (kbd "C-M-i") #'completion-at-point)

(provide 'html-ext)

;;; html-ext ends here

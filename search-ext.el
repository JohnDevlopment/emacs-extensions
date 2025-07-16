;; -*- lexical-binding: t; -*-

;; ### Functions

(defun python-regexp-to-emacs (regexp)
  "Convert Python-style REGEXP to Emacs syntax."
  (declare (pure t) (side-effect-free t))
  (let ((converted regexp)
	(handle-char (lambda (char prev-char)
		       (if (not prev-char)
			   (pcase char
			     ((or ?\( ?\) ?\{ ?\})
			      (format "\\%c" char))
			     (_ (char-to-string char)))
			 (pcase char
			   (?\\ "")
			   ((and (or ?\[ ?\])
				 (guard (eql prev-char ?\\)))
			    (format "\\%c" char))
			   ((or ?\( ?\) ?\{ ?\} ?|)
			    (if (eql prev-char ?\\)
				(char-to-string char)
			      (format "\\%c" char)))
			   (_ (char-to-string char)))))))
    (cl-loop with pc
	     for c across converted
	     concat (prog1 (funcall handle-char c pc)
		      (setq pc c)))))
(--ignore
 (cl-prettyprint (symbol-function 'python-regexp-to-emacs))
 (python-regexp-to-emacs "\\b")
 t)

(defun search-ext--around-search-function (oldfun regexp &rest args)
  "Convert Python regular expression to Emacs syntax.
For right now, this is only meant for `query-replace-regexp'."
  (setq regexp (python-regexp-to-emacs regexp))
  (--print-expr var regexp)
  (apply oldfun regexp args))

(defun search-ext-advise-replace-functions ()
  ;; TODO: Add docstring
  (interactive)
  (advice-add #'query-replace-regexp :around #'search-ext--around-search-function))

(defun search-ext-unadvise-replace-functions ()
  ;; TODO: Add docstring
  (interactive)
  (advice-remove #'query-replace-regexp #'search-ext--around-search-function))

(provide 'search-ext)
;;; search-ext.el ends here

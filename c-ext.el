;;; c-ext --- C mode extension.  -*- lexical-binding: t; -*-

(require 'skeleton)
(require 'cc-mode)

(defmacro c-skeleton-define (name doc &rest skel)
  "Define a `c-mode' skeleton using NAME DOC and SKEL.
The skeleton will be bound to c-skeleton-NAME."
  (declare (indent 1) (doc-string 2)
	   (debug (&define name stringp def-body)))
  (let* ((name (symbol-name name))
	 (funcname (intern (concat "c-skeleton-" name))))
    `(progn
       (define-skeleton ,funcname
	 ,(or doc (format "Insert %s statement." name))
	 ,@skel))))

(c-skeleton-define function "Insert a function definition."
		   "Name: "
		   "void " str "(" ("Parameter, %s: "
				    (unless (equal ?\( (char-before)) ", ")
				    str) ") {" \n
				    > _ \n < "}" \n \n)

(c-skeleton-define guard "Insert a header guard."
		   "Name: "
		   "#ifndef " str \n
		   "#define " str \n \n _ \n \n
		   "#endif /* " str " */")

(c-skeleton-define includes "Insert one or more includes."
		   "Header: "
		   "#include <" str ">" \n
		   ("Header: "
		    "#include <" str ">" \n)
		   resume:)

(c-skeleton-define struct nil
		   "Name: "
		   "struct " str " {"
		   \n > ("Field: " str ";" \n)
		   _ < "};")

;;;###autoload
(defun c-ext--extra-hook ()
  (setq-local skeleton-further-elements
	      '((< '(- (min c-basic-offset (current-column))))))
  (c-set-style "user"))

(define-key c-mode-base-map (kbd "C-c C-t f") #'c-skeleton-function)
(define-key c-mode-base-map (kbd "C-c C-t g") #'c-skeleton-guard)
(define-key c-mode-base-map (kbd "C-c C-t i") #'c-skeleton-includes)
(define-key c-mode-base-map (kbd "C-c C-t s") #'c-skeleton-struct)

;;;###autoload
(add-hook 'c-mode-common-hook 'c-ext--extra-hook)

(provide 'c-ext)

;;; c-ext ends here

(eval-when-compile
  (require 'skeleton)
  (require 'cc-mode))

(defmacro c-skeleton-define (name doc &rest skel)
  "Define a `c-mode' skeleton using NAME DOC and SKEL. The skeleton will be
bound to c-skeleton-NAME."
  (declare (indent 2))
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

(defun c--custom-hook ()
  (setq-local skeleton-further-elements
	      '((< '(- (min c-basic-offset (current-column))))))
  (define-key c-mode-base-map (kbd "C-c C-t f") #'c-skeleton-function)
  (define-key c-mode-base-map (kbd "C-c C-t g") #'c-skeleton-guard)
  (define-key c-mode-base-map (kbd "C-c C-t i") #'c-skeleton-includes)
  (define-key c-mode-base-map (kbd "C-c C-t s") #'c-skeleton-struct)
  (c-set-style "user"))

(add-hook 'c-mode-common-hook 'c--custom-hook)

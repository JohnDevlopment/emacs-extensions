;; load rust
(eval-when-compile
  (require 'rust-mode)
  (require 'skeleton))

(defmacro rust-skeleton-define (name doc &rest skel)
  "Define a `rust-mode' skeleton using NAME DOC and SKEL. The skeleton will be
bound to rust-skeleton-NAME."
  (declare (indent 2))
  (let* ((name (symbol-name name))
	 (funcname (intern (concat "rust-skeleton-" name))))
    `(progn
       (define-skeleton ,funcname
	 ,(or doc (format "Insert %s statement." name))
	 ,@skel))))

(rust-skeleton-define function "Insert a function definition."
  "Name: "
  "fn " str "(" ("Parameter, %s: "
		 (unless (equal ?\( (char-before)) ", ")
		 str) ") {" \n
		 > "// code..." _ \n < "}" \n)

(defun rust--custom-hook ()
  (setq-local skeleton-further-elements
	      '((< '(- (min rust-indent-offset (current-column))))))
  (define-key rust-mode-map (kbd "C-c C-t f") #'rust-skeleton-function))

(add-hook 'rust-mode-hook #'rust--custom-hook)

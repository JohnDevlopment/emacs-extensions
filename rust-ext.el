;;; rust-ext --- Rust mode extension.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'rust-mode)
  (require 'skeleton))

;; Variables

(defgroup rust-ext nil
  "A group for Rust extension."
  :group 'user-extensions)

(defcustom user-ext-rust-docstring-fill-column 60
  "Fill column for docstring."
  :type 'integer
  :group 'rust-ext)

(defvar user-ext-rust--register ?r)

(defvar user-ext-rust--docstring-buffer nil
  "Docstring buffer in `rust-mode'.")

;; Functions

(defun rust-ext--write-docstring ()
  "Exit the Rust docstring buffer and apply the docstring."
  (interactive)
  (let (num-lines prefix buffer-string)
    (deactivate-mark)
    (untabify (point-min) (point-max))
    (setq num-lines (count-lines (point-min) (point-max))) ; count number of lines
    (with-current-buffer user-ext-rust--docstring-buffer   ; extract contents of buffer
      (setq buffer-string (buffer-string)))		   ; into a variable
    (when (string-blank-p buffer-string)		   ; exit function if string is empty
      (kill-buffer)					   ;
      (jump-to-register user-ext-rust--register)	   ;
      (error "Docstring is empty"))			   ;
    (kill-buffer)
    (jump-to-register user-ext-rust--register)
    (insert buffer-string)
    (save-excursion
      ;; get leading spaces
      (beginning-of-line)
      (setq prefix
	    (concat "/// "
		    (if (looking-at "^\\([ \t]+\\)")
			(match-string 1)
		      ""))))
    ;; go to the beginning of each line and indent it
    ;; starting on the second line
    (move-beginning-of-line 2)
    (dotimes (_i (1- num-lines))
      (insert prefix)
      (move-beginning-of-line 2))))

;;;###autoload
(defun rust-ext-docstring ()
  "Open a temporary buffer to write a docstring."
  (interactive)
  (let (modes)
    (window-configuration-to-register user-ext-rust--register)
    (dolist (mode '(abbrev-mode))
      (when (and (boundp mode) mode)
	(cl-pushnew mode modes)))
    (setq user-ext-rust--docstring-buffer (tmpbuf "rust-docstring"))
    (assert (not (null user-ext-rust--docstring-buffer)))
    (split-window-sensibly)				 ;
    (switch-to-buffer user-ext-rust--docstring-buffer)   ; Split window and enter temporary buffer
    (markdown-mode)
    (dolist (mode modes)
      (funcall mode 1))
    ;; set fill column and enable auto-fill
    (set-fill-column user-ext-rust-docstring-fill-column)
    (auto-fill-mode 1)
    (local-set-key (kbd "C-c C-c") #'rust-ext--write-docstring)
    (setq header-line-format "Rust Docstring: Type C-c C-c to apply changes")
    (message "Type C-c C-c to save changes.")))

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
(define-key rust-mode-map (kbd "C-c i D") #'rust-ext-docstring)

;;;###autoload
(defun rust--custom-hook ()
  (setq-local skeleton-further-elements
	      '((< '(- (min rust-indent-offset (current-column))))))
  (define-key rust-mode-map (kbd "C-c C-t f") #'rust-skeleton-function))

;;;###autoload
(add-hook 'rust-mode-hook #'rust--custom-hook)

(provide 'rust-ext)

;;; rust-ext ends here

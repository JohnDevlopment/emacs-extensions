;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((dired-mode .
	     ((eval .
		    (progn
		      (defun dired-diff-with-source-file (file &optional switches)
			(interactive
			 (let* ((current (dired-get-filename t))
				(target-dir "~/.emacs.d/extensions")
				(default (s-lex-format "${target-dir}/${current}"))
				(initial (s-lex-format "/${current}"))
				(defaults (dired-dwim-target-defaults (list current) target-dir)))
			   (list (minibuffer-with-setup-hook
				     (lambda nil
				       (setq-local minibuffer-default-add-function nil)
				       (setq minibuffer-default defaults))
				   (read-file-name (format "Diff %s with%s: " current
							   (if default
							       (s-lex-format " (default ${default})")
							     ""))
						   target-dir default t initial))
				 (when current-prefix-arg
				   (read-string "Options for diff: "
						(if (stringp diff-switches)
						    diff-switches
						  (mapconcat #'identity diff-switches " ")))))))
			(let ((current (dired-get-filename t)))
			  (diff current file switches)))
		      (define-key dired-mode-map (kbd "M-=") #'dired-diff-with-source-file)

		      (defun mc-todo-hyphen-check (&optional arg)
			"Keyboard macro."
			(interactive "p")
			(kmacro-exec-ring-item '([insert home right right right kp-subtract end insert] 0 "%d")
					       arg)))))))

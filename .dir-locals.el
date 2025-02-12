;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((emacs-lisp-mode . ((eval .
			   (progn
			     (unless (fboundp 'mc-doc-ext)
			       (defun mc-doc-ext (&optional arg)
				 "Keyboard macro."
				 (interactive "p")
				 (kmacro-exec-ring-item
				  '([40 100 111 99 117 109 101 110 116 45 101 120 116
					101 110 115 105 111 110 32 34 21 24 81 69 120
					116 101 110 115 105 111 110 58 32 return return
					right return 34] 0 "%d")
				  arg)))
			     (unless (fboundp 'mc-autoload)
			       (defun mc-autoload (&optional arg)
				 "Keyboard macro."
				 (interactive "p")
				 (kmacro-exec-ring-item '(";;;###autoload" 0 "%d") arg))))))))

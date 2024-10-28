;; -*- lexical-binding: t; -*-

(require 'kmacro)

(defun macro-ext-query (arg)
  (interactive "P")
  "Prompt for user input using minibuffer during macro execution.
With prefix ARG, allows you to choose what prompt string to
use.  If the input is non-empty, it is inserted at point."
  (let* ((prompt (if arg
		     (read-from-minibuffer "PROMPT: ")
		   "Input: "))
	 (input (minibuffer-with-setup-hook
		    (lambda ()
		      (kbd-macro-query t))
		  (read-from-minibuffer prompt))))
    (unless (string= input "")
      (insert input))))

(defun forward-or-backward-sexp (&optional arg)
  "Go to the matching parenthesis character if one is adjacent to point.
With ARG, do it that many times. A negative arg -N reverses the direction of the
motion."
  (interactive "^p")
  (cond ((looking-at "\\s(")
	 (forward-sexp arg))
	((looking-at "\\s)")
	 (forward-char)
	 (backward-sexp arg))
        ((looking-back "\\s)" 1)
	 (backward-sexp arg))
        ;; Now, try to succeed from inside of a bracket
        ((looking-back "\\s(" 1)
	 (backward-char)
	 (forward-sexp arg)
	 (backward-char))))

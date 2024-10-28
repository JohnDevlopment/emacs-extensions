;;; elisp-ext --- Emacs Lisp mode extension.  -*- lexical-binding: t;  -*-

;;; Commentary:

;;; Code:

(defconst user-ext-elisp-dir-locals-template
  "((major-mode . ((var1 . value)
		(var2 . value)))
 (\"subdirectory\" .
  ((major-mode . ((var1 . value)
		  (var2 . value)))
   (nil . ((var1 . value)
	   (eval . ((message \"A message.\"))))))))"
  "Boilerplate for a .dir-locals.el file.")

(defun elisp-ext-create-dir-locals-file (file)
  "Create a .dir-locals.el file at FILE."
  (interactive
   (let ((default ".dir-locals.el"))
     (list (read-file-name "Filename: " nil nil
			   (confirm-nonexistent-file-or-buffer)
			   default))))
  (with-temp-file file
    (let ((_buf (current-buffer)))
      (insert user-ext-elisp-dir-locals-template))))

(defun elisp-ext-forward-or-backward-sexp (&optional arg)
  "Go to the matching parenthesis to the one is adjacent at point.
With ARG, do it that many times.  A negative arg -N reverses
the direction of the motion."
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

(define-scratch-buffer-function elisp-ext-scratch-buffer "elisp" nil
  "Create a scratch buffer for Emacs lisp code."
  nil
  (emacs-lisp-mode)
  ;; Enable electric pair mode
  (unless (and (boundp 'electric-pair-mode) electric-pair-mode)
    (electric-pair-local-mode t))
  ;; Enable company mode
  (unless (and (boundp 'company-mode) company-mode)
    (company-mode t))
  (setq header-line-format "Type C-c C-c when finished, C-x k to cancel editing.")
  (local-set-key (kbd "C-c C-c")
		 (lambda ()
		   (interactive)
		   (kill-region (point-min) (point-max))
		   (kill-and-quit))))

(defun elisp-ext--find-quote (&optional reverse)
  "Find the next quote.
If REVERSE is non-nil, search backwards."
  (save-excursion
    (if reverse
	(progn
	  (goto-char (point-max))
	  (search-backward "\"")
	  (point))
      (goto-char (point-min))
      (search-forward "\"")
      (point))))

(define-scratch-buffer-function elisp-ext-doc-scratch-buffer "elisp docstring" nil
  "Create a scratch buffer for Emacs lisp docstrings."
  nil
  (emacs-lisp-mode)
  (auto-fill-mode t)
  (set-fill-column 67)
  (local-set-key (kbd "S-<return>")
		 (lambda ()
		   (interactive)
		   (set-fill-column 60)
		   (newline)))
  ;; Enable electric pair mode
  (unless (and (boundp 'electric-pair-mode) electric-pair-mode)
    (electric-pair-local-mode t))
  ;; Enable company mode
  (unless (and (boundp 'company-mode) company-mode)
    (company-mode t))
  (insert ";; Fill column is set to 67. Type S-<return> to set it to 60.\n")
  (insert "\"\n\n\"")			   ; insert quotes
  (goto-char (1+ (elisp-ext--find-quote))) ; set point in between the quotes
  (setq header-line-format "Type C-c C-c when finished, C-x k to cancel editing.")
  (local-set-key (kbd "C-c C-c")
		 (lambda ()
		   (interactive)
		   (let* ((beg (1+ (elisp-ext--find-quote)))
			  (end (1- (elisp-ext--find-quote t))))
		     (kill-region beg end)
		     (kill-and-quit)))))


(provide 'elisp-ext)

;;; elisp-ext ends here

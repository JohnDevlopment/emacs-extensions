;;; elisp-ext --- Emacs Lisp mode extension.  -*- lexical-binding: t;  -*-

;;; Commentary:

;;; Code:


;;;###autoload
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

(defun elisp-ext--scratch-buffer-ctrl-c-ctrl-c ()
  (interactive)
  (kill-region (point-min) (point-max))
  (kill-and-quit))

;;;###autoload (autoload 'elisp-ext-scratch-buffer "elisp-ext" "Create a scratch buffer for Emacs lisp code." t)
(define-scratch-buffer-function elisp-ext-scratch-buffer "elisp" nil
  "Create a scratch buffer for Emacs lisp code."
  nil
  (emacs-lisp-mode)
  (unless (and (boundp 'electric-pair-mode) electric-pair-mode) ; Enable electric pair mode
    (electric-pair-local-mode t))				;
  (unless (and (boundp 'company-mode) company-mode) ; Enable company mode
    (company-mode t))				    ;
  (setq header-line-format "Type C-c C-c when finished, C-x k to cancel editing.")
  (local-set-key (kbd "C-c C-c") #'elisp-ext--scratch-buffer-ctrl-c-ctrl-c))

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

(defun elisp-ext-doc-scratch-buffer--shift-return ()
  (interactive)
  (newline)
  (set-fill-column 60))

(defun elisp-ext-doc-scratch-buffer--ctrl-c-ctrl-c ()
  (interactive)
  (let* ((beg (1+ (elisp-ext--find-quote)))
	 (end (1- (elisp-ext--find-quote t))))
    (kill-region beg end)
    (kill-and-quit)))

;;;###autoload (autoload 'elisp-ext-doc-scratch-buffer "elisp-ext" "Create a scratch buffer for Emacs lisp docstrings.")
(define-scratch-buffer-function elisp-ext-doc-scratch-buffer "elisp docstring" nil
  "Create a scratch buffer for Emacs lisp docstrings."
  nil
  (emacs-lisp-mode)
  (auto-fill-mode t)
  (set-fill-column 67)
  (local-set-key (kbd "S-<return>") #'elisp-ext-doc-scratch-buffer--shift-return)
  (unless (and (boundp 'electric-pair-mode) electric-pair-mode)
    (electric-pair-local-mode t))
  (unless (and (boundp 'company-mode) company-mode)
    (company-mode t))
  (insert ";; Fill column is set to 67. Type S-<return> to set it to 60.\n")
  (insert "\"\n\n\"")
  (goto-char (1+ (elisp-ext--find-quote)))
  (setq header-line-format "Type C-c C-c when finished, C-x k to cancel editing.")
  (local-set-key (kbd "C-c C-c") #'elisp-ext-doc-scratch-buffer--ctrl-c-ctrl-c))

(provide 'elisp-ext)

;;; elisp-ext ends here

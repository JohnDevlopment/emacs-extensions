;;  -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-ext)
  (require 'yasnippet))

(when (and (boundp 'yas-minor-mode) yas-minor-mode)
  (add-hook 'completion-at-point-functions #'yasnippet-capf))

(defun yas-ext-org-very-safe-expand () (yas-expand))

(defun yas-ext-enable-company-completion (&optional local)
  "Enable completion for Yasnippets, including for `company-mode'."
  (interactive "P")
  (if local
      (cl-ext-progn
	(add-hook 'completion-at-point-functions #'yasnippet-capf nil t)
	(setq-local company-backends (cons #'company-yasnippet company-backends)))
    (add-hook 'completion-at-point-functions #'yasnippet-capf)
    (add-to-list 'company-backends #'company-yasnippet)))

(defun yas-ext-compile-snippet-dir (&optional is-interactive)
  "Call this function to compile a snippet directory.

The user is prompted to choose from a list of directories
taken from `yas-snippet-dirs'. The input is then forwarded
to `yas-compile-directory'.

The prefix arg IS-INTERACTIVE is never actually used in the
function; rather, it's checked to see whether this function
was called interactively."
  (interactive "p")
  (cl-ext-unless is-interactive
    (user-error "This method can only be called interactively"))
  (let (dir)
    (setq dir (completing-read "Top-level directory: "
			       (yas-snippet-dirs)
			       nil "confirm"))
    (yas-compile-directory dir)))
(put 'yas-ext-compile-snippet-dir 'interactive-only t)

(define-key yas-minor-mode-map (kbd "C-c & c") #'yas-ext-compile-snippet-dir)

(provide 'yasnippets-ext)

;;; yasnippets-ext ends here

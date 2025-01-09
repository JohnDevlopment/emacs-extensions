;;; yasnippets-ext --- Yasnippets mode extension.

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'yasnippet))

(defun yas-ext-org-very-safe-expand ()
  (let ((yas-fallback-behavior 'return-nil))
    (yas-expand)))

(defun yas-ext-enable-company-completion ()
  (interactive)
  (add-to-list 'completion-at-point-functions #'company-yasnippet nil #'eq))

(defun yas-ext-compile-snippet-dir (&optional is-interactive)
  "Call this function to compile a snippet directory.

The user is prompted to choose from a list of directories
taken from `yas-snippet-dirs'. The input is then forwarded
to `yas-compile-directory'.

The prefix arg IS-INTERACTIVE is never actually used in the
function; rather, it's checked to see whether this function
was called interactively."
  (interactive "p")
  (unless is-interactive
    (user-error "This method can only be called interactively"))
  (let (dir)
    (setq dir (completing-read "Top-level directory: "
			       (yas-snippet-dirs)
			       nil "confirm"))
    (yas-compile-directory dir)))
(put 'yas-ext-compile-snippet-dir 'interactive-only t)

(defun yas--extra-hook ()
  "Extra hook for `yasnippet-minor-mode'."
  (interactive)
  ;; (add-hook 'completion-at-point-functions #'company-yasnippet nil t)
  )

(when nil
  (remove-hook 'completion-at-point-functions #'company-yasnippet t))

(add-hook 'yas-minor-mode-hook #'yas--extra-hook)

(eval-and-compile
  (define-key yas-minor-mode-map (kbd "C-c & c") #'yas-ext-compile-snippet-dir))

(provide 'yasnippets-ext)

;;; yasnippets-ext ends here

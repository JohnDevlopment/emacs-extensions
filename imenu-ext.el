;; -*- lexical-binding: t; -*-

;;;###autoload
(defun bind-imenu ()
  "Binds `imenu' to the right-mouse button locally.
The bindings are local to the active keymap, which means
buffers sharing the same major mode will be affected."
  (interactive)
  (if (not (boundp 'python-mode))
      (let ()
	(local-set-key (kbd "<mouse-3>") #'imenu)
	(message "Locally bound `imenu' to the right mouse button"))
    (user-error "`bind-imenu' is not meant to be used in Python mode")))

;;;###autoload
(defun bind-imenu-lsp ()
  "Binds `imenu' to the double left-click mouse button locally.
The bindings are local to the active keymap, which means
buffers sharing the same major mode will be affected."
  (interactive)
  (when (not (boundp 'python-mode))
    (setq mark-active nil)
    (local-set-key (kbd "<double-mouse-1>") #'imenu)
    (message "Bind `imenu' to the left mouse button double-clicked")))

(provide 'imenu-ext)

;;; imenu-ext ends here

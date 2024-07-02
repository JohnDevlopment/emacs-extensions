;; -*- lexical-binding: t; -*-

(defun log4j--extra-hook ()
  (visual-line-mode t)
  (local-set-key "r" (lambda ()
		       (interactive)
		       (revert-buffer t t)
		       (message "Buffer reverted.")))
  (local-set-key "k" (lambda ()
		       (interactive)
		       (kill-buffer)))
  (local-set-key "q" #'bury-buffer)
  (read-only-mode t)
  (setq header-line-format "Press r to revert, q to quit, k to kill this buffer."))

(add-hook 'log4j-mode-hook #'log4j--extra-hook)

(add-to-list 'auto-mode-alist '("\\.log\\.[1-9]+\\'" . log4j-mode))

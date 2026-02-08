;; -*- lexical-binding: t; -*-

(check-emacs-minimum-version "27.4")
(extension-check-requires log4j-mode)

(defvar user-ext-log4j-filtering nil
  "Non-nil during the middle of filtering.")

(defvar-local user-ext-log4j-filtering-buffer nil
  "The name of the temporary buffer that was created via
`log4j-ext-stop-filter'.")

;;;###autoload
(defun log4j-ext-start-filter (include-string exclude-string)
  "Turn filtering on in the current log file buffer.
When used interactively, the user enters INCLUDE-STRING and
EXCLUDE-STRING, which should be strings of filter keywords,
separated by spaces.

This calls `log4j-start-filter' with the given args."
  (interactive "sInclude keywords: \nsExclude keywords: ")
  (setq user-ext-log4j-filtering t
	user-ext-log4j-filtering-buffer (format "*%s*" (buffer-name)))
  (message "Set `user-ext-log4j-filtering' to %s, buffer file" user-ext-log4j-filtering)
  (log4j-start-filter include-string exclude-string)
  (with-current-buffer user-ext-log4j-filtering-buffer
    (read-only-mode t))
  (setq user-ext-log4j-filtering-buffer nil user-ext-log4j-filtering nil)
  (define-key log4j-mode-map (kbd "C-c C-s") #'log4j-ext-stop-filter))

;;;###autoload
(defun log4j-ext-stop-filter ()
  "Turn filtering off in the current log file buffer."
  (interactive)
  (log4j-stop-filter)
  (define-key log4j-mode-map (kbd "C-c C-s") #'log4j-ext-start-filter))

;;;###autoload
(defun log4j--extra-hook ()
  "Extra hook for `log4j-mode'."
  (visual-line-mode t)
  (local-set-key "r" (lambda ()
		       (interactive)
		       (revert-buffer t t)
		       (message "Buffer reverted.")))
  (local-set-key "k" (lambda ()
		       (interactive)
		       (kill-buffer)))
  (local-set-key "q" #'bury-buffer)
  (message "user-ext-log4j-filtering = %s" user-ext-log4j-filtering)
  (unless user-ext-log4j-filtering	; is the buffer with filtered records
    (read-only-mode t))			;
  (setq header-line-format "Press r to revert, q to quit, k to kill this buffer.")
  (define-key log4j-mode-map (kbd "C-c C-s") #'log4j-ext-start-filter))

;;;###autoload
(add-hook 'log4j-mode-hook #'log4j--extra-hook)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.log\\.[1-9]+\\'" . log4j-mode))

(extension-provide 'log4j-ext)
;;; log4j-ext ends here

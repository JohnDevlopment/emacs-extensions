;; -*- lexical-binding: t; -*-

(use-package hi-lock
  :autoload
  hi-lock-read-face-name
  :functions
  hi-lock-read-face-name)

(defconst user-ext-highlight-category 'highlight-ext)

(defvar user-ext-highlight-initialized nil
  "Set to true when this extension is initialized.")

(defun highlight-ext--init ()
  (let ((symbol user-ext-highlight-category))
    (cl-ext-unless user-ext-highlight-initialized
      (setplist symbol '(evaporate t))
      (setq user-ext-highlight-initialized t))))

(highlight-ext--init)

;;;###autoload
(defun highlight-ext-region (beg end face)
  (interactive (progn
		 (cl-ext-unless (use-region-p)
		   (user-error "The region is inactive"))
		 (list (region-beginning) (region-end)
		       (intern-soft (hi-lock-read-face-name)))))
  (deactivate-mark)
  (cl-assert (and beg end face) t)
  (let ((ov (make-overlay beg end (current-buffer) t)))
    (overlay-put ov 'category user-ext-highlight-category)
    (overlay-put ov 'face face)))

;;;###autoload
(defun highlight-ext-remove-all-highlights ()
  "docstring"
  (interactive)
  (cl-loop for ov being the overlays
	   if (eq (overlay-get ov 'category) user-ext-highlight-category)
	   do
	   (delete-overlay ov)))

(provide 'highlight-ext)
;;; highlight-ext.el ends here

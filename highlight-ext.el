;; -*- lexical-binding: t; -*-

(check-emacs-minimum-version "27.4")

(use-package hi-lock
  :autoload
  hi-lock-read-face-name
  :functions
  hi-lock-read-face-name)

;; ### Customization

(defgroup highlight-ext nil
  "Extension for highlighting things."
  :group 'user-extensions)

(defface user-ext-highlight-strikethrough
  '((t (:inherit default :strike-through t)))
  "Strikethrough face."
  :group 'highlight-ext)


;; ### Variables

(defconst user-ext-highlight-category 'highlight-ext)

(defvar user-ext-highlight-initialized nil
  "Set to true when this extension is initialized.")


;; ### Functions

(defun highlight-ext--init ()
  (let ((symbol user-ext-highlight-category))
    (unless user-ext-highlight-initialized
      (setplist symbol '(evaporate t))
      (setq user-ext-highlight-initialized t))))

(highlight-ext--init)

;;;###autoload
(defun highlight-ext-region (start end face)
  "Highlight the region using FACE.
When called interactively, START and END are the region, and
the user is prompted for the name of a face."
  (interactive
   (progn
     (unless (use-region-p)
       (user-error "The region is inactive"))
     (prog1 (list (region-beginning)
		  (region-end)
		  (intern-soft
		   (hi-lock-read-face-name)))
       (deactivate-mark))))
  (cl-assert (and start end face) t)
  (let ((ov (make-overlay start end (current-buffer) t)))
    (overlay-put ov 'category user-ext-highlight-category)
    (overlay-put ov 'face face)))

;;;###autoload
(defun highlight-ext-remove-all-highlights ()
  "Remove all highlights in the current buffer."
  (interactive)
  (cl-loop for ov being the overlays
	   if (eq (overlay-get ov 'category) user-ext-highlight-category)
	   do
	   (delete-overlay ov)))


(extension-provide 'highlight-ext)
;;; highlight-ext.el ends here

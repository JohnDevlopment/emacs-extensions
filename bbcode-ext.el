;; -*- lexical-binding: t; -*-

(require 'bbcode-mode)

(eval-when-compile
  (defvar bbcode-mode-prefix)
  (declare-function bbcode-mode-prefix "bbcode-ext.el"))

(defun bbcode-ext--insert-tag (name beg end)
  "Insert the tag NAME at point or around region BEG and END."
  (let ((pos (point-marker))
	(dist (length (format "[/%s]" name))))
    (if (use-region-p)
	(progn
	  (deactivate-mark)
	  (goto-char end)
	  (insert (format "[/%s]" name))
	  (goto-char beg)
	  (insert (format "[%s]" name))
	  (goto-char pos))
      (insert (format "[%s][/%s]" name name))
      (backward-char dist))))

(when nil
  (let ((pos (point-marker)))
    (if (not (use-region-p))
	(let ((dist (length (format "[/%s]" name))))
	  (insert (format "[%s][/%s]" name name))
	  (backward-char dist))
      ;; Surround region in markers
      (goto-char end)
      (insert (format "[/%s]" name))

      (goto-char beg)
      (insert (format "[%s]" name))

      (goto-char pos))))

;; (defun bbcode-emphasis (char &optional beg end)
;;   "Insert BBCode tags at point or around region according to CHAR.

;; If called interactively, or if BEG and END are non-nil, the
;; tags are wrapped around the region indicated by BEG and END."
;;   (interactive "cWhat (b = bold, i = italicize): \nr")
;;   (message "%s %d-%d" char beg end)
;;   (bbcode-add-tag (char-to-string char) beg end))

(defmacro bbcode-ext-define-insert-tag-command (name)
  (let ((fname (intern (format "bbcode-ext-insert-tag-%s" name))))
    (cl-check-type fname symbol)
    `(progn
       (defun ,fname (&optional beg end)
	 (interactive (when (use-region-p) (region-bounds)))
	 (bbcode-ext--insert-tag ,name beg end)))))

(bbcode-ext-define-insert-tag-command "i")
(bbcode-ext-define-insert-tag-command "b")

(defun bbcode-ext-insert-tag--completion ()
  (let ((tag (read-string "Name: ")))
    (if (use-region-p)
	(pcase (region-bounds)
	  (`((,beg . ,end)) (list tag beg end))
	  (x (error "Invalid form: %S" x)))
      (list tag))))

;;;###autoload
(defun bbcode-ext-insert-tag (string &optional beg end)
  (interactive (bbcode-ext-insert-tag--completion))
  (bbcode-ext--insert-tag string beg end))

(define-prefix-command 'bbcode-mode-prefix)
(define-key bbcode-mode-map (kbd "C-c i") #'bbcode-mode-prefix)
(define-key bbcode-mode-prefix "e" #'bbcode-ext-insert-tag-i)
(define-key bbcode-mode-prefix "b" #'bbcode-ext-insert-tag-b)

;;;###autoload
(defun bbcode-extra-hook ()
  "Extra hook for `bbcode-mode'."
  (setq indent-tabs-mode nil)
  (setq tab-width 4))

;;;###autoload
(add-hook 'bbcode-mode-hook #'bbcode-extra-hook)

(provide 'bbcode-ext)

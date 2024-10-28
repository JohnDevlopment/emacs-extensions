;;; bbcode-ext --- BBCode mode extension  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'bbcode-mode))

(define-prefix-command 'bbcode-mode-prefix)

(defun bbcode-add-tag (name &optional beg end)
  "Insert the tag NAME at point or around region BEG and END."
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

;;;###autoload
(defun bbcode-emphasis (char &optional beg end)
  "Insert BBCode tags at point or around region according to CHAR.

If called interactively, or if BEG and END are non-nil, the
tags are wrapped around the region indicated by BEG and END."
  (interactive "cWhat (b = bold, i = italicize): \nr")
  (message "%s %d-%d" char beg end)
  (bbcode-add-tag (char-to-string char) beg end))

(defun bbcode-extra-hook ()
  "Extra hook for `bbcode-mode'."
  (local-set-key (kbd "C-c i") #'bbcode-mode-prefix)
  (define-key bbcode-mode-prefix "e" #'bbcode-emphasis)
  (setq indent-tabs-mode nil)
  (setq tab-width 4))

;;;###autoload
(defun bbcode-ext-enable ()
  "Call this function to load `bbcode-ext'.
Otherwise, this function does nothing."
  (interactive))

(add-hook 'bbcode-mode-hook #'bbcode-extra-hook)

(provide 'bbcode-ext)

;;; bbcode-ext ends here

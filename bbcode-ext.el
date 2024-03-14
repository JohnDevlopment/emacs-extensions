(eval-when-compile
  (require 'bbcode-mode))

(eval-and-compile
  (define-prefix-command 'bbcode-mode-prefix))

(defun bbcode-add-tag (name &optional beg end)
  "Inserts the tag NAME at the current point or surrounds the text in the region
BEG and END."
  (let ((pos (point-marker))
	mend)
    (if (not (use-region-p))
	(let ((dist (length (format "[/%s]" name))))
	  (insert (format "[%s][/%s]" name name))
	  (backward-char dist))
      ;; Surround region in markers
      (goto-char end)
      (setq mend (point-marker))
      (insert (format "[/%s]" name))

      (goto-char beg)
      (insert (format "[%s]" name))

      (goto-char pos))))

(defun bbcode-emphasis (char &optional beg end)
  (interactive "cWhat (b = bold, i = italicize): \nr")
  (message "%s %d-%d" char beg end)
  (bbcode-add-tag (char-to-string char) beg end))

(defun bbcode-extra-hook ()
  (local-set-key (kbd "C-c i") #'bbcode-mode-prefix)
  (define-key bbcode-mode-prefix "e" #'bbcode-emphasis)
  (setq indent-tabs-mode nil)
  (setq tab-width 4))

(add-hook 'bbcode-mode-hook #'bbcode-extra-hook)

;;; comment-tags-ext --- Comment-tags mode extension.  -*- lexical-binding: t;  -*-

;;; Commentary:

;;; Code:

(require 'comment-tags)

;;; Code:

(defcustom user-ext-global-comment-tags-exclude
  nil
  "List of buffers to exclude from `comment-tags-mode'."
  :group 'global-comment-tags
  :type '(repeat string))

(defvar global-comment-tags-regexp (comment-tags--make-regexp)
  "Regular expression for matching TODO-ish comments.")

;;;###autoload
(defun comment-tags-mode-turn-on ()
  "Turn on `comment-tags-mode' if the buffer has TODO-ish comments."
  (interactive)
  (let ((buf (current-buffer))
	flag)
    (unless (memq buf user-ext-global-comment-tags-exclude)
      (save-excursion
	(goto-char (point-min))
	(setq flag (re-search-forward global-comment-tags-regexp nil t)))
      (when flag
	(message "`comment-tags-mode' enabled in %s" buf)
	(comment-tags-mode 1)))))

;;;###autoload
(define-globalized-minor-mode global-comment-tags-mode
  comment-tags-mode comment-tags-mode-turn-on)

(provide 'comment-tags-ext)

;;; comment-tags-ext ends here

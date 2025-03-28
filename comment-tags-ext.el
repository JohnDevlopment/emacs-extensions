;; -*- lexical-binding: t;  -*-

(require 'comment-tags)

;; Advice

(fext-defadvice comment-tags-list-tags-buffer (after list-comment-tags-buffer)
  (with-current-buffer comment-tags-temp-buffer-name
    ;; (local-set-key (kbd "C-c c q") #'quit-window)
    (view-mode 1)))

;; Variables

(defcustom user-ext-global-comment-tags-exclude
  nil
  "List of buffers to exclude from `comment-tags-mode'."
  :group 'global-comment-tags
  :type '(repeat string))

(defvar global-comment-tags-regexp (comment-tags--make-regexp)
  "Regular expression for matching TODO-ish comments.")

;; Functions

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
	;; (message "`comment-tags-mode' enabled in %s" buf)
	(comment-tags-mode 1)))))

;;;###autoload
(define-globalized-minor-mode global-comment-tags-mode
  comment-tags-mode comment-tags-mode-turn-on)

(provide 'comment-tags-ext)

;;; comment-tags-ext ends here

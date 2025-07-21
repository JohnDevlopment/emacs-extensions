;; -*- lexical-binding: t;  -*-

(require 'comment-tags)

(eval-when-compile
  (require 'cl-ext))

;; ### Advice

(fext-defadvice comment-tags-list-tags-buffer (after list-comment-tags-buffer)
  "Enable View mode and bind `kill-and-quit' to k."
  (with-current-buffer comment-tags-temp-buffer-name
    (activate-view-mode)
    (local-set-key (kbd "k") #'kill-and-quit)))

;; ### Variables

(defgroup comment-tags-ext nil
  "Comment tags extension."
  :group 'user-extensions)

(defcustom user-ext-global-comment-tags-exclude nil
  "Modes for which `comment-tags-mode' is turned on by `global-comment-tags-mode'.
If nil, `comment-tags-mode' is not enabled for any mode.  If
t, it is enabled for every mode.  Otherwise, this is a list
of `major-mode' symbols for which `comment-tags-mode' should
be automatically turned on.  The sense of the list is
negated if it starts with `not'."
  :type '(choice (const :tag "none" nil)
		 (const :tag "all" t)
		 (set :tag "modes"
		      :value (not)
		      (const :tag "Except" not)
		      (repeat :inline t (symbol :tag "mode"))))
  :group 'comment-tags-ext)

(defvar user-ext-global-comment-tags-regexp
  (rx (+ (syntax comment-start)) (* (syntax whitespace))
      (regexp (comment-tags--make-regexp)))
  "Regular expression for matching TODO-ish comments.
This is set once the global minor mode is enabled.")

;; ### Functions

;;;###autoload
(defun comment-tags-mode-turn-on ()
  "Turn on `comment-tags-mode' if the buffer has TODO-ish comments."
  (interactive)
  (let ((modes user-ext-global-comment-tags-exclude))
    (if (eq (car modes) 'not)
	(cl-ext-progn
	  (cl-ext-unless (cl-member-if #'derived-mode-p (cdr modes))
	      (comment-tags-mode)))
      (cl-ext-when (cl-member-if #'derived-mode-p modes)
	  (comment-tags-mode)))))

;;;###autoload
(define-globalized-minor-mode global-comment-tags-mode
  comment-tags-mode comment-tags-mode-turn-on
  :group 'comment-tags-ext)

(provide 'comment-tags-ext)

;;; comment-tags-ext ends here

;; Local Variables:
;; eval: (local-lambda-define-self-insert-command fgprefix "global-comment-tags-")
;; eval: (local-lambda-define-self-insert-command vgprefix "user-ext-global-comment-tags-")
;; End:

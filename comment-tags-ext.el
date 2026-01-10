;; -*- lexical-binding: t;  -*-

(require 'comment-tags)
(require 'compat-28)

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
(defun comment-tags-ext-add-todo (tag arg)
  (interactive
   (cl-ext-progn
     (barf-if-buffer-read-only)
     (list (ido-completing-read "Tag: " comment-tags-keywords)
	   current-prefix-arg)))
  (barf-if-buffer-read-only)
  (comment-dwim arg)
  (insert (format "%s: " tag)))
(--ignore
 (prog1 nil
   (with-current-buffer (get-buffer-create "*output*")
     (emacs-lisp-mode)
     (cl-prettyprint (symbol-function 'comment-tags-ext-add-todo))
     (run-with-idle-timer 0.5 nil #'activate-view-mode 1)
     (set-buffer-modified-p nil))
   (pop-to-buffer "*output*" t))
 t)

;;;###autoload
(defun comment-tags-mode-turn-on ()
  "Turn on `comment-tags-mode' if the buffer has TODO-ish comments."
  (interactive)
  (let ((modes user-ext-global-comment-tags-exclude))
    (if (eq (car modes) 'not)
	(cl-ext-progn
	  (unless (cl-member-if #'derived-mode-p (cdr modes))
	    (with-demoted-errors "Error turning on Comment Tags: %S"
	      (comment-tags-mode 1))))
      (when (cl-member-if #'derived-mode-p modes)
	(with-demoted-errors "Error turning on Comment Tags: %S"
	  (comment-tags-mode 1))))))

(--ignore
 (fext-defadvice comment-tags--highlight-tags
     (override comment-tags--highlight-tags (limit))
   :remove t
   "Find areas marked with `comment-tags-highlight' and apply proper face within LIMIT."
   (let ((pos (point))
	 (case-fold-search (not comment-tags-case-sensitive)))
     (with-silent-modifications
       (remove-text-properties pos (or limit (point-max)) '(comment-tags-highlight))
       (cl-loop
	with chg = (re-search-forward user-ext-global-comment-tags-regexp limit t)
	while chg
	do
	(when (and chg (> chg pos)
		   (comment-tags--in-comment chg)
		   (or (not comment-tags-comment-start-only)
		       (string-match-p
			(rx bos (* (not (any alnum))) eos)
			(buffer-substring-no-properties
			 (comment-tags--comment-start chg)
			 (match-beginning 0)))))
	  (put-text-property
	   (match-beginning 0)
	   (match-end 0)
	   'comment-tags-highlight
	   (match-data)))
	(setq chg (re-search-forward user-ext-global-comment-tags-regexp limit t))
	(sit-for 0.1)
	finally return t))))

 (defun temp ()
   (interactive)
   (with-timeout (5)
     (comment-tags--highlight-tags (point-max))))
 t)

;;;###autoload
(define-globalized-minor-mode global-comment-tags-mode
  comment-tags-mode comment-tags-mode-turn-on
  :group 'comment-tags-ext)

(define-key comment-tags-command-map (kbd "c") #'comment-tags-ext-add-todo)

(provide 'comment-tags-ext)

;;; comment-tags-ext ends here

;; Local Variables:
;; eval: (abbrev-ext-install-local-abbrev-functions)
;; eval: (abbrev-ext-define-local-abbrev "ux" "user-ext-comment-tags")
;; eval: (abbrev-ext-define-local-abbrev "uxg" "user-ext-global-comment-tags")
;; eval: (abbrev-ext-define-local-abbrev "ct" "comment-tags")
;; eval: (abbrev-ext-define-local-abbrev "cx" "comment-tags-ext")
;; End:

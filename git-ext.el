;; -*- lexical-binding: t; -*-

;; ### Customization

;; (eval-when-compile
;;   (require 'alist-ext))

;; (require 'git-command)

;; ### Customization

;; (defgroup git-ext nil
;;   "Group for Git commits."
;;   :group 'user-extensions)

;; ### Variables

;; (defconst git-commit-buffer "git commit")

;; (defvar-local git-commit-arg nil "The ARG argument of `git-commit'.")

;; (defvar git--porcelain-cache nil)

;; (defvar git-current-directory nil
;;   "The directory inside which Git commands are being run.")
;; (put 'git-current-directory 'risky-local-variable-p t)

;; (defvar git-update-cache nil
;;   "Non-nil if `git-status-porcelain' should update the cache.")

;; ### Functions

;; (defun git-commit (arg)
;;   (interactive "P")
;;   (let ((buffer (tmpbuf git-commit-buffer nil)))
;;     (display-buffer buffer)
;;     (switch-to-buffer-other-window buffer)
;;     ;; Inside buffer
;;     (git-commit-mode)
;;     (setq git-commit-arg arg)))

;; (defun git-commit-finish ()
;;   "Kill text text in buffer and quit the window."
;;   (interactive)
;;   (let ((string (delete-and-extract-region
;; 		 (point-min) (point-max))))
;;     (insert (string-trim string))
;;     (kill-region (point-min) (point-max))
;;     (kill-and-quit git-commit-arg)))

;; --- Basic Git Commands

;; (defun git-add (file)
;;   "Add FILE to the index."
;;   (cl-check-type file string)
;;   (git-command (format "git add %s" file))
;;   (git-status-update-file file))

;; --- Git Porcelain Status

;; (cl-defstruct git-porcelain-status
;;   "Porcelain status for a file."
;;   (file "" :type string)
;;   (index "" :type string
;; 	 :documentation "Index status of the file.")
;;   (work-tree "" :type string
;; 	     :documentation "Work tree status of the file."))

;; (defun git-status-update-file (file)
;;   ;; TODO: Add docstring
;;   (cl-check-type file string)
;;   (git-status-generate-porcelain t file))

;; (defsubst git-porcelain-status-status (cl-x)
;;   "Get the index and work tree status.

;; Accesses slots \"index\" and \"work-tree\"."
;;   (declare (side-effect-free t))
;;   (concat (git-porcelain-status-index cl-x)
;; 	  (git-porcelain-status-work-tree cl-x)))

;; (defun git-status-generate-porcelain (&optional update file)
;;   "(Re)generate the porcelain cache.
;; If UPDATE is non-nil, for an update on the porcelain status
;; of the current directory.

;; This function uses a simple caching system: if UPDATE is
;; non-nil, or if the cache is empty or undefined, the cache is
;; regenerated."
;;   (cl-check-type file (or string null))
;;   (cl-ext-when (or update
;; 		   (not git--porcelain-cache)
;; 		   (hash-table-empty-p git--porcelain-cache))
;;       (cond ((not git--porcelain-cache)
;; 	     (setq git--porcelain-cache (make-hash-table :test #'equal)))
;; 	    ((and (not (hash-table-empty-p git--porcelain-cache))
;; 		  (not file))
;; 	     (clrhash git--porcelain-cache)))
;;     (with-temp-buffer
;;       (cl-ext-unless (= (call-process "git" nil t nil "status" "--porcelain")
;; 			0)
;; 	  (error "Failed to get porcelain status of directory"))
;;       (goto-char (point-min))
;;       (if file
;; 	  (let ((nth-char (##substring %1 %2 (1+ %2)))
;; 		xy)
;; 	    (cl-ext-when (re-search-forward
;; 			  (rx bol (group
;; 				   (or (= 2 (any " MTADCU")) "??"))
;; 			      ?\ (group (literal file)))
;; 			  nil t)
;; 		(setq xy (match-string-no-properties 1))
;; 	      (puthash file
;; 		       (make-git-porcelain-status
;; 			:file file
;; 			:index (funcall nth-char xy 0)
;; 			:work-tree (funcall nth-char xy 1))
;; 		       git--porcelain-cache)))
;; 	(cl-loop until (eobp)
;; 		 with xy
;; 		 with file
;; 		 with nth-char = (##substring %1 %2 (1+ %2))
;; 		 do
;; 		 (cl-ext-when (looking-at (rx bol (group (or (= 2 (any " MTADCU"))
;; 							     "??"))
;; 					      ?\ (group (+ nonl))))
;; 		     (setq xy (match-string-no-properties 1)
;; 			   file (match-string-no-properties 2))
;; 		   (puthash file
;; 			    (make-git-porcelain-status
;; 			     :file file
;; 			     :index (funcall nth-char xy 0)
;; 			     :work-tree (funcall nth-char xy 1))
;; 			    git--porcelain-cache))
;; 		 (forward-line 1))))))

;; (cl-defun git-status-porcelain (file &optional (update 'ignore))
;;   "Get the porcelain status of FILE.
;; If UPDATE is non-nil, update the porcelain status of the
;; current directory.

;; This returns a `git-porcelain-status' type if FILE is
;; defined, or nil otherwise.

;; This function uses a cache.  See
;; `git-status-generate-porcelain' for more information on
;; this."
;;   (cl-check-type file (or string null))
;;   (git-status--check-directory)
;;   (git-status-generate-porcelain (if (eq update 'ignore)
;; 				     git-update-cache update))
;;   (gethash file git--porcelain-cache))

;; (defun git-status--check-directory ()
;;   (pcase git-current-directory
;;     ('nil
;;      (setq git-current-directory default-directory
;; 	   git-update-cache t))
;;     ((and gcd (guard (not (equal gcd default-directory))))
;;      (setq git-current-directory default-directory
;; 	   git-update-cache t))))
;; (--ignore
;;  (cl-prettyprint (symbol-function 'git-status--check-directory))
;;  (git-status--check-directory)
;;  (git-status-generate-porcelain t)
;;  (cl-loop for file being the hash-keys of git--porcelain-cache
;; 	  using (hash-values status)
;; 	  do
;; 	  (message "%s => %S" file status))
;;  (setq git-current-directory default-directory
;;        git-update-cache t)
;;  (let ((rx (rx bol (group (or (= 2 (any " MTADCU")) "??")) ?\ (group (+ nonl)))))
;;    (string-match-p rx "?? loaddefs-ext.el"))
;;  t)

;; ### Major mode for git commits

;; (define-derived-mode git-commit-mode outline-mode "Git Commit"
;;   "Major mode for editing Git Commits."
;;   (setq header-line-format (substitute-command-keys
;; 			    (eval-when-compile
;; 			      (concat
;; 			       "Type \\[git-commit-finish] when finished, "
;; 			       "\\[kill-and-quit] to cancel editing."))))
;;   (auto-fill-mode 1)
;;   (display-fill-column-indicator-mode 1)
;;   (set-fill-column 50))

;; (define-key git-commit-mode-map (kbd "C-c C-c") #'git-commit-finish)

;; (provide 'git-ext)
;;; git-ext.el ends here

;; Local Variables:
;; eval: (local-lambda-define-self-insert-command vprefix "user-ext-git-commit-")
;; End:

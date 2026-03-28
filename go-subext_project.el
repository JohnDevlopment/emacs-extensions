;; -*- lexical-binding: t; -*-

;; ### Customization

(defgroup go-ext-project nil
  "Go projects."
  :group 'go-ext)

(defcustom user-ext-go-project-ignores nil
  "List of glob patterns to ignore in a Go project.
This has the same format as `project-vc-ignores', which see."
  :group 'go-ext-project
  :type '(repeat string)
  :safe #'list-of-strings-p)


;; ### Functions

(cl-defstruct (go-ext-project (:type list)
			      :named)
  (root default-directory :type string
	:documentation "Root directory of the project.")
  (name "" :type string
	:documentation "Name of the project."))

(defsubst go-ext-project-get-root ()
  "Return the root of the current project."
  (go-ext-project-root (go-ext-project-current)))

(defun go-ext-project-current (&optional maybe-prompt no-error)
  (let ((project (project-current maybe-prompt)))
    (if (and project (go-ext-project-p project))
	project
      (unless no-error
	(user-error "Not in Go project")))))
(--ignore
 (prog1 nil
   (with-current-buffer (get-buffer-create "*output*")
     (emacs-lisp-mode)
     (cl-prettyprint (symbol-function #'go-ext-project-current))
     (run-with-idle-timer 0.2 nil #'activate-view-mode 1)
     (set-buffer-modified-p nil))
   (pop-to-buffer "*output*" t)
   (call-interactively #'menu-bar--toggle-truncate-long-lines))
 t)

(defun go-ext-project-parent (file)
  "Return the parent of FILE within the Go project."
  (let* ((file (abbreviate-file-name file))
	 (project (go-ext-project-current t))
	 (root (project-root project))
	 (parent (f-common-parent (list root file))))
    parent))

(defun go-ext-project-rename-buffer ()
  (interactive)
  (when-let ((file (abbreviate-file-name (buffer-file-name)))
	     (parent (abbreviate-file-name (go-ext-project-parent file))))
    (rename-buffer (f-no-ext
		    (string-remove-prefix parent file)))))

(defun go-ext-try-project (dir)
  (when-let ((root (locate-dominating-file default-directory "go.mod")))
    (make-go-ext-project :root root
			 :name (f-filename root))))

(defun go-ext-project-package ()
  "Return the package this file belongs in."
  (let* ((file (buffer-file-name))
	 (path (f-split file)))
    (cl-ext-cond
      ((let ((main (f-full "main.go")))
	 (and (f-exists-p main)
	      (f-same-p (f-dirname (buffer-file-name))
			(f-dirname main))))
       ;; This file's in the same directory as main.go
       "main")
      ((go-ext-project-parent file)
       (car (last path 2))))))

(cl-defmethod project-root ((project (head go-ext-project)))
  (go-ext-project-root project))

(cl-defmethod project-ignores ((project (head go-ext-project)) dir)
  (let* ((root (go-ext-project-root project))
         (backend 'Git))
    (append
     (when (and backend (file-equal-p dir root))
       (delq
        nil
        (mapcar
         (lambda (entry)
           (cond
            ((eq ?! (aref entry 0))
             ;; No support for whitelisting (yet).
             nil)
            ((string-match "\\(/\\)[^/]" entry)
             ;; FIXME: This seems to be Git-specific.
             ;; And / in the entry (start or even the middle) means
             ;; the pattern is "rooted".  Or actually it is then
             ;; relative to its respective .gitignore (of which there
             ;; could be several), but we only support .gitignore at
             ;; the root.
             (if (= (match-beginning 0) 0)
                 (replace-match "./" t t entry 1)
               (concat "./" entry)))
            (t entry)))
         (condition-case nil
             (vc-call-backend backend 'ignore-completion-table root)
           (vc-not-supported () nil)))))
     (project--value-in-dir 'user-ext-go-project-ignores root)
     (mapcar
      (lambda (dir)
        (concat dir "/"))
      vc-directory-exclusion-list))))

(cl-defmethod project-files ((project (head go-ext-project)) &optional dirs)
  (mapcan (lambda (dir)
	    (let ((ignores (project--value-in-dir 'user-ext-go-project-ignores dir))
		  (backend 'Git))
	      (require 'vc-git)
	      (vc-call-backend backend 'project-list-files dir ignores)))
	  (or dirs (list (project-root project)))))

(add-hook 'project-find-functions #'go-ext-try-project)


;; ### Keymaps

(keymaps-ext-set-keymap go-mode-map "C-x p M-r" #'go-ext-project-rename-buffer)


(cl-pushnew 'project user-ext-go-subextensions)
;;; .el ends here

;; Local Variables:
;; eval: (abbrev-ext-install-local-abbrev-functions)
;; eval: (abbrev-ext-define-local-abbrev "ux" "user-ext-go")
;; eval: (abbrev-ext-define-local-abbrev "uxg" "user-ext-go-godoc")
;; eval: (abbrev-ext-define-local-abbrev "gx" "go-ext")
;; eval: (abbrev-ext-define-local-abbrev "gxp" "go-ext-project")
;; End:

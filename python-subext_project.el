;; -*- lexical-binding: t; -*-

(check-emacs-minimum-version "27.4")
;; TODO: check that parent extension is loaded


;; ### Customization

(defgroup python-ext-project nil
  "Python project management."
  :group 'python-ext)

(define-obsolete-variable-alias 'user-ext-python-project/ignores
  'user-ext-python/project-ignores
  "2026-01-18")
(define-obsolete-variable-alias 'user-ext-python-project-ignores
  'user-ext-python/project-ignores
  "2026-01-18")
(defcustom user-ext-python/project-ignores nil
  "List of glob patterns to ignore in a Python project.
This has the same format as `project-vc-ignores', which see."
  :group 'python-ext-project
  :type '(repeat string)
  :safe #'list-of-strings-p)


;; ### Functions

;; --- Project methods

(cl-defstruct python-ext-project
  (root default-directory :type string
	:documentation "Root directory of the project.")
  (name "" :type string
	:documentation "Name of the project."))

(cl-defmethod project-root ((project python-ext-project))
  (python-ext-project-root project))

(cl-defmethod project-ignores ((project python-ext-project) dir)
  (let* ((root (python-ext-project-root project))
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
     (project--value-in-dir 'user-ext-python-project-ignores root)
     (mapcar
      (lambda (dir)
        (concat dir "/"))
      vc-directory-exclusion-list))))

(cl-defmethod project-files ((project python-ext-project) &optional dirs)
  (mapcan
   (lambda (dir)
     (let ((ignores (project--value-in-dir 'user-ext-python-project-ignores dir))
           (backend 'Git))
       (require 'vc-git)
       (vc-call-backend backend 'project-list-files dir ignores)))
   (or dirs (list (project-root project)))))


;; --- Misc

(defun python-ext-try-project (dir)
  "Detect Python project."
  (let ((root (locate-dominating-file default-directory "pyproject.toml")))
    (when root
      (make-python-ext-project :root root
			       :name (f-filename root)))))

(defun python-ext/project-current (&optional maybe-prompt no-error)
  "Return the project instance if this is a Python project."
  (let ((project (project-current maybe-prompt)))
    (if (and project (python-ext-project-p project))
	project
      (unless no-error
	(user-error "Not in Python project")))))

(defsubst python-ext/project-get-root ()
  "Return the root of the current project."
  (python-ext-project-root (python-ext/project-current)))

(defun python-ext/project-parent (file)
  "Return the parent of FILE within the Python project."
  (let* ((root (project-root (python-ext/project-current t))))
    (f-common-parent (list root (abbreviate-file-name file)))))

(defun python-ext/project-rename-buffer ()
  "Rename the current buffer to its relative module path."
  (interactive)
  (when-let ((file (abbreviate-file-name (buffer-file-name)))
	     (parent (abbreviate-file-name (python-ext/project-parent file)))
	     (module (python-ext/project-module-path file parent)))
    (cl-ext-cond
      ((string-match "\\`\\(.+\\)\\.__\\(init\\|main\\)__" module)
       (setq module (replace-match "\\1<\\2>" nil nil module)))
      ((string-match "\\`\\.__\\(init\\|main\\).*\\'" module)
       (let ((props (save-match-data (python-ext/project--module-path file parent))))
	 (setq module (replace-match
		       (format "%s \\1" (plist-get props :package))
		       nil
		       nil
		       module)))))
    (rename-buffer module t)))

(defun python-ext/project--module-path (file root)
  "Return a property list pertaining to FILE.
ROOT is the root directory of the current project.
Return nil if FILE is not a descendant of ROOT.

Properties:
  :package - the package of this project
  :path - the module import path as a list
  :file - the name of the file with its extension"
  (if (f-descendant-of? file root)
      (let* ((modpath (f-split file))
	     (drop (length (f-split root))))
	(when (string= (nth drop modpath) "src")
	  (cl-incf drop))
	(when (= (- (length modpath) drop) 1)
	  (error "%s is not a part of a package" file))
	(cl-incf drop)			; Next index assumed to be package name
	(let* ((package (nth (1- drop) modpath))
	       (modpath (--drop-while (< it-index drop) modpath))
	       (i (1- (length modpath)))
	       (file (nth i modpath)))
	  ;; Remove extension from last item in list
	  (setf (nth i modpath) (f-no-ext file))
	  (list :package package :path modpath :file file)))))

(defun python-ext/project-module-path (file root)
  "Return the relative module path of FILE as a string.
ROOT is the root directory of the current project.
Return nil if FILE is not a descendant of ROOT."
  (when-let ((props (python-ext/project--module-path file root)))
    (concat "." (s-join "." (plist-get props :path)))))
(ert-deftest python-ext-project-test-module-path ()
  "A file inside the package -> .errors"
  (let ((parent "~/github/download-video")
	(file "~/github/download-video/download_video/errors.py"))
    (let ((result (python-ext/project-module-path file parent)))
      (should-not (null result))
      (should (equal result ".errors")))))
(ert-deftest python-ext-project-test-module-path-wrong-root ()
  "Nil result because file does not descend from parent"
  (let ((parent "~/github/emacs")
	(file "~/github/download-video/download_video/errors.py"))
    (let ((result (python-ext/project-module-path file parent)))
      (should (null result)))))
(ert-deftest python-ext-project-test-module-path-nested ()
  "Nested paths -> .site_processors.formats"
  (let ((parent "~/github/download-video")
	(file "~/github/download-video/download_video/site_processors/formats.py"))
    (let ((result (python-ext/project-module-path file parent)))
      (should-not (null result))
      (should (equal result ".site_processors.formats")))))
(ert-deftest python-ext-project-test-module-path-src ()
  "Handles src/ layouts -> .utils"
  (let ((parent "~/github/download-video")
	(file "~/github/download-video/src/download_video/utils.py"))
    (let ((result (python-ext/project-module-path file parent)))
      (should-not (null result))
      (should (equal result ".utils")))))

(add-hook 'project-find-functions #'python-ext-try-project -100)


;; ### Keymaps

(keymaps-ext-set-keymap python-mode-map "C-x p M-r" #'python-ext/project-rename-buffer)


(cl-pushnew 'project user-ext-python-subextensions)
;;; python-ext.el ends here

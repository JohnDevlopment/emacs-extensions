;;-*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'debug-ext)
(require 'faces)
(require 'function-ext)
(require 'gv)
(require 'lsp-mode)

(eval-when-compile
  (require 'cl-ext)
  (require 'dash)
  (declare-function lsp-headerline--workspace-root "lsp-headerline")
  (declare-function lsp-headerline--path-up-to-project-root "lsp-headerline")
  (declare-function lsp-headerline--build-file-string "lsp-headerline"))

(eval-and-compile
  (eval-after-require lsp-mode
    (setq lsp-keymap-prefix "C-c c l")
    (define-key lsp-mode-map (kbd "C-c l T i") #'lsp-inlay-hints-mode)
    (define-key lsp-mode-map (kbd "<double-mouse-1>") #'imenu)
    (define-key lsp-mode-map (kbd "M-R") #'revert-buffer)))

;; ### Customizations

(defgroup lsp-ext nil
  "LSP extension."
  :group 'user-extensions)

(defcustom user-ext-lsp-buffers-to-kill
  nil
  "A list of buffers to kill when `kill-lsp-buffers' is called."
  :type '(repeat regexp)
  :safe 'listp
  :group 'lsp-ext)

(defcustom user-ext-lsp-safe-inlay-modes
  nil
  "A list of major modes in which inlay hints are not disabled."
  :type '(repeat function)
  :group 'lsp-ext)

;; ### Variables

(defvar user-ext-lsp-temporary-workspace-folders nil
  "Folders added with `lsp-workspace-folders-add-temp'.")

(defconst user-ext-lsp-open-project-config-alist
  '((pyright . #1=python)
    (ruff . #1#)))

;; ### Advice

(fext-defadvice lsp-execute-code-action (after lsp-code-action)
  "Called after `lsp-execute-code-action'."
  (deactivate-mark))

(fext-defadvice lsp--before-save (after lsp-before-save)
  "Disable LSP inlays and maybe do other things before saving.
This is supposed to be called after `lsp--before-save'."
  (unless (memq major-mode user-ext-lsp-safe-inlay-modes)
    (lsp-inlay-hints-mode -1)))

(fext-defadvice lsp-disconnect (before lsp-disconnect)
  "Stuff to do before disabling LSP."
  (flycheck-mode 0)
  (company-mode 0))

;; ### Functions

(defun lsp-ext-fix-flycheck-face-errors ()
  (interactive)
  (defvar lsp-flycheck-warning-unnecessary 'lsp-flycheck-warning-unnecessary-face)
  (defvar lsp-flycheck-info-unnecessary 'lsp-flycheck-info-unnecessary-face)
  (run-with-idle-timer 1 nil
		       (lambda ()
			 (fmakunbound 'lsp-ext-fix-flycheck-face-errors)
			 (message "`lsp-ext-fix-flycheck-face-errors' is no longer valid"))))
(put 'lsp-ext-fix-flycheck-face-errors 'disabled t)

(defun lsp-ext-define-error-level (flycheck-level tags &optional force)
  (let ((name (format "lsp-flycheck-%s-%s"
                      flycheck-level
                      (mapconcat #'symbol-name tags "-"))))
    (cl-ext-when (or force (intern-soft name))
	(let* ((face (--doto (intern (format "%s-face" name))
                       (copy-face (-> flycheck-level
                                      (get 'flycheck-overlay-category)
                                      (get 'face))
				  it)
                       (mapc (lambda (tag)
                               (apply #'set-face-attribute it nil
                                      (cl-rest (assoc tag lsp-diagnostics-attributes))))
                             tags)))
               (category (--doto (intern (format "%s-category" name))
			   (setf (get it 'face) face
				 (get it 'priority) 100)))
               (new-level (intern name))
               (bitmap (or (get flycheck-level 'flycheck-fringe-bitmaps)
			   (get flycheck-level 'flycheck-fringe-bitmap-double-arrow))))
	  (flycheck-define-error-level new-level
            :severity (get flycheck-level 'flycheck-error-severity)
            :compilation-level (get flycheck-level 'flycheck-compilation-level)
            :overlay-category category
            :fringe-bitmap bitmap
            :fringe-face (get flycheck-level 'flycheck-fringe-face)
            :error-list-face face)
	  new-level))))

;;;###autoload
(defun kill-lsp-buffers ()
  "Kill all buffers that have to do with function `lsp-mode'."
  (interactive)
  (dolist (brx user-ext-lsp-buffers-to-kill)
    (kill-buffers brx))
  (kill-buffer "*lsp log*"))

;;;###autoload
(defun lsp-ext--delete-temp-workspace-folders ()
  "Remove temporary folders from the LSP workspace.

Remove the contents of `user-ext-lsp-temporary-workspace-folders'
from the workspace list.  Effectively, this removes
temporary folders from the workspace."
  (dolist (dir user-ext-lsp-temporary-workspace-folders)
    (lsp-workspace-folders-remove dir)))
(define-obsolete-function-alias 'lsp--delete-temp-workspace-folders
  'lsp-ext--delete-temp-workspace-folders "2025-06-03")

;;;###autoload
(defun lsp-ext-workspace-folders-remove-list ()
  "Call `lsp-workspace-folders-remove' one or more times in a loop."
  (interactive)
  (let (answer (flag t))
    (call-interactively 'lsp-workspace-folders-remove)
    (while flag
      (setq answer (read-char-choice "Continue? [y/n] " (list ?y ?n)))
      (if (= answer ?y)
	  (call-interactively 'lsp-workspace-folders-remove)
	(setq flag nil)))))
(define-obsolete-function-alias 'lsp-workspace-folders-remove-list
  'lsp-ext-workspace-folders-remove-list "2025-06-03")

;;;###autoload
(defun lsp-ext-workspace-blocklist-add (project-root)
  "Add PROJECT-ROOT to the workspace blocklist."
  (interactive
   (list (read-directory-name "Select folder to add: "
			      (or (lsp--suggest-project-root) default-directory) nil t)))
  (setf (lsp-session-folders-blocklist (lsp-session))
	(cons project-root (lsp-session-folders-blocklist (lsp-session))))
  (lsp--persist-session (lsp-session)))
(define-obsolete-function-alias 'lsp-workspace-blocklist-add
  'lsp-ext-workspace-blocklist-add "2025-06-03")

;;;###autoload
(defun lsp-ext-workspace-blocklist-remove-all ()
  "Remove all folders in the workspace blocklist."
  (interactive)
  (setf (lsp-session-folders-blocklist (lsp-session)) nil)
  (lsp--persist-session (lsp-session)))
(define-obsolete-function-alias 'lsp-workspace-blocklist-remove-all
  'lsp-ext-workspace-blocklist-remove-all "2025-06-03")

;;;###autoload
(defun lsp-ext-workspace-folders-add-temp (project-root)
  "Temporarily add PROJECT-ROOT to the list of workspace folders."
  (interactive
   (list (read-directory-name "Select folder to add: "
			      (or (lsp--suggest-project-root) default-directory) nil t)))
  (lsp-workspace-folders-add project-root)
  (cl-pushnew project-root user-ext-lsp-temporary-workspace-folders :test #'string=))
(define-obsolete-function-alias 'lsp-workspace-folders-add-temp
  'lsp-ext-workspace-folders-add-temp "2025-06-03")

(defun lsp-ext--project-type (servers)
  (let ((types (mapcar (lambda (server)
			 (alist-get server user-ext-lsp-open-project-config-alist))
		       servers)))
    (cl-ext-when (apply #'eq types)
	(car types))))

(defun lsp-ext--prompt-project-root ()
  (read-directory-name "Workspace folder: "
		       (or (lsp--suggest-project-root) default-directory) nil t))

;;;###autoload
(defun lsp-ext-open-project-config (project-root)
  "Open the project config file."
  (interactive (list (lsp-ext--prompt-project-root)))
  (let* ((servers (mapcar (lambda (workspace)
			    (lsp--client-server-id (lsp--workspace-client workspace)))
			  (lsp-workspaces)))
	 (type (and servers (lsp-ext--project-type servers))))
    (cl-ext-unless (and type)
	(user-error "Failed to get LSP server type. Is LSP even on?"))
    (lsp-ext--open-project-config project-root type)))

(cl-defgeneric lsp-ext--open-project-config (project-root type)
  "Open project config.")

(cl-defmethod lsp-ext--open-project-config ((project-root string) (type (eql python)))
  "Open the Python project's config file."
  (let ((file (f-join project-root "pyproject.toml")))
    (message "Open config for %s project" type)
    (if (f-exists-p file)
	(find-file file)
      (user-error "No pyproject.toml was found under %s" project-root))))

(define-face-alias 'lsp-flycheck-info-unnecessary 'lsp-flycheck-info-unnecessary-face)
(define-face-alias 'lsp-flycheck-warning-unnecessary 'lsp-flycheck-warning-unnecessary-face)

;; --- Python-specific functions

(defsubst lsp-ext-python--error-if-not-mode ()
  (cl-ext-cond
      ((not (eq major-mode 'python-mode))
       (user-error "Must be in Python mode"))))

;;;###autoload
(defun lsp-ext-python-rename-buffer ()
  "Rename the current buffer."
  (interactive)
  (lsp-ext-python--error-if-not-mode)
  (-when-let* ((root (lsp-headerline--workspace-root))
	       (segments (append
			  (lsp-headerline--path-up-to-project-root
			   root
			   (lsp-f-parent (buffer-file-name)))
			  (list (lsp-headerline--build-file-string)))))
    (let* ((module (concat
		    "."
		    (mapconcat
		     (lambda (elt)
		       (set-text-properties 0 (length elt) nil elt)
		       (s-trim-left (f-base elt)))
		     (cdr segments) "."))))
      (rename-buffer module))))

;; ### Keymaps

(define-key lsp-mode-map (kbd "<C-return>") #'lsp-find-definition)

(define-key lsp-mode-map (kbd "C-c l T I") #'lsp-ui-imenu)

;; ### Hooks

;;;###autoload
(defun lsp-ext--start-hook ()
  (let (failed)
    (message "Adding missing error levels...")
    (add-hook 'lsp-configure-hook #'lsp-ext--start-hook)
    (condition-case err
	(progn
	  (lsp-ext-define-error-level 'info '(unnecessary) t)
	  (lsp-ext-define-error-level 'warning '(unnecessary) t))
      ((debug error)
       (message "lsp-ext--start-hook: %S" err)
       (setq failed t)))
    (if failed
	(cl-ext-progn
	  (message "Adding missing error levels...failed")
	  nil)
      (remove-hook 'lsp-configure-hook #'lsp-ext--start-hook)
      (message "Adding missing error levels...done")
      t)))

;;;###autoload
(add-hook 'kill-emacs-hook #'lsp-ext--delete-temp-workspace-folders)

;;;###autoload
(add-hook 'lsp-configure-hook #'lsp-ext--start-hook)

(provide 'lsp-ext)

;;; lsp-ext ends here

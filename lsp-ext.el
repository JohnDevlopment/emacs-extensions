;;-*- lexical-binding: t; -*-

(require 'function-ext)
(require 'lsp-mode)
(require 'gv)
(require 'cl-lib)

(eval-and-compile
  (setq lsp-keymap-prefix "C-c c l")
  (define-key lsp-mode-map (kbd "C-c l T i") #'lsp-inlay-hints-mode)
  (define-key lsp-mode-map (kbd "<double-mouse-1>") #'imenu)
  (define-key lsp-mode-map (kbd "M-R") #'revert-buffer))

;; Customizations

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

;; Variables

(defvar user-ext-lsp-temporary-workspace-folders nil
  "Folders added with `lsp-workspace-folders-add-temp'.")

;; Advice

(fext-defadvice lsp-execute-code-action (after lsp-code-action)
  "Called after `lsp-execute-code-action'."
  (deactivate-mark))

(fext-defadvice lsp--before-save (after lsp-before-save)
  "Disable LSP inlays and maybe do other things before saving.
This is supposed to be called after `lsp--before-save'."
  (unless (memq major-mode user-ext-lsp-safe-inlay-modes)
    (lsp-inlay-hints-mode -1)))

;; Functions

;; (when nil
;;   (defun lsp-fix-lsp-flycheck-face-errors ()
;;     (interactive)
;;     (defface lsp-flycheck-info-unnecessary
;;       '((t (:inherit 'lsp-flycheck-info-unnecessary-face)))
;;       "Face for the sideline for symbols not used.
;; Possibly redundant of face `lsp-flycheck-info-unnecessary-face'."
;;       :group 'lsp-ui-sideline)
;;     (defface lsp-flycheck-warning-unnecessary
;;       '((t (:inherit 'lsp-flycheck-warning-unnecessary-face)))
;;       "Face for the sideline for symbols not used.
;; Possibly redundant of face `lsp-flycheck-warning-unnecessary-face'."
;;       :group 'lsp-ui-sideline))

;;   (defun lsp-flycheck-define-error-level (name face)
;;     "Define a Flycheck error level with NAME and FACE."
;;     (let ((category (intern (format "%s-category" name))))
;;       (setf (get category 'face) face
;; 	    (get category 'priority) 100)
;;       (flycheck-define-error-level name
;; 	:overlay-face face
;; 	:evaporate t
;; 	:color 'blue)))

;;   (lsp-flycheck-define-error-level )

;;   (symbol-plist 'lsp-flycheck-info-unnecessary))

;; (my-lsp-flycheck-define-error-level "lsp-flycheck-info-unnecessary" 'lsp-flycheck-info-unnecessary)

(defun kill-lsp-buffers ()
  "Kill all buffers that have to do with function `lsp-mode'."
  (interactive)
  (dolist (brx user-ext-lsp-buffers-to-kill)
    (kill-buffers brx)))

(defun lsp--delete-temp-workspace-folders ()
  "Remove temporary folders from the LSP workspace.

Remove the contents of `user-ext-lsp-temporary-workspace-folders'
from the workspace list.  Effectively, this removes
temporary folders from the workspace."
  (dolist (dir user-ext-lsp-temporary-workspace-folders)
    (lsp-workspace-folders-remove dir)))

(defun lsp-workspace-folders-remove-list ()
  "Call `lsp-workspace-folders-remove' one or more times in a loop."
  (interactive)
  (let (answer (flag t))
    (call-interactively 'lsp-workspace-folders-remove)
    (while flag
      (setq answer (read-char-choice "Continue? [y/n] " (list ?y ?n)))
      (if (= answer ?y)
	  (call-interactively 'lsp-workspace-folders-remove)
	(setq flag nil)))))

(defun lsp-workspace-blocklist-add (project-root)
  "Add PROJECT-ROOT to the workspace blocklist."
  (interactive
   (list (read-directory-name "Select folder to add: "
			      (or (lsp--suggest-project-root) default-directory) nil t)))
  (setf (lsp-session-folders-blocklist (lsp-session))
	(cons project-root (lsp-session-folders-blocklist (lsp-session))))
  (lsp--persist-session (lsp-session)))

(defun lsp-workspace-blocklist-remove-all ()
  "Remove all folders in the workspace blocklist."
  (interactive)
  (setf (lsp-session-folders-blocklist (lsp-session)) nil)
  (lsp--persist-session (lsp-session)))

(defun lsp-workspace-folders-add-temp (project-root)
  "Temporarily add PROJECT-ROOT to the list of workspace folders."
  (interactive
   (list (read-directory-name "Select folder to add: "
			      (or (lsp--suggest-project-root) default-directory) nil t)))
  (lsp-workspace-folders-add project-root)
  (cl-pushnew project-root user-ext-lsp-temporary-workspace-folders :test #'string=))

;; Keymaps

(define-key lsp-mode-map (kbd "<C-return>") #'lsp-find-definition)

;; Exit hook
(add-hook 'kill-emacs-hook #'lsp--delete-temp-workspace-folders)

;; (add-hook 'lsp-after-open-hook #'lsp-origami-try-enable)

(provide 'lsp-ext)

;;; lsp-ext ends here

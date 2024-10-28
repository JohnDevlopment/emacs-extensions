;;; lsp-ext --- Lsp Extension

;;; Commentary:

;;; Code:

(require 'lsp-mode)
(require 'lsp-origami)
(require 'lsp-pyright)

(with-eval-after-load 'lsp-mode
  (setq lsp-keymap-prefix "C-c c l"))

;; Faces

;; (defface lsp-flycheck-info-unnecessary
;;   '((t :inherit lsp-flycheck-info-unnecessary-face))
;;   "Lsp Flycheck Info Unnecessary Face."
;;   :group 'user-extensions)

;; (defface lsp-flycheck-info-unnecessary-face
;;   '((t :inherit default :foreground "Gray"
;;        :underline '(:style 'wave :color "ForestGreen")))
;;   "Lsp Flycheck Info Unnecessary Face."
;;   :group 'user-extensions)

;; (defface lsp-flycheck-info-unnecessary
;;   '((t :inherit default :foreground "Gray"
;;        :underline '(:style 'wave :color "ForestGreen")))
;;   "Lsp Flycheck Info Unnecessary Face."
;;   :group 'user-extensions)

(defvar lsp-temporary-workspace-folders nil
  "Folders added with `lsp-workspace-folders-add-temp'.")

;; Functions

(defun lsp--delete-temp-workspace-folders ()
  "Remove temporary folders from the LSP workspace.

Remove the contents of `lsp-temporary-workspace-folders'
from the workspace list.  Effectively, this removes
temporary folders from the workspace."
  (dolist (dir lsp-temporary-workspace-folders)
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

(defun lsp-workspace-folders-add-temp (project-root)
  "Temporarily add PROJECT-ROOT to the list of workspace folders."
  (interactive
   (list (read-directory-name "Select folder to add: "
			      (or (lsp--suggest-project-root) default-directory) nil t)))
  ;; TODO: check that folder is not already in `lsp-temporary-workspace-folders'
  (lsp-workspace-folders-add project-root)
  (cl-pushnew project-root lsp-temporary-workspace-folders))

;; Exit hook
(add-hook 'kill-emacs-hook #'lsp--delete-temp-workspace-folders)

(add-hook 'lsp-after-open-hook #'lsp-origami-try-enable)

(provide 'lsp-ext)

;;; lsp-ext ends here

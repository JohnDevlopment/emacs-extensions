;;; lsp-ext --- Lsp Extension  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'lsp-mode)
(require 'lsp-origami)
(require 'lsp-pyright)

(with-eval-after-load 'lsp-mode
  (setq lsp-keymap-prefix "C-c c l")
  (define-key lsp-mode-map (kbd "C-c l T i") #'lsp-inlay-hints-mode))

;; Customizations

(defgroup lsp-ext nil
  "LSP extension."
  :group 'user-extensions)

(defcustom user-ext-lsp-buffers-to-kill
  nil
  "A list of buffers to kill when `kill-lsp-buffers' is called."
  :type '(repeat string)
  :safe 'listp
  :group 'lsp-ext)

;; Variables

(defvar user-ext-lsp-temporary-workspace-folders nil
  "Folders added with `lsp-workspace-folders-add-temp'.")

;; Functions

;;;###autoload
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

;;;###autoload
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

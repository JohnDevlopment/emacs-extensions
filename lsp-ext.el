;; lsp extension

(require 'lsp-mode)

(with-eval-after-load 'lsp-mode
  (setq lsp-keymap-prefix "C-c c l"))

(defvar lsp-temporary-workspace-folders nil
  "Folders added with `lsp-workspace-folders-add-temp'.")

(defun lsp--delete-temp-workspace-folders ()
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

(let ()
  (require 'lsp-origami)
  (add-hook 'lsp-after-open-hook #'lsp-origami-try-enable))

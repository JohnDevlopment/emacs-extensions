;; -*- lexical-binding: t; -*-

(check-emacs-minimum-version "27.4")

(extension-check-requires eglot transient)

(require 'eglot nil t)
(require 'transient nil t)


;; ### Customization

(defgroup eglot-ext nil
  "Eglot extension."
  :group 'user-extensions)

(defcustom user-ext-eglot-manage-hook nil
  "A hook run during the extension hook."
  :group 'eglot-ext
  :type 'hook)

(defcustom user-ext-eglot-after-save-hook nil
  "A hook run after an Eglot-managed buffer is saved."
  :group 'eglot-ext
  :type 'hook)

(defcustom user-ext-eglot-semantic-tokens-mode t
  "Whether to enable `eglot-semantic-tokens-mode'."
  :group 'eglot-ext
  :type 'boolean)


;; ### Functions

(defun eglot-ext-kill-buffers (arg)
  "Shutdown Eglot and maybe kill all of its buffers.
If ARG is non-nil, shutdown all Eglot servers and kill all
of its buffers.  Otherwise, shutdown one of its servers."
  (interactive "P")
  (let ((rx (rx (or (seq "*Flymake" (* nonl))))))
    (if arg
	(cl-ext-progn
	  (eglot-shutdown-all)
	  (kill-buffers rx))
      (call-interactively #'eglot-shutdown))))

(defun eglot-ext-inlay-complete-function-return ()
  "Complete the function return type at point using the inlay hint."
  (interactive)
  (dolist (ov (overlays-at (point)))
    (when-let ((bs (overlay-get ov 'before-string)))
      (insert bs))))

(defun eglot-ext-find-references ()
  "Find references to SYM, the identifier at point."
  (interactive)
  (eglot--lsp-xref-helper
   :textDocument/references
   :extra-params '(:context (:includeDeclaration t))))

(defmacro eglot-ext-with-visible-range (&rest body)
  "Bind BEG and END to the visible range of window, then do BODY.
BEG is bound to the visible start of window.
END is bound to the visible end of window."
  (declare (indent defun) (debug (body)))
  `(let ((beg (max (point-min) (window-start)))
	 (end (min (point-max) (window-end))))
     ,@body))

(defun eglot-ext-update-inlay-hints ()
  "Update the inlay hints currently visible."
  (interactive)
  (eglot-ext-with-visible-range
    (eglot--update-hints beg end)))

(fext-defadvice eglot-show-workspace-configuration
    (after eglot-show-workspace-configuration)
  (let ((buffer (get-buffer "*EGLOT workspace configuration*")))
    (cl-assert buffer)
    (with-current-buffer buffer
      (activate-view-mode 1))))


;; ### Keymaps

(--ignore
  (cl-ext-progn
    (define-key eglot-mode-map (kbd "C-c l") nil)
    (define-key eglot-mode-map (kbd "<mouse-3>") nil)
    ;; Workspace map
    (define-key user-ext-eglot-workspace-map (kbd "d") nil)
    (define-key user-ext-eglot-workspace-map (kbd "q") nil)
    (define-key user-ext-eglot-workspace-map (kbd "M-q") nil)
    (define-key user-ext-eglot-workspace-map (kbd "k") nil)

    ;; Code actions map
    (define-key user-ext-eglot-code-actions-map (kbd "a") nil)
    (define-key user-ext-eglot-code-actions-map (kbd "i") nil)
    (define-key user-ext-eglot-code-actions-map (kbd "q") nil)
    (define-key user-ext-eglot-code-actions-map (kbd "n") nil)
    (define-key user-ext-eglot-code-actions-map (kbd "x") nil)
    (define-key user-ext-eglot-code-actions-map (kbd "w") nil)

    ;; Rename map
    (define-key user-ext-eglot-rename-map (kbd "r") nil)

    ;; Format map
    (define-key user-ext-eglot-format-map (kbd "r") nil)
    (define-key user-ext-eglot-format-map (kbd "=") nil)

    (define-key user-ext-eglot-command-map (kbd "w") nil)
    (define-key user-ext-eglot-command-map (kbd "a") nil)
    (define-key user-ext-eglot-command-map (kbd "r") nil)
    (define-key user-ext-eglot-command-map (kbd "=") nil)

    (setq user-ext-eglot-command-map nil)
    (setq user-ext-eglot-workspace-map nil)
    (setq user-ext-eglot-code-actions-map nil)
    (setq user-ext-eglot-rename-map nil)
    (setq user-ext-eglot-format-map nil))

  (dolist (symbol '(user-ext-eglot-code-actions-map
		    user-ext-eglot-command-map
		    user-ext-eglot-format-map
		    user-ext-eglot-rename-map
		    user-ext-eglot-workspace-map))
    (--destroy-function symbol)
    (--destroy-variable symbol))

  t)

(--ignore
  (eval-and-compile
    ;; Command map
    (define-prefix-command 'user-ext-eglot-command-map)
    (define-key eglot-mode-map (kbd "C-c l") #'user-ext-eglot-command-map)

    ;; Workspace
    (define-prefix-command 'user-ext-eglot-workspace-map)
    (define-key user-ext-eglot-command-map (kbd "w") #'user-ext-eglot-workspace-map)
    ;; (define-key user-ext-eglot-workspace-map (kbd "d") #'eglot-show-workspace-configuration)
    ;; (define-key user-ext-eglot-workspace-map (kbd "q") #'eglot-shutdown)
    ;; (define-key user-ext-eglot-workspace-map (kbd "M-q") #'eglot-shutdown-all)
    ;; (define-key user-ext-eglot-workspace-map (kbd "k") #'eglot-ext-kill-buffers)

    ;; Code actions
    ;; (define-prefix-command 'user-ext-eglot-code-actions-map)
    ;; (define-key user-ext-eglot-command-map (kbd "a") #'user-ext-eglot-code-actions-map)
    ;; (define-key user-ext-eglot-code-actions-map (kbd "a") #'eglot-code-actions)
    ;; (define-key user-ext-eglot-code-actions-map (kbd "i") #'eglot-code-action-organize-imports)
    ;; (define-key user-ext-eglot-code-actions-map (kbd "q") #'eglot-code-action-quickfix)
    ;; (define-key user-ext-eglot-code-actions-map (kbd "n") #'eglot-code-action-inline)
    ;; (define-key user-ext-eglot-code-actions-map (kbd "x") #'eglot-code-action-extract)
    ;; (define-key user-ext-eglot-code-actions-map (kbd "w") #'eglot-code-action-rewrite)

    ;; Rename
    ;; (define-prefix-command 'user-ext-eglot-rename-map)
    ;; (define-key user-ext-eglot-command-map (kbd "r") #'user-ext-eglot-rename-map)
    ;; (define-key user-ext-eglot-rename-map (kbd "r") #'eglot-rename)

    ;; Format
    (define-prefix-command 'user-ext-eglot-format-map)
    (define-key user-ext-eglot-command-map (kbd "=") #'user-ext-eglot-format-map)
    ;; (define-key user-ext-eglot-format-map (kbd "r") #'eglot-format)
    ;; (define-key user-ext-eglot-format-map (kbd "=") #'eglot-format-buffer)

    (define-key eglot-mode-map (kbd "<mouse-3>") #'imenu))
  t)

(transient-define-prefix eglot-ext-main ()
  "Run an Eglot command."
  ["Workspace"
   ("w d" "Show workspace configuration" eglot-show-workspace-configuration)
   ("w q" "Shutdown a workspace" eglot-shutdown)
   ("w M-q" "Shutdown all workspaces" eglot-shutdown-all)
   ("w k" "Kill all Eglot-related buffers" eglot-ext-kill-buffers)
   " "
   "Code Actions"
   ("a a" "Do a code action" eglot-code-actions)
   ("a i" "Organize imports" eglot-code-action-organize-imports)
   ("a q" "Do a quickfix action" eglot-code-action-quickfix)
   ("a n" "Do an inline action" eglot-code-action-inline)
   ("a x" "Do an extract action" eglot-code-action-extract)
   ("a w" "Do a rewrite action" eglot-code-action-rewrite)
   ("a c" "Complete inlay function hint" eglot-ext-inlay-complete-function-return)
   " "
   "Rename"
   ("r r" "Rename symbol at point" eglot-rename)
   " "
   "Jumping"
   ("g d" "Find declaration" eglot-find-declaration)
   ("g r" "Find references" eglot-ext-find-references)
   ("g i" "Find implementation" eglot-find-implementation)
   ("g t" "Find type definition" eglot-find-typeDefinition)
   " "
   "Feature"
   ("T i" "Toggle inlay hints" eglot-inlay-hints-mode)
   " "
   "Formatting"
   ("= r" "Format the region" eglot-format)
   ("= =" "Format the buffer" eglot-format-buffer)
   " "
   "Help"
   ("h h" "Show help for symbol at point" eldoc)
   " "
   "Misc."
   (". c" "Customize group" (lambda ()
			      (interactive)
			      (customize-group 'eglot-ext)))])

(define-key eglot-mode-map (kbd "<mouse-3>") #'imenu)

(define-key eglot-mode-map (kbd "C-c l") #'eglot-ext-main)


;; ### Hooks

(defun eglot-ext--run-after-save-hook ()
  (run-hooks 'user-ext-eglot-after-save-hook))

;;;###autoload
(defun eglot-managed--extra-hook ()
  (add-hook 'after-save-hook #'eglot-ext--run-after-save-hook nil t)
  (eglot-semantic-tokens-mode
   (if user-ext-eglot-semantic-tokens-mode 1 0))
  (eval-after-require tree-sitter-hl
    (when eglot-semantic-tokens-mode
      (tree-sitter-hl-mode 0)))
  (let ((wf (lambda (f)
	      (prog1 nil
		(with-demoted-errors "`user-ext-eglot-manage-hook' error: %S"
		  (funcall f))))))
    (run-hook-wrapped 'user-ext-eglot-manage-hook wf)))

;;;###autoload
(add-hook 'eglot-managed-mode-hook #'eglot-managed--extra-hook)


(extension-provide 'eglot-ext)
;;; eglot-ext.el ends here

;; Local Variables:
;; eval: (abbrev-ext-install-local-abbrev-functions)
;; eval: (abbrev-ext-define-local-abbrev "ex" "eglot-ext")
;; eval: (abbrev-ext-define-local-abbrev "ux" "user-ext-eglot")
;; End:

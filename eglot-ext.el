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


;; ### Variables

(defconst user-ext-eglot-help-buffer "*eglot help*" "Name of the help buffer.")


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

(defun eglot-ext-show-help-at-point ()
  "Show the documentation for symbol at point."
  (interactive)
  (eglot-server-capable-or-lose :hoverProvider)
  (let ((buf (current-buffer)))
    (eglot--async-request
     (eglot--current-server-or-lose)
     :textDocument/hover
     (eglot--TextDocumentPositionParams)
     :success-fn
     (eglot--lambda ((Hover) contents (range nil))
       (ignore range)
       (eglot-ext--display-content
	(eglot--format-markup contents)))
     :hint :textDocument/hover)))

(defun eglot-ext--display-content (content)
  (with-current-buffer (get-buffer-create user-ext-eglot-help-buffer)
    (delay-mode-hooks
      (eglot-ext-help-mode)
      (with-help-window user-ext-eglot-help-buffer
	(insert content)))
    (run-mode-hooks)))

;; TODO: Remove this function
(defun temp-show-in-temp-buffer (obj)
  (prog1 nil
    (with-current-buffer (get-buffer-create "*output*")
      (emacs-lisp-mode)
      (cl-prettyprint obj)
      (run-with-idle-timer 0.2 nil #'activate-view-mode 1)
      (set-buffer-modified-p nil))
    (pop-to-buffer "*output*" t)))


;; ### Help mode

(defvar eglot-ext-help-mode-map (make-sparse-keymap)
  "Keymap for `eglot-ext-help-mode'.")

(define-derived-mode eglot-ext-help-mode help-mode "EglotHelp"
  "Major mode for displaying Eglot extension help.")


;; ### Keymaps

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
   ("h h" "Show help for symbol at point" eglot-ext-show-help-at-point)
   " "
   "Misc."
   (". c" "Customize group" (lambda ()
			      (interactive)
			      (customize-group 'eglot-ext)))
   (". e" "Show event buffer" eglot-events-buffer)
   (". s" "Show stderr buffer" eglot-stderr-buffer)])

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

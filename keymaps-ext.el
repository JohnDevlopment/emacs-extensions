;; -*- lexical-binding: t; -*-

(require 'ido)

;; ### Customization

(defgroup keymaps-ext nil
  "Global user keymaps."
  :group 'user-extensions)

;; ### Code

(eval-after-require move-text
  (move-text-default-bindings))

;; ---Prefix Commands

(define-prefix-command 'command-map)

(define-prefix-command 'quick-mode-map)
(global-set-key (kbd "C-c m") #'quick-mode-map)

;; ---

(defun --contextual-key-func-warning (function value)
  (cl-typecase value
    (string
     (display-warning 'user-extensions
		      (format "In %S, the argument %s is invalid"
			      function value)))
    (list (display-warning 'user-extensions
			   (format "In %S, the following arguments are invalid: %s"
				   function (string-join value ", "))))))

(defmacro define-contextual-key-func (key &rest args)
  "Define a function that enables one of the modes listed in ARGS.
KEY is a string name for the key to press with the prefix
C-c C-m.

MODE is a string that is the name of a mode.  If there is
only one mode, this is expanded into a direct function
call.  Otherwise, the user is prompted to pick from a list
of modes using a completion function.

\(fn KEY ARGS...)"
  (declare (indent 1))
  (let* ((fname (intern (concat "contextual-key-" key)))
	 doc modes)
    (unless args
      (error "No args provided"))
    ;; Collect modes from ARGS; handle keyword args
    (dolist (arg args)
      (push arg modes))
    (setq modes (sort modes #'string<))	; alphabetically sort mode strings
    (if (= (length modes) 1)		; only one mode, so call directly as a function
	(let* ((mode (car modes))
	       (mode-symbol (intern-soft mode)))
	  (if (not mode-symbol)
	      (cl-ext-progn
		(--contextual-key-func-warning fname mode))
	    `(progn
	       (defun ,fname ()
		 ,(format "Call `%s'.
Likely called from C-c C-m %s." (car modes) key)
		 (interactive)
		 ;; ARGS is one mode, so call it directly
		 (call-interactively (function ,mode-symbol)))
	       (define-key quick-mode-map ,(kbd key) (function ,fname)))))
      ;; Multiple modes; use ido to select one
      (let (invalid-modes)
	(setq invalid-modes (cl-loop
			     for mode in modes
			     unless (intern-soft mode)
			     collect (cl-ext-progn
				       (setq modes (cl-delete mode modes))
				       mode)))
	(when invalid-modes
	  (--contextual-key-func-warning fname invalid-modes))
	(setq doc (format "Choose from a list of modes to enable.
Likely called from C-c C-m %s.

Included modes are:
%s" key (string-join (mapcar (lambda (string)
			       (format "- `%s'" string)) modes) "\n")))
	`(progn
	   (defun ,fname (choice)
	     ,doc
	     (interactive (list (ido-completing-read "Choose: " ',modes)))
	     (cl-assert (intern-soft choice))
	     (call-interactively (intern-soft choice)))
	   (define-key quick-mode-map (kbd ,key) (function ,fname)))))))

(define-contextual-key-func "a"
   "abbrev-mode"
   "auto-fill-mode"
   "auto-revert-mode")

(define-contextual-key-func "b"
  "basic-generic-mode"
  "basic-libreoffice-mode"
  "bind-fill-region"
  "bind-imenu"
  "bind-imenu-lsp")

(define-contextual-key-func "c"
  "code-outline-mode"
  "comment-tags-mode"
  "company-mode"
  "conf-mode")

(define-contextual-key-func "d" "display-fill-column-indicator-mode")

(define-contextual-key-func "f" "flycheck-mode")

(define-contextual-key-func "g"
  "electric-pair-mode"
  "global-comment-tags-mode"
  "global-company-mode"
  "global-flycheck-mode"
  "global-visual-line-mode"
  "yas-global-mode")

(define-contextual-key-func "h" "hs-minor-mode")

(define-contextual-key-func "l"
  "local-lambda-mode"
  "lsp-mode")

(define-contextual-key-func "o"
  "org-ext-tbl-minor-mode"
  "outline-minor-mode"
  "outline-mode")

(define-contextual-key-func "s"
  "shell-script-mode"
  "sphinx-doc-mode")

(define-contextual-key-func "j"
  "jinja2-mode"
  "jit-lock-debug-mode")

(define-contextual-key-func "m" "map-revert-buffer")

(define-contextual-key-func "v" "activate-view-mode")

(define-contextual-key-func "w" "enable-wrap")

(define-contextual-key-func "y"
  "yaml-mode"
  "yas-minor-mode")

(defun lookup-function (fn &optional keymap firstonly)
  "Lookup the key binding for FN.
If KEYMAP is non-nil, only search through that keymap.

Returns a list of keybinding descriptions unless FIRSTONLY
is non-nil."
  (let ((all-bindings
	 (where-is-internal (if (symbolp fn)
				fn
			      (cl-first fn))
			    keymap))
	keys key-bindings)
    (dolist (binding all-bindings)
      (when (and (vectorp binding)
		 (integerp (aref binding 0)))
	(push binding key-bindings)))
    (push (mapconcat #'key-description key-bindings " or ") keys)
    (if (null firstonly)
	keys
      (car keys))))

(defun map-revert-buffer ()
  "Map M-R to `revert-buffer'."
  (interactive)
  (local-set-key (kbd "M-R") #'revert-buffer)
  (message "Mapped %s `revert-buffer'"
	   (lookup-function 'revert-buffer nil t)))

;; Gloal commands
(bind-keys ("C-c d" . bookmark-delete)
	   ("C-<tab>" . tab-next)		; tab commands
	   ("C-S-<iso-lefttab>" . tab-previous) ;
	   ("C-M-k" . copy-line)
	   ("C-M-n" . nonincremental-repeat-search-backward)
	   ("M-=" . count-words-region2)
	   ("C-x C-b" . ibuffer)
	   ("C-x M-v" . view-file)	    ; find file
	   ("C-x M-f" . find-file-at-point) ;
	   ("C-x M-=" . describe-char)
	   ("C-x Q" . macro-ext-query)
	   ("C-x Z" . save-and-kill)	; killing buffers
	   ("C-c M-q" . kill-and-quit))	;

;; Prefix command for misc commands
(bind-keys :prefix-map command-map
	   :prefix "C-c c"
	   ("%" . nonincremental-re-search-forward)
	   ("M-%" . nonincremental-re-search-backward)
	   ("M-s" . nonincremental-search-backward)
	   ("c" . comment-region)
	   ("d" . copy-text-down)
	   ("p" . elisp-ext-forward-or-backward-sexp)
	   ("s" . nonincremental-search-forward)
	   ("'" . save-current-position)
	   ("&" . pop-saved-position)
	   ("^" . print-saved-positions)
	   ("P" . add-file-local-variable-prop-line)
	   ("M-P" . add-file-local-variable))

;; Extension-related commands
(bind-keys :prefix-map extension-map
	   :prefix "C-c e"
	   ("l" . load-extension)
	   ("F" . find-extension-at-point)
	   ("h" . get-extension-documentation)
	   ("f" . find-extension))

(provide 'keymaps-ext)

;;; keymaps-ext ends here

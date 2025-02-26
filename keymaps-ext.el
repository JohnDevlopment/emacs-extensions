;;; keymaps-ext --- Keymaps and keybinds.  -*- lexical-binding: t; -*-

(require 'ido)

(eval-after-require move-text
  (move-text-default-bindings))

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
  (let (modes use-ido)
    (unless args
      (error "No args provided"))
    ;; Collect modes from ARGS; handle keyword args
    (while args
      (let* ((keyword (pop args))
	     (value (pop args)))
	(cond
	 ((eq keyword :ido)
	  (unless value
	    (error "Keyword %s is missing an argument" keyword))
	  (setq use-ido value))
	 (t
	  (push keyword modes)
	  (unless (null value)
	    (push value modes))))))
    (setq modes (sort modes #'string<))	; alphabetically sort mode strings
    (if (= (length modes) 1)		; only one mode, so call directly as a function
	`(progn
	   (defun ,(intern (concat "contextual-key-" key)) ()
	     ,(format "Call `%s'.\nLikely called from C-c C-m %s."
		      (car modes) key)
	     (interactive)
	     ;; ARGS is one mode, so call it directly
	     (call-interactively #',(intern (car modes)))))
      `(progn
	 (defun ,(intern (concat "contextual-key-" key))
	     (choice)
	   ,(format "Choose from a list of modes to enable.\nLikely called from C-c C-m %s."
		    key)
	   (interactive
	    (list
	     ,(if use-ido
		  `(ido-completing-read "Choose: " ',modes)
		`(completing-read "Choose: " ',modes))))
	   (call-interactively (intern choice)))))))

(define-prefix-command 'command-map)
(define-prefix-command 'quick-mode-map)

(define-contextual-key-func "a"
  :ido t
  "abbrev-mode"
  "auto-fill-mode"
  "auto-revert-mode")

(define-contextual-key-func "b"
  "bind-imenu"
  "bind-imenu-lsp")

(define-contextual-key-func "c"
  "comment-tags-mode"
  "company-mode"
  "conf-mode")

(define-contextual-key-func "f"
  "flycheck-mode")

(define-contextual-key-func "g"
  :ido t
  "electric-pair-mode"
  "global-comment-tags-mode"
  "global-company-mode"
  "global-flycheck-mode"
  "global-visual-line-mode"
  "yas-global-mode")

(define-contextual-key-func "h"
  "hs-minor-mode")

(define-contextual-key-func "o" "org-ext-tbl-minor-mode")

(define-contextual-key-func "s"
  "shell-script-mode"
  "sphinx-doc-mode")

(define-contextual-key-func "y" "yas-minor-mode")

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

(defun remap-narrow-to-region ()
  "Assign the key mapped to `narrow-to-region' to `narrow-to-region2'."
  (interactive)
  (local-set-key [remap narrow-to-region] #'narrow-to-region2)
  (message "Remapped %s to `narrow-to-region2'"
	   (lookup-function 'narrow-to-region nil t)))

;;; Binds

;; Bind `view-file' to C-x M-v
(global-set-key (kbd "C-x M-v") #'view-file)

;; Delete bookmark
(global-set-key (kbd "C-c d") #'bookmark-delete)

;; Tab bindings
(global-set-key (kbd "C-<tab>")           #'tab-next)
(global-set-key (kbd "C-S-<iso-lefttab>") #'tab-previous)

;; Prefix command for quickly turning on different modes
(global-set-key (kbd "C-c m") #'quick-mode-map)

(define-key quick-mode-map (kbd "a") #'contextual-key-a)
(define-key quick-mode-map (kbd "b") #'contextual-key-b)
(define-key quick-mode-map (kbd "c") #'contextual-key-c)
(define-key quick-mode-map (kbd "f") #'contextual-key-f)
(define-key quick-mode-map (kbd "g") #'contextual-key-g)
(define-key quick-mode-map (kbd "h") #'contextual-key-h)
(define-key quick-mode-map (kbd "l") #'lsp-mode)
(define-key quick-mode-map (kbd "o") #'contextual-key-o)
(define-key quick-mode-map (kbd "s") #'contextual-key-s)
(define-key quick-mode-map (kbd "v") #'activate-view-mode)
(define-key quick-mode-map (kbd "w") #'enable-wrap)
(define-key quick-mode-map (kbd "y") #'contextual-key-y)

;; Prefix command for misc commands
(global-set-key (kbd "C-c c") #'command-map)

(define-key command-map (kbd "%")   #'nonincremental-re-search-forward)
(define-key command-map (kbd "M-%") #'nonincremental-re-search-backward)
(define-key command-map (kbd "M-s") #'nonincremental-search-backward)
(define-key command-map (kbd "c")   #'comment-region)
(define-key command-map (kbd "d")   #'copy-text-down)
(define-key command-map (kbd "p")   #'elisp-ext-forward-or-backward-sexp)
(define-key command-map (kbd "s")   #'nonincremental-search-forward)
(define-key command-map (kbd "'")   #'save-current-position)
(define-key command-map (kbd "&")   #'pop-saved-position)
(define-key command-map (kbd "^")   #'print-saved-positions)
(define-key command-map (kbd "M")   #'add-mode-comment)
(define-key command-map (kbd "P")   #'add-file-local-variable-prop-line)
(define-key command-map (kbd "M-P")   #'add-file-local-variable)

;; Misc commands which do not use my custom prefix
(global-set-key (kbd "C-M-k")   #'copy-line)
(global-set-key (kbd "C-M-n")   #'nonincremental-repeat-search-backward)
(global-set-key (kbd "C-c M-q") #'kill-and-quit)
(global-set-key (kbd "C-n")     #'nonincremental-repeat-search-forward)
(global-set-key (kbd "M-=")     #'count-words-region2)
(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key (kbd "C-x M-f") #'find-file-at-point)
(global-set-key (kbd "C-x Q")   #'macro-ext-query)
(global-set-key (kbd "C-x Z")   #'save-and-kill)

;; Extension-related commands
(define-prefix-command 'extension-map)
(global-set-key (kbd "C-c e") #'extension-map)

(define-key extension-map "l" #'load-extension)
(define-key extension-map "F" #'find-extension-at-point)
(define-key extension-map "h" #'get-extension-documentation)
(define-key extension-map "f" #'find-extension)

(provide 'keymaps-ext)

;;; keymaps-ext ends here
